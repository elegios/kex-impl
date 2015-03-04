{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens hiding (indices, op)
import LLVM.General.Module (withModuleFromLLVMAssembly, moduleAST, File(File))
import LLVM.General.Context (withContext)
import LLVM.General.PrettyPrint (showPretty)
import qualified LLVM.General.AST as AST
import LLVM.General.AST (Name, Named(..))
import LLVM.General.AST.Instruction (Instruction(..))
import qualified LLVM.General.AST.Constant as Constant
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.State.Lazy (runStateT, StateT)
import System.Environment (getArgs)
import Data.Functor ((<$>))
import Control.Applicative ((<*>))
import Data.List (isInfixOf, find)
import Data.Maybe (fromJust)
import Data.Number.Symbolic
import Control.Exception (evaluate, try, ErrorCall(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map as M

import Debug.Trace

type BlockPath = [Name]

data Result = Result
  { _inOrder :: [Name]
  , _outOfOrder :: [Name]
  , _unknown :: [Name] }
noResult :: Result
noResult = Result [] [] []

-- TODO: implement tracking of pointers etc inside composite data structures
data ComputationState = ComputationState
  { _intValue :: M.Map Name RelValue
  , _ptrValue :: M.Map Name (RegionKey, RelValue)
  , _lastAccess :: M.Map RegionKey RelValue
  , _availableNames :: [RelValue]
  , _availableRegions :: [RegionKey]
  , _prevBlock :: Name
  }

type RelValue = Sym Int

data Function = Function [AST.Parameter] [AST.BasicBlock]

newtype RegionKey = RegionKey Int deriving (Eq, Ord)

type BlockMonad a = StateT (Result, ComputationState) Identity a

makeLenses ''ComputationState
makeLenses ''Result

main = do
  target : _ <- getArgs
  result <- readAssembly target
  putStrLn $ showPretty result

readAssembly :: FilePath -> IO AST.Module
readAssembly path = withContext $ \c ->
  failIO $ withModuleFromLLVMAssembly c (File path) moduleAST
runBlockMonad :: ComputationState -> BlockMonad a -> (Result, ComputationState, a)
runBlockMonad initS m = case runIdentity $ runStateT m (noResult, initS) of
  (a, (r, s)) -> (r, s, a)

analyseFunction :: Function -> M.Map BlockPath Result
analyseFunction (Function params (entry : blocks)) = recurse [] initState entry
  where
    recurse path@(prev : _) _ b
      | [blockName b, prev] `isInfixOf` path = M.empty
    recurse path@(prev : _) s b = case runBlockMonad s $ analyseBlock b of
      (res, state, continuations) -> M.singleton newPath res `M.union` M.unions (recurse newPath (nextstate state) <$> nextBlocks continuations)
      where
        newPath = blockName b : path
        nextBlocks continuations = fromJust . (`M.lookup` blockMap) <$> continuations
        nextstate state = state { _prevBlock = prev }
    initState = ComputationState ints ptrs initAccess availNames availRegions undefined
    ints = M.fromList $ zip [ n | AST.Parameter (AST.IntegerType _) n _ <- params ] newNames
    ptrs = M.fromList . zip [ n | AST.Parameter (AST.PointerType _ _) n _ <- params] $ zip newRegions (M.size ints `drop` newNames)
    initAccess = M.fromList . take (M.size ptrs) . zip newRegions $ M.size ints `drop` newNames
    availNames = (M.size ints + M.size ptrs) `drop` newNames
    availRegions = M.size ptrs `drop` newRegions
    newNames = var . show <$> [(0::Int)..]
    newRegions = RegionKey <$> [0..]
    blockMap = M.fromList [ (n, b) | b@(AST.BasicBlock n _ _) <- blocks ]
    blockName (AST.BasicBlock n _ _) = n

analyseBlock :: AST.BasicBlock -> BlockMonad [Name]
analyseBlock (AST.BasicBlock _ instr term) = mapM_ analyseInstruction instr >> cont
  where
    cont = return $ case withoutName term of
      AST.CondBr _ n1 n2 _ -> [n1, n2]
      AST.Br n _ -> [n]
      AST.Switch _ n dests _ -> n : map snd dests
      AST.IndirectBr _ ns _ -> ns
      AST.Invoke{} -> error "need function call analysis"
      _ -> []

withoutName :: Named a -> a
withoutName (Do a) = a
withoutName (_ := a) = a

biOp :: (RelValue -> RelValue -> RelValue) -> Name -> AST.Operand -> AST.Operand -> BlockMonad ()
biOp f n op1 op2 = (f <$> convertOperandToRelvalue op1 <*> convertOperandToRelvalue op2)
                   >>= (_2 . intValue . at n ?=)

orderThreshold :: Int
orderThreshold = 1

analyseInstruction :: AST.Named AST.Instruction -> BlockMonad ()
analyseInstruction (n := Add _ _ op1 op2 _) = biOp (+) n op1 op2
analyseInstruction (n := Sub _ _ op1 op2 _) = biOp (-) n op1 op2
analyseInstruction (n := Mul _ _ op1 op2 _) = biOp (*) n op1 op2
analyseInstruction (n := SDiv _ op1 op2 _) = biOp quot n op1 op2
analyseInstruction (_ := UDiv{}) = error "not sure what to do about unsigned operations (udiv)"
analyseInstruction (n := SRem op1 op2 _) = biOp rem n op1 op2
analyseInstruction (_ := URem{}) = error "not sure what to do about unsigned operations (urem)"
analyseInstruction (n := And{}) = newName >>= (_2 . intValue . at n ?=)
analyseInstruction (n := Or{}) = newName >>= (_2 . intValue . at n ?=)
analyseInstruction (n := Xor{}) = newName >>= (_2 . intValue . at n ?=)
analyseInstruction (_ := Shl{}) = error "not sure what to do about shift operations (shl)"
analyseInstruction (_ := LShr{}) = error "not sure what to do about shift operations (lshr)"
analyseInstruction (_ := AShr{}) = error "not sure what to do about shift operations (ashr)"

analyseInstruction (n := Phi (AST.IntegerType{}) vals _) = do
  prev <- use $ _2 . prevBlock
  case find ((prev ==) . snd) vals of
    Nothing -> error $ "We came from " ++ show prev ++ " but that's impossible (phi int)"
    Just (AST.ConstantOperand{}, _) -> newName >>= (_2 . intValue . at n ?=)
    Just (op, _) -> convertOperandToRelvalue op >>= (_2 . intValue . at n ?=)

analyseInstruction (n := Phi (AST.PointerType{}) vals _) = do
  prev <- use $ _2 . prevBlock
  case find ((prev ==) . snd) vals of
    Nothing -> error $ "We came from " ++ show prev ++ " but that's impossible (phi pointer)"
    Just (op, _) -> (fromJust <$> convertOperandToPointer op) >>= (_2 . ptrValue . at n ?=)

analyseInstruction (_ := Phi{}) = return ()

analyseInstruction (Do (Call _ _ _retAttr _ args _ _)) = mapM_ markUnknown args
 where
   markUnknown (p, _) = convertOperandToPointer p >>= maybe (return ()) mark
   mark (k, _) = newName >>= (_2 . lastAccess . at k ?=)
analyseInstruction (n := c@Call{function = Right callop}) = do
  analyseInstruction $ Do c
  case AST.resultType $ extractType callop of
    AST.IntegerType{} -> newName >>= (_2 . intValue . at n ?=)
    AST.PointerType{} -> newPointer >>= (_2 . ptrValue . at n ?=)
    _ -> return () -- TODO: when implementing tracking of structs this should be changed

analyseInstruction (Do Store{address = ptrOp}) = analyseInstruction (undefined := Load{address = ptrOp})

analyseInstruction (n := Load{address = ptrOp}) = do
  (k, i) <- fromJust <$> convertOperandToPointer ptrOp
  lastAccessI <- fromJust <$> use (_2 . lastAccess . at k)
  case getValue $ lastAccessI - i of
    Nothing -> _1 . unknown %= (n :)
    Just diff | abs diff < orderThreshold -> _1 . inOrder %= (n :)
    Just _ -> _1 . outOfOrder %= (n :)
  _2 . lastAccess . at k ?= i

-- NOTE: this may be wrong if we do a gep on a pointer that is not the original pointer into the region
analyseInstruction (n := GetElementPtr{address = ptrOp, indices = indOps}) = do
  (k, i) <- fromJust <$> convertOperandToPointer ptrOp
  relOp <- convertOperandToRelvalue $ indOps !! indexIndex
  _2 . ptrValue . at n ?= (k, i + relOp)
  where -- NOTE: we treat pointers to arrays differently, as they are not allocated as we'd want
    indexIndex = case extractType ptrOp of
      AST.PointerType (AST.ArrayType{}) _ -> 1
      _ -> 0

analyseInstruction (n := Alloca{}) = newPointer >>= (_2 . ptrValue . at n ?=)

analyseInstruction (Do i) | shouldIgnore = return ()
  where
    shouldIgnore = case i of
      Add{} -> True; Mul{} -> True; Sub{} -> True; UDiv{} -> True
      SDiv{} -> True; URem{} -> True; SRem{} -> True; And{} -> True
      Or{} -> True; Xor{} -> True; Shl{} -> True; LShr{} -> True; AShr{} -> True

analyseInstruction i | shouldIgnore = return ()
  where
    shouldIgnore = case withoutName i of
      FAdd{} -> True; FSub{} -> True; FMul{} -> True; FDiv{} -> True; FRem{} -> True
      UIToFP{} -> True; SIToFP{} -> True; FPTrunc{} -> True; FPExt{} -> True
      ICmp{} -> True; FCmp{} -> True
      InsertElement{} -> True; InsertValue{} -> True -- TODO: when implementing struct tracking these shouldn't be ignored

analyseInstruction i = error $ "unknown instruction: " ++ show i

extractType :: AST.Operand -> AST.Type
extractType (AST.LocalReference t _) = t
extractType (AST.ConstantOperand (Constant.GlobalReference t _)) = t
extractType o = error $ "haven't implemented extractType for " ++ show o

convertOperandToRelvalue :: AST.Operand -> BlockMonad RelValue
convertOperandToRelvalue (AST.ConstantOperand c) = case c of
  Constant.Int _ v -> return . con $ fromInteger v
  _ -> error $ "Could not convert " ++ show c ++ " to RelValue"

convertOperandToRelvalue (AST.LocalReference _ n) = use (_2 . intValue . at n) >>= \mVal -> case mVal of
  Nothing -> error $ "Could not find value of " ++ show n
  Just val -> return val

convertOperandToPointer :: AST.Operand -> BlockMonad (Maybe (RegionKey, RelValue))
convertOperandToPointer (AST.LocalReference _ n) = use (_2 . ptrValue . at n)

newName :: BlockMonad RelValue
newName = head <$> (_2 . availableNames <<%= tail)

newRegion :: BlockMonad RegionKey
newRegion = do
  key <- head <$> (_2 . availableRegions <<%= tail)
  newName >>= (_2 . lastAccess . at key ?=)
  return key

newPointer :: BlockMonad (RegionKey, RelValue)
newPointer = (,) <$> newRegion <*> newName

failIO :: Show err => ExceptT err IO a -> IO a
failIO e = runExceptT e >>= \r -> case r of
  Left err -> fail $ show err
  Right a -> return a

getValue :: Show a => Sym a -> Maybe a
getValue v = case unsafeCall of
  Left (ErrorCall mess) -> trace mess Nothing
  Right a -> Just a
  where
    unsafeCall = unsafePerformIO . (try :: IO a -> IO (Either ErrorCall a)) . evaluate $ unSym v
