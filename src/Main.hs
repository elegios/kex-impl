{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import LLVM.General.Module (withModuleFromLLVMAssembly, moduleAST, File(File))
import LLVM.General.Context (withContext)
import LLVM.General.PrettyPrint (showPretty)
import qualified LLVM.General.AST as AST
import LLVM.General.AST (Name, Named(..))
import LLVM.General.AST.Instruction (Instruction(..))
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.State.Lazy (runStateT, StateT)
import System.Environment (getArgs)
import Data.Functor ((<$>))
import Data.List (isInfixOf)
import Data.Maybe (fromJust)
import qualified Data.Map as M

type BlockPath = [Name]

data Result = Result
  { _inOrder :: [Name]
  , _outOfOrder :: [Name]
  , _unknown :: [Name] }
noResult :: Result
noResult = Result [] [] []

data ComputationState = ComputationState
  { _intValue :: M.Map Name RelValue
  , _ptrValue :: M.Map Name (RegionKey, RelValue)
  , _lastAccess :: M.Map RegionKey RelValue
  , _availableNames :: [RelValue]
  , _availableRegions :: [RegionKey]
  }

data RelValue = NamedRel Int

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
    recurse path s b = case runBlockMonad s $ analyseBlock b of
      (res, state, continuations) -> M.singleton newPath res `M.union` M.unions (recurse newPath state <$> nextBlocks continuations)
      where
        newPath = blockName b : path
        nextBlocks continuations = fromJust . (`M.lookup` blockMap) <$> continuations
    initState = ComputationState ints ptrs initAccess availNames availRegions
    ints = M.fromList $ zip [ n | AST.Parameter (AST.IntegerType _) n _ <- params ] newNames
    ptrs = M.fromList . zip [ n | AST.Parameter (AST.PointerType _ _) n _ <- params] $ zip newRegions (M.size ints `drop` newNames)
    initAccess = M.fromList . take (M.size ptrs) . zip newRegions $ M.size ints `drop` newNames
    availNames = (M.size ints + M.size ptrs) `drop` newNames
    availRegions = M.size ptrs `drop` newRegions
    newNames = NamedRel <$> [0..]
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

analyseInstruction :: AST.Named AST.Instruction -> BlockMonad ()
analyseInstruction (n := Add _ _ op1 op2 _) = undefined -- TODO: implement

analyseInstruction (Do i) | shouldIgnore = return ()
  where
    shouldIgnore = case i of
      Add{} -> True; Mul{} -> True; Sub{} -> True; UDiv{} -> True
      SDiv{} -> True; URem{} -> True; SRem{} -> True

analyseInstruction i | shouldIgnore = return ()
  where
    shouldIgnore = case withoutName i of
      FAdd{} -> True; FSub{} -> True; FMul{} -> True; FDiv{} -> True; FRem{} -> True
      UIToFP{} -> True; SIToFP{} -> True; FPTrunc{} -> True; FPExt{} -> True
      ICmp{} -> True; FCmp{} -> True
      InsertElement{} -> True; InsertValue{} -> True

analyseInstruction i = error $ "unknown instruction: " ++ show i

newName :: BlockMonad RelValue
newName = head <$> (_2 . availableNames <<%= tail)

newRegion :: BlockMonad RegionKey
newRegion = do
  key <- head <$> (_2 . availableRegions <<%= tail)
  newName >>= (_2 . lastAccess . at key ?=)
  return key

failIO :: Show err => ExceptT err IO a -> IO a
failIO e = runExceptT e >>= \r -> case r of
  Left err -> fail $ show err
  Right a -> return a
