{-# LANGUAGE TemplateHaskell, TupleSections #-}

module Main where

import Control.Lens hiding (indices, op)
import Control.Applicative ((<*>))
import Control.Monad.Except (runExceptT, ExceptT)
import Control.Monad.State.Lazy (runStateT, StateT)
import System.Environment (getArgs)
import Data.Functor ((<$>))
import Data.List (find, groupBy, isPrefixOf)
import Data.Maybe (fromJust, isJust)
import Data.Function (on)
import Data.Sequence (ViewL(..), (><))
import LLVM.General.Module (withModuleFromLLVMAssembly, moduleAST, File(File))
import LLVM.General.Context (withContext)
import LLVM.General.AST (Name, Named(..))
import LLVM.General.AST.Instruction (Instruction(..))
import Text.Printf
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Map as M
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as G

import Debug.Trace

import RelValue

type Signature = (Name, M.Map Name Int)

data SourceLoc = SourceLoc Int Int FilePath deriving (Eq, Ord)

instance Show SourceLoc where
  show (SourceLoc l c f) = f ++ " " ++ show l ++ ":" ++ show c

data Result = Result
  { _inOrder :: [SourceLoc]
  , _outOfOrder :: [SourceLoc]
  , _unknown :: [SourceLoc] } deriving (Eq, Ord)
noResult :: Result
noResult = Result [] [] []

instance Show Result where
  show (Result i o u) = "inorder: " ++ show i ++ "\noutoforder: " ++ show o ++ "\nunknown: " ++ show u

-- TODO: implement tracking of pointers etc inside composite data structures
data ComputationState = ComputationState
  { _numberedMetadata :: NumberedMetadata
  , _intValue :: M.Map Name RelValue
  , _ptrValue :: M.Map Name (RegionKey, RelValue)
  , _lastAccess :: M.Map RegionKey RelValue
  , _availableNames :: [RelValue]
  , _availableRegions :: [RegionKey]
  , _phiAlts :: M.Map Name Int -- TODO: implement this kind of pruning. span is a useful function. need to preserve previous phiAlts checked for the current block and make one for each
  , _prevBlock :: Name
  }

type NumberedMetadata = M.Map AST.MetadataNodeID [Maybe AST.Operand]
type NamedMetadata = M.Map String [AST.MetadataNodeID]

data Function = Function [AST.Parameter] [AST.BasicBlock]

newtype RegionKey = RegionKey Int deriving (Eq, Ord)

type BlockMonad a = StateT (Result, ComputationState) Identity a

makeLenses ''ComputationState
makeLenses ''Result

simpleAnalysisString :: M.Map Signature Result -> String
simpleAnalysisString = fancyShow . M.unionsWith tupOr . map convert . M.elems
  where
    fancyShow :: M.Map SourceLoc [Bool] -> String
    fancyShow m = unlines $ zipWith (++) padded sProps
      where
        padded = printf ("%-" ++ show (maximum (length <$> sLocs) + 1) ++ "s") <$> sLocs
        sLocs = show <$> locs
        sProps = map (boolean 'x' '-') <$> props
        (locs, props) = unzip $ M.toList m
    convert :: Result -> M.Map SourceLoc [Bool]
    convert (Result i o u) = M.fromListWith tupOr $
      map (,[t,f,f]) i ++
      map (,[f,t,f]) o ++
      map (,[f,f,t]) u
    tupOr = zipWith (||)
    t = True
    f = False

main :: IO (M.Map Name (M.Map Signature Result))
main = do
  target : _ <- getArgs
  parsed <- AST.moduleDefinitions <$> readAssembly target
  let numbered = M.fromList [ (i, c) | AST.MetadataNodeDefinition i c <- parsed ]
      funcs = [ (n, Function ps bs) | (AST.GlobalDefinition AST.Function{G.parameters = (ps, _), G.name = n, G.basicBlocks = bs}) <- parsed ]
  res <- M.fromList <$> mapM (analyse numbered) funcs
  mapM_ (\(n, m) -> print n >> putStrLn (simpleAnalysisString m)) $ M.toList res
  return res
  where
    analyse numbered (name, f) = print name >> print (M.size res) >> prettyPrint >> return (name, res)
      where
        prettyPrint = mapM_ (\(n, r) -> putStr $ show n ++ ":\n" ++ show r ++ "\n\n") $ M.toList res
        res = simplifySignature . M.filter nonEmpty . analyseFunction numbered $ f
        nonEmpty (Result l1 l2 l3) = not $ all null [l1, l2, l3]

simplifySignature :: M.Map Signature Result -> M.Map Signature Result
simplifySignature m = trace ("Removed " ++ show (M.size m - M.size result) ++ " signatures") result
  where
    result = M.unions $ zipWith attachName blocks simplified
    attachName n = M.mapKeysMonotonic (n, )
    simplified = M.map fromJust <$> zipWith (foldl tryRemove) alts phis
    tryRemove :: M.Map (M.Map Name Int) (Maybe Result) -> Name -> M.Map (M.Map Name Int) (Maybe Result)
    tryRemove original phi = if M.fold ((&&) . isJust) True removed
                             then removed
                             else original
      where
        removed = M.mapKeysWith combine (M.delete phi) original
        combine a b
          | a == b = a
          | otherwise = Nothing
    phis = S.toList . S.unions . map M.keysSet . M.keys <$> alts
    alts = M.fromList . map ((_1 %~ snd) . (_2 %~ Just)) <$> partitions
    blocks = fst . fst . head <$> partitions
    partitions = groupBy ((==) `on` fst . fst) $ M.toAscList m

readAssembly :: FilePath -> IO AST.Module
readAssembly path = withContext $ \c ->
  failIO $ withModuleFromLLVMAssembly c (File path) moduleAST
runBlockMonad :: ComputationState -> BlockMonad a -> (Result, ComputationState, a)
runBlockMonad initS m = case runIdentity $ runStateT m (noResult, initS) of
  (a, (r, s)) -> (r, s, a)

analyseFunction :: NumberedMetadata -> Function -> M.Map Signature Result
analyseFunction num (Function params (entry : blocks)) = recurse (Seq.singleton (entry, initState)) S.empty
  where
    recurse q _ | Seq.null q = M.empty
    recurse q done = case Seq.viewl q of
      EmptyL -> M.empty
      (b, s) :< rest -> case runBlockMonad s $ analyseBlock done b of
        (res, state, continuations) -> let
          nextState = state { _prevBlock = blockName b }
          nextQ = (rest ><) . Seq.fromList $ ((, nextState) . getBlock) <$> continuations
          nextDone = S.insert signature done
          signature = (blockName b, _phiAlts state)
          in M.singleton signature res `M.union` recurse nextQ nextDone
    getBlock n = fromJust $ M.lookup n blockMap
    blockMap = M.fromList [ (n, b) | b@(AST.BasicBlock n _ _) <- blocks ]
    blockName (AST.BasicBlock n _ _) = n
    initState = ComputationState num ints ptrs initAccess availNames availRegions M.empty undefined
    initAccess = M.fromList . take (M.size ptrs) . zip newRegions $ M.size ptrs `drop` newNames
    availNames = (2 * M.size ptrs) `drop` newNames
    availRegions = M.size ptrs `drop` newRegions
    ints = M.fromList [ (n, Sym n) | AST.Parameter (AST.IntegerType _) n _ <- params ]
    ptrs = M.fromList . zip [ n | AST.Parameter (AST.PointerType _ _) n _ <- params] $ zip newRegions newNames
    newNames = Uniq <$> [0..]
    newRegions = RegionKey <$> [0..]
analyseFunction _ _ = M.empty

analyseBlock :: S.Set Signature -> AST.BasicBlock -> BlockMonad [Name]
analyseBlock done (AST.BasicBlock name instr term) = do
  mapM_ analyseInstruction phis
  phiConf <- use $ _2 . phiAlts
  if (name, phiConf) `S.member` done
     then return []
     else mapM_ analyseInstruction rest >> cont
  where
    (phis, rest) = span isPhi instr
    isPhi (Do Phi{}) = True
    isPhi (_ := Phi{}) = True
    isPhi _ = False
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
                   >>= setInt n

orderThreshold :: Int
orderThreshold = 1

deathAt :: Name -> String -> a
deathAt n s = error $ show n ++ ": " ++ s

analyseInstruction :: AST.Named AST.Instruction -> BlockMonad ()
analyseInstruction (n := Add _ _ op1 op2 _) = biOp (+) n op1 op2
analyseInstruction (n := Sub _ _ op1 op2 _) = biOp (-) n op1 op2
analyseInstruction (n := Mul _ _ op1 op2 _) = biOp (*) n op1 op2
analyseInstruction (n := SDiv{}) = deathAt n "(sdiv)"
analyseInstruction (n := UDiv{}) = deathAt n "(udiv)"
analyseInstruction (n := SRem{}) = deathAt n "(sdiv)"
analyseInstruction (n := URem{}) = deathAt n "(urem)"
analyseInstruction (n := And{}) = newName n >>= setInt n
analyseInstruction (n := Or{}) = newName n >>= setInt n
analyseInstruction (n := Xor{}) = newName n >>= setInt n
analyseInstruction (n := Shl _ _ op1 _ _) = convertOperandToRelvalue op1 >>= setInt n -- TODO: these are obviously not correct, but they work for a certain common case. Should detect that case and/or handle shifting correctly
analyseInstruction (n := LShr _ op1 _ _) = convertOperandToRelvalue op1 >>= setInt n
analyseInstruction (n := AShr _ op1 _ _) = convertOperandToRelvalue op1 >>= setInt n
analyseInstruction (n := Trunc{}) = newName n >>= setInt n
analyseInstruction (n := SExt op1 _ _) = convertOperandToRelvalue op1 >>= setInt n

analyseInstruction (n := Phi (AST.IntegerType{}) vals _) = do
  prev <- use $ _2 . prevBlock
  case find ((prev ==) . snd . snd) $ zip [0..] vals of
    Nothing -> error $ "We came from " ++ show prev ++ " but that's impossible (phi int)"
    Just (i, (AST.ConstantOperand{}, _)) -> newName n >>= setInt n >> (_2 . phiAlts . at n ?= i)
    Just (i, (op, _)) -> convertOperandToRelvalue op >>= setInt n >> (_2 . phiAlts . at n ?= i)

analyseInstruction (n := Phi (AST.PointerType{}) vals _) = do
  prev <- use $ _2 . prevBlock
  case find ((prev ==) . snd . snd) $ zip [0..] vals of
    Nothing -> error $ "We came from " ++ show prev ++ " but that's impossible (phi pointer)"
    Just (i, (op, _)) -> (fromJust <$> convertOperandToPointer op) >>= setPointer n >> (_2 . phiAlts . at n ?= i)

analyseInstruction (_ := Phi{}) = return ()

analyseInstruction (n := BitCast op (AST.PointerType targetT _) _) =
  case extractType op of
    AST.PointerType origT _ -> do
      (r, i) <- fromJust <$> convertOperandToPointer op
      _2 . ptrValue . at n ?= (r, i * fromIntegral (getSize origT `div` getSize targetT))
    _ -> newPointer n >>= setPointer n
  where
    getSize (AST.IntegerType s) = s

analyseInstruction (n := BitCast op (AST.IntegerType{}) _)
  | opIsInteger = convertOperandToRelvalue op >>= setInt n
  | otherwise = newName n >>= setInt n
  where
    opIsInteger = case extractType op of
      AST.IntegerType{} -> True
      _ -> False

analyseInstruction (Do Call{function = Right (AST.ConstantOperand (Constant.GlobalReference _ (AST.Name n))), arguments = args})
  | "llvm.dbg" `isPrefixOf` n = return ()
  | otherwise = mapM_ markUnknown args
  where
    markUnknown (p, _) = convertOperandToPointer p >>= maybe (return ()) mark
    mark (k, _) = newUniq >>= (_2 . lastAccess . at k ?=)
analyseInstruction (n := c@Call{function = Right callop}) = do
  analyseInstruction $ Do c
  case getReturnType $ extractType callop of
    AST.IntegerType{} -> newName n >>= setInt n
    AST.PointerType{} -> newPointer n >>= setPointer n
    _ -> return () -- TODO: for implementing struct tracking
  where
    getReturnType AST.FunctionType{AST.resultType = t} = t
    getReturnType (AST.PointerType AST.FunctionType{AST.resultType = t} _) = t

analyseInstruction (Do Store{address = ptrOp, metadata = md}) =
  getLoc md >>= analyseMemoryAccess ptrOp

analyseInstruction (n := Load{address = ptrOp, metadata = md}) = do
  getLoc md >>= analyseMemoryAccess ptrOp
  case AST.pointerReferent $ extractType ptrOp of
    AST.IntegerType{} -> newName n >>= setInt n
    AST.PointerType{} -> newPointer n >>= setPointer n

-- NOTE: this may be wrong if we do a gep on a pointer that is not the original pointer into the region
analyseInstruction (n := GetElementPtr{address = ptrOp, indices = indOps}) = do
  (k, i) <- fromJust <$> convertOperandToPointer ptrOp
  relOp <- convertOperandToRelvalue $ indOps !! indexIndex
  _2 . ptrValue . at n ?= (k, i + relOp)
  where -- NOTE: we treat pointers to arrays differently, as they are not allocated as we'd want
    indexIndex = case extractType ptrOp of
      AST.PointerType (AST.ArrayType{}) _ -> 1
      _ -> 0

analyseInstruction (n := Alloca{}) = newPointer n >>= setPointer n

analyseInstruction (Do i) | shouldIgnore = return ()
  where
    shouldIgnore = case i of
      Add{} -> True; Mul{} -> True; Sub{} -> True; UDiv{} -> True
      SDiv{} -> True; URem{} -> True; SRem{} -> True; And{} -> True
      Or{} -> True; Xor{} -> True; Shl{} -> True; LShr{} -> True; AShr{} -> True
      _ -> False

analyseInstruction i | shouldIgnore = return ()
  where
    shouldIgnore = case withoutName i of
      FAdd{} -> True; FSub{} -> True; FMul{} -> True; FDiv{} -> True; FRem{} -> True
      UIToFP{} -> True; SIToFP{} -> True; FPTrunc{} -> True; FPExt{} -> True
      ICmp{} -> True; FCmp{} -> True
      InsertElement{} -> True; InsertValue{} -> True -- TODO: when implementing struct tracking these shouldn't be ignored
      _ -> False

analyseInstruction i = error $ "unknown instruction: " ++ show i

getLoc :: [(String, AST.MetadataNode)] -> BlockMonad SourceLoc
getLoc md = case lookup "dbg" md of
  Nothing -> error $ "Couldn't find dbg in " ++ show md
  Just (AST.MetadataNode l) -> inner l
  Just (AST.MetadataNodeReference i) -> fromJust <$> use (_2 . numberedMetadata . at i) >>= inner
  where
    inner :: [Maybe AST.Operand] -> BlockMonad SourceLoc
    inner (l : c : Just scope : _) = SourceLoc (getVal l) (getVal c) <$> case scope of
      AST.MetadataNodeOperand (AST.MetadataNodeReference r) -> getStr . head <$>
        (readRef r >>= readRef . getRef . (!! 1))
    getVal (Just (AST.ConstantOperand (Constant.Int _ v))) = fromInteger v
    getStr (Just (AST.MetadataStringOperand s)) = s
    getRef (Just (AST.MetadataNodeOperand (AST.MetadataNodeReference r))) = r
    readRef r = fromJust <$> use (_2 . numberedMetadata . at r)

analyseMemoryAccess :: AST.Operand -> SourceLoc -> BlockMonad ()
analyseMemoryAccess ptrOp loc = do
  (k, i) <- fromJust <$> convertOperandToPointer ptrOp
  lastAccessI <- fromJust <$> use (_2 . lastAccess . at k)
  case fromRelValue $ i - lastAccessI of
    Nothing ->
      trace ("U   "++show loc++": "++show (i - lastAccessI)) $ _1 . unknown %= (loc :)
    Just diff | abs diff <= orderThreshold ->
      trace ("IO  " ++ show loc ++ ": " ++ show diff) $ _1 . inOrder %= (loc :)
    Just diff ->
      trace ("OOO " ++ show loc ++ ": " ++ show diff) $ _1 . outOfOrder %= (loc :)
  _2 . lastAccess . at k ?= i

extractType :: AST.Operand -> AST.Type
extractType (AST.LocalReference t _) = t
extractType (AST.ConstantOperand (Constant.GlobalReference t _)) = t
extractType o = error $ "haven't implemented extractType for " ++ show o

convertOperandToRelvalue :: AST.Operand -> BlockMonad RelValue
convertOperandToRelvalue (AST.ConstantOperand c) = case c of
  Constant.Int _ v -> return $ fromInteger v
  _ -> error $ "Could not convert " ++ show c ++ " to RelValue"

convertOperandToRelvalue (AST.LocalReference _ n) = use (_2 . intValue . at n) >>= \mVal -> case mVal of
  Nothing -> error $ "Could not find value of " ++ show n
  Just val -> return val

convertOperandToPointer :: AST.Operand -> BlockMonad (Maybe (RegionKey, RelValue))
convertOperandToPointer (AST.LocalReference _ n) = use (_2 . ptrValue . at n)
convertOperandToPointer _ = return Nothing

newName :: Name -> BlockMonad RelValue
-- newName = head <$> (_2 . availableNames <<%= tail)
newName = return . Sym

newUniq :: BlockMonad RelValue
newUniq = head <$> (_2 . availableNames <<%= tail)

newRegion :: BlockMonad RegionKey
newRegion = do
  key <- head <$> (_2 . availableRegions <<%= tail)
  newUniq >>= (_2 . lastAccess . at key ?=)
  return key

newPointer :: Name -> BlockMonad (RegionKey, RelValue)
newPointer n = (,) <$> newRegion <*> newName n

setPointer :: Name -> (RegionKey, RelValue) -> BlockMonad ()
setPointer n = (_2 . ptrValue . at n ?=)

setInt :: Name -> RelValue -> BlockMonad ()
setInt n = (_2 . intValue . at n ?=)

failIO :: Show err => ExceptT err IO a -> IO a
failIO e = runExceptT e >>= \r -> case r of
  Left err -> fail $ show err
  Right a -> return a

boolean :: a -> a -> Bool -> a
boolean a _ True = a
boolean _ a False = a
