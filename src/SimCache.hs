{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Functor ((<$>))
import Data.Maybe (fromJust)
import System.FilePath (replaceExtension)
import Control.Lens hiding (op)
import Control.Applicative ((<*>))
import Control.Monad.State.Lazy (runStateT, StateT)
import Control.Monad.Except (runExceptT, ExceptT)
import System.Environment (getArgs)
import LLVM.General.PrettyPrint (showPretty)
import LLVM.General.Analysis (verify)
import LLVM.General.PassManager (withPassManager, defaultCuratedPassSetSpec, optLevel, runPassManager)
import LLVM.General.Target (withDefaultTargetMachine)
import LLVM.General.Context (withContext)
import LLVM.General.Module (withModuleFromLLVMAssembly, moduleAST, File(File))
import LLVM.General.AST.Instruction (Named(..), Instruction(..))
import LLVM.General.AST.Attribute (ParameterAttribute)
import LLVM.General.AST.AddrSpace (AddrSpace(..))
import qualified Data.Map as M
import qualified LLVM.General.AST as AST
import qualified LLVM.General.Module as M
import qualified LLVM.General.AST.Global as G
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Type as T
import qualified LLVM.General.AST.CallingConvention as CallingConvention

data SourceLoc = SourceLoc Int Int FilePath deriving (Eq, Ord)

type NumberedMetadata = M.Map AST.MetadataNodeID [Maybe AST.Operand]
data ComputationState = ComputationState
  { _globalCounters :: M.Map SourceLoc AST.Operand
  , _introducedGlobals :: [AST.Definition]
  , _fresh :: Int
  , _numberedMetadata :: NumberedMetadata
  }

makeLenses ''ComputationState

state :: NumberedMetadata -> ComputationState
state = ComputationState M.empty [] 0

type BlockMonad a = StateT ComputationState Identity a

main :: IO ()
main = do
  cacheSource : target : _ <- getArgs
  cacheDefs <- AST.moduleDefinitions <$> readAssembly cacheSource
  parsed <- readAssembly target
  let (inj, st) = runBlockMonad (state md) . mapM inject $ AST.moduleDefinitions parsed
      newDefs = _introducedGlobals st ++ cacheDefs ++ (injectPrinting (_globalCounters st) <$> inj)
      altered = parsed { AST.moduleDefinitions = newDefs }
      md = M.fromList [ (i, ops) | AST.MetadataNodeDefinition i ops <- AST.moduleDefinitions parsed ]
  asGeneralModule altered (\m -> do
    verifyResult <- runExceptT $ verify m
    case verifyResult of
      Left mess -> putStrLn $ "Verify error: " ++ mess
      Right _ -> do
        withPassManager (defaultCuratedPassSetSpec {optLevel = Just 3}) $ \pm ->
          runPassManager pm m
        -- writeObjectFile (replaceExtension target "o") m
        printModule m
    )

injectPrinting :: M.Map SourceLoc AST.Operand -> AST.Definition -> AST.Definition
injectPrinting locs = inner
  where
    inner (AST.GlobalDefinition f@G.Function{G.basicBlocks = bs, G.name = AST.Name "main"}) = AST.GlobalDefinition $ f {G.basicBlocks = map attachPrinting bs}
    inner d = d
    attachPrinting (AST.BasicBlock n i r@(Do AST.Ret{})) = AST.BasicBlock n (i ++ printIs) r
    attachPrinting b = b
    printIs = printI <$> M.toList locs
    printI (SourceLoc l c _, op) = Do $ Call False CallingConvention.C [] func [(cInt l, []), (cInt c, []), (op, [])] [] []
    cInt = AST.ConstantOperand . C.Int 64 . toInteger
    func = Right . AST.ConstantOperand $ C.GlobalReference t (AST.Name "__printSimCacheData")
    t = T.FunctionType T.VoidType [T.i64, T.i64, T.PointerType counterType (AddrSpace 0)] False

inject :: AST.Definition -> BlockMonad AST.Definition
inject (AST.GlobalDefinition f@G.Function{G.basicBlocks = blocks}) = do
  newBlocks <- mapM iBlock blocks
  return . AST.GlobalDefinition $ f {G.basicBlocks = newBlocks}
  where
    iBlock (AST.BasicBlock n is t) = AST.BasicBlock n <$> recurse is <*> return t
    recurse [] = return []
    recurse (i:is) = case unName i of
      Load{address = ptrOp, metadata = md} -> access ptrOp md $ (i:) <$> recurse is
      Store{address = ptrOp, metadata = md} -> access ptrOp md $ (i:) <$> recurse is
      _ -> (i:) <$> recurse is
    unName (Do i) = i
    unName (_ := i) = i

inject d = return d

access :: AST.Operand -> [(String, AST.MetadataNode)] -> BlockMonad [Named Instruction] -> BlockMonad [Named Instruction]
access op md cont = do
  counterOp <- getLoc md >>= getCounter
  (ptrOp, castInstr) <- cast
  callInstr <- callCache [(ptrOp, []), (counterOp, [])]
  (castInstr :) . (callInstr :) <$> cont
  where
    cast = do
      nInt <- fresh <<+= 1
      let name = AST.Name $ "bitcastthing" ++ show nInt
      return (AST.LocalReference t name, name := BitCast op t [])
    t = T.PointerType T.i8 (AddrSpace 0)

getCounter :: SourceLoc -> BlockMonad AST.Operand
getCounter l = use (globalCounters . at l) >>= \mo -> case mo of
  Just o -> return o
  Nothing -> do
    nInt <- fresh <<+= 1
    let g = G.globalVariableDefaults {G.name = name, G.type' = counterType, G.initializer = Just $ C.Struct Nothing False [C.Int 64 0, C.Int 64 0]}
        name = AST.Name $ "globForLoc" ++ show nInt
        op = AST.ConstantOperand $ C.GlobalReference (T.PointerType counterType (AddrSpace 0)) name
    introducedGlobals %= (AST.GlobalDefinition g:)
    globalCounters . at l ?= op
    return op

callCache :: [(AST.Operand, [ParameterAttribute])] -> BlockMonad (Named Instruction)
callCache params = return . Do $ Call False CallingConvention.Fast [] func params [] []
  where
    func = Right . AST.ConstantOperand $ C.GlobalReference t (AST.Name "__memory_blub")
    t = T.FunctionType T.VoidType [T.PointerType T.i8 (AddrSpace 0), T.PointerType counterType (AddrSpace 0)] False

counterType :: T.Type
counterType = T.StructureType False [T.i64, T.i64]

getLoc :: [(String, AST.MetadataNode)] -> BlockMonad SourceLoc
getLoc md = case lookup "dbg" md of
  Nothing -> error $ "Couldn't find dbg in " ++ show md
  Just (AST.MetadataNode l) -> inner l
  Just (AST.MetadataNodeReference i) -> fromJust <$> use (numberedMetadata . at i) >>= inner
  where
    inner :: [Maybe AST.Operand] -> BlockMonad SourceLoc
    inner (l : c : Just scope : _) = SourceLoc (getVal l) (getVal c) <$> case scope of
      AST.MetadataNodeOperand (AST.MetadataNodeReference r) -> getStr . head <$>
        (readRef r >>= readRef . getRef . (!! 1))
    getVal (Just (AST.ConstantOperand (C.Int _ v))) = fromInteger v
    getStr (Just (AST.MetadataStringOperand s)) = s
    getRef (Just (AST.MetadataNodeOperand (AST.MetadataNodeReference r))) = r
    readRef r = fromJust <$> use (numberedMetadata . at r)

runBlockMonad :: ComputationState -> BlockMonad a -> (a, ComputationState)
runBlockMonad st m = runIdentity $ runStateT m st

readAssembly :: FilePath -> IO AST.Module
readAssembly path = withContext $ \c ->
  failIO $ withModuleFromLLVMAssembly c (File path) moduleAST

failIO :: Show err => ExceptT err IO a -> IO a
failIO e = runExceptT e >>= \r -> case r of
  Left err -> fail $ show err
  Right a -> return a

boolean :: a -> a -> Bool -> a
boolean a _ True = a
boolean _ a False = a

writeObjectFile :: FilePath -> M.Module -> IO ()
writeObjectFile path m = failIO . withDefaultTargetMachine $ \mac -> failIO $ M.writeObjectToFile mac (M.File path) m

asGeneralModule :: AST.Module -> (M.Module -> IO a) -> IO a
asGeneralModule m monad = do
  eRes <- withContext $ \context ->
    runExceptT . M.withModuleFromAST context m $ monad
  case eRes of
    Left mess -> fail mess
    Right res -> return res

printModule :: M.Module -> IO ()
printModule m = M.moduleLLVMAssembly m >>= putStrLn
