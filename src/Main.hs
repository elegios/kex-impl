module Main where

import LLVM.General.Module (withModuleFromLLVMAssembly, moduleAST, File(File))
import LLVM.General.Context (withContext)
import LLVM.General.PrettyPrint (showPretty)
import qualified LLVM.General.AST as AST
import Control.Monad.Except (runExceptT, ExceptT)
import System.Environment (getArgs)
import Data.Functor ((<$>))
import qualified Data.Map as M

main = do
  target : _ <- getArgs
  result <- readAssembly target
  putStrLn $ showPretty result

readAssembly :: FilePath -> IO AST.Module
readAssembly path = withContext $ \c ->
  failIO $ withModuleFromLLVMAssembly c (File path) moduleAST

data BlockPath

data Counts = Counts
  { inOrder :: Int
  , outOfOrder :: Int
  , unknown :: Int}

analyse :: AST.Module -> M.Map BlockPath Counts
analyse = undefined

failIO :: Show err => ExceptT err IO a -> IO a
failIO e = runExceptT e >>= \r -> case r of
  Left err -> fail $ show err
  Right a -> return a
