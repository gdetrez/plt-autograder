module Options where

import Paths_plt_autograder 
import Filesystem.Path
import Filesystem.Path.CurrentOS (decodeString)
import Prelude hiding (FilePath)

data Lab = Lab1 | Lab2 | Lab3 | Help deriving (Eq, Show)

data Options = Options
  { getOptionTestDir :: FilePath
  , getOptionVerbose :: Bool
  , getOptionLab     :: Lab
} deriving (Eq,Show)

defaultOptions :: IO Options
defaultOptions = do
  testsDir <- getDataFileName "."  
  return $ Options (decodeString testsDir) False Help

