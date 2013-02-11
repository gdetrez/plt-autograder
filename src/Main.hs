module Main where

import Filesystem.Path.CurrentOS (decodeString)
import Prelude hiding (FilePath)
import Shelly
import System.Console.GetOpt
import System.Directory
import System.Environment   
import Test.HUnit
import Text.Printf
import Data.List (intercalate)
import System.Console.ANSI

import qualified Lab1
import qualified Lab2
import qualified Lab3
import Options

defaultFile :: IO FilePath
defaultFile = do
  cwd <- getCurrentDirectory
  return $ cwd </> "CPP.cf"

options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "v" ["verbose"]
      (NoArg (\opt -> return opt { getOptionVerbose = True }))
      "Increase verbolity"
  , Option "" ["lab1"]
      (NoArg (\opt -> return opt { getOptionLab = Lab1 }))
      "Grade lab 1"
  , Option "" ["lab2"]
      (NoArg (\opt -> return opt { getOptionLab = Lab2 }))
      "Grade lab 2"
  , Option "" ["lab3"]
      (NoArg (\opt -> return opt { getOptionLab = Lab3 }))
      "Grade lab 3"
  , Option "h" ["help"]
      (NoArg (\opt -> return opt { getOptionLab = Help }))
      "Show help" 
  , Option "d" ["debug"]
      (NoArg (\opt -> return opt { getOptionDebug = True }))
      "Debugging info" ]

header = "#####"

main :: IO ()
main = do
  args <- getArgs
  cwd <- getCurrentDirectory
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  opts <- foldl (>>=) defaultOptions actions
  when (length errors > 0) $ do
    putStr(concat errors)
    ioError (userError "Try --help.")
  file <- case nonOptions of
            [] -> defaultFile
            [f] -> return $ decodeString f
            _ -> ioError (userError "Too many files")
  case getOptionLab opts of 
    Lab1 -> Lab1.grade opts file >>= runGraderTT
--    ["lab2"] -> runGraderTT (Lab2.grade defaultFile)
--    ["lab3"] -> runGraderTT (Lab3.grade defaultFile)
    Help -> putStrLn (usageInfo header options)

usage :: IO ()
usage = putStrLn "Usage: plt-autograder ( lab1 | lab2 | lab3 ) [PATH]"

runGraderTT :: Test -> IO ()
runGraderTT tests = do
  -- counts <- runTestTT tests
  (counts,us) <- performTest reportStart reportErr reportFailure () tests
  printf "Final score: %i/%i\n" (successes counts) (cases counts)
  where successes r = cases r - errors r - failures r
        reportStart state _ = return ()
        reportErr msg state _ = do
          setSGR [SetColor Foreground Dull Red]
          putStrLn (getName state)
          putStrLn msg
          setSGR [Reset]
        reportFailure msg state _ = do
          putStrLn $ (getName state) ++ " (error)"
          setSGR [SetColor Foreground Dull Yellow]
          putStrLn msg
          setSGR [Reset]
        dummyReport str state us = return ()
        getName s = intercalate " > "
          (reverse [ x | Label x <- Test.HUnit.path s])

