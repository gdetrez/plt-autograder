{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Lab1 where

import Shelly
import Prelude hiding (FilePath)
import qualified Data.Text.Lazy as LT
import Filesystem.Path hiding ((</>),(<.>))
import Filesystem.Path.CurrentOS (encodeString)
import Text.Printf
import Test.HUnit
import Options
import Utils
default (LT.Text)

grade :: Options -> FilePath -> IO Test
grade opts filepath = shelly' $ do
  -- Test setup:
  -- in a temporary directory, we copy the cf file
  -- and then call bnfc then make.
  -- Note that with a recent verion of bnfc, it is possible to
  -- use the generated program as a test driver returning 0 for 
  -- a successful parse and 1 for an error.
  absFilepath <-  absPath filepath
  withTmpDir $ \tmpdir -> do
    -- Copy the cf file in the temp dir
    dbg ("Grading lab 1, file: " ++ show filepath)
    cp absFilepath tmpdir
    dbg ("Working in tmp dir: " ++ show tmpdir)
    cd tmpdir
    let cfFile = filename absFilepath
    -- run bnfc and then make
    bnfc "-haskell" "-m" cfFile
    make
    stderr <- lastStderr
    let (shiftreduce,reducereduce) = grepForConflicts stderr
    dbg ("Found conflicts (resp. s/r and r/r) " ++ show shiftreduce
        ++ " " ++ show reducereduce)
    let tests = getOptionTestDir opts </> "lab1"
    goodFiles <- findCCFiles (tests </> "good")
    badFiles <- findCCFiles (tests </> "bad")
    testCf <- absPath $ testProgram cfFile
    good <- flip mapM goodFiles (\file -> do
      a <- testFile opts testCf True file
      return (encodeString (filename file) ~: a))
    bad <- flip mapM badFiles (\file -> do
      a <- testFile opts testCf False file
      return (encodeString (filename file) ~: a))
    return $ "Tests for lab 1" ~:
      [ "Good programs" ~: good
      , "Bad programs"  ~: bad 
      , "Max. 10 shift/reduce conflicts" ~: 
          assertBool "More than 10 shift/reduce conflicts" (shiftreduce <= 10)
      , "No reduce/reduce conflicts" ~:
          assertBool "This grammar has reduce/reduce conflicts" (reducereduce == 0)]
  where bnfc = cmd "bnfc"
        make = cmd "make"
        shelly' = shelly . print_stdout v . print_commands v
        v = getOptionVerbose opts
        dbg s = if (getOptionDebug opts) then debug s else return ()

findCCFiles :: FilePath -> Sh [FilePath]
findCCFiles = findWhen (return . flip hasExtension "cc")

-- This function test try to parse a given file with the 
-- test program and return the result (success or failure)
-- in the form of an assertion, taking into account the type
-- of input file (good or bad)
testFile :: Options -> FilePath -> Bool -> FilePath -> Sh Assertion
testFile opts testCf good path = errExit False $ do
  output <- cmd testCf path
  status <- lastExitCode
  errors <- lastStderr
  return ((good && status == 0 || not good && status /= 0 ) @? (msg status output errors))
  where msg status out err = unlines
          [ if good then printf "Failed good program: %s" name
                    else printf "Passed bad program: %s" name
          , printf "status: %d" status
          , "----- stdout -----"
          , LT.unpack out
          , "----- stderr -----"
          , LT.unpack err
          , "------------------" ]
        name = (encodeString $ filename path)

-- | Given the .cf file path, this function return the path of the 
-- test program created by bnfc.
testProgram :: FilePath -> FilePath
testProgram f = dirname f </> name
  where name = "Test" ++ encodeString (basename f)


-- This function parses the output of happy and tries to find reports
-- of conflicts. It returns a pair with shift/reduce conflicts
-- first and reduce/reduce conflict second.
-- (it should work on the full output of the Makefile as well)
grepForConflicts :: LT.Text -> (Int,Int)
grepForConflicts = foldl f (0,0) . LT.lines
  where f (sr,ss) l
          | LT.isPrefixOf "shift/reduce conflicts:" l = (value,ss)
          | LT.isPrefixOf "reduce/reduce conflicts:" l = (sr, value)
          | otherwise = (sr,ss)
          where value = read (LT.unpack (last (LT.words l)))

          
