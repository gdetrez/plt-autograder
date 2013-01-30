{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.List
import Filesystem.Path
import Filesystem.Path.CurrentOS (decodeString)
import Prelude hiding (FilePath)
import qualified Data.Text.Lazy as LT

import Lab1
import Options

default (LT.Text)

testLocation = "tests/regression/lab1/" :: FilePath

-- List of test cases:
-- This is a list generated from the results of the old test suite
-- The first item in the tupple is the path to the .cf file,
-- The others are the number of passed good programs
-- and (correctly) failed bad programs, respectively.
test_cases :: [(FilePath, Int, Int, Int)]
test_cases =
  --  File name   passed good, passed bad, conflicts
  [ ( "test1.cf", 37,          136,        2)
  , ( "test2.cf", 0,           136,        1) ]

-- good programs, bad prorams, 1 for having less than 10 shift/reduce
-- conflicts and one for not haveing any reduce/reduce conflicts.
totalTests = 37 + 136 + 1 + 1

main = defaultMain tests

tests = [ testCase "sort7" (regTest (head test_cases)) ]

regTest :: (FilePath, Int, Int, Int) -> Assertion
regTest (path,good,bad,conflicts) = do
  opts <- defaultOptions
  r <- Lab1.grade opts (testLocation </> path) >>= runTestTT
  assertEqual "Wrong number of test cases" (totalTests) (cases r)
  assertEqual "Wrong number of failures"
    (totalTests - bad - good - conflicts) (failures r)
