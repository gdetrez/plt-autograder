module Main where

import Test.HUnit
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import qualified Data.Text.Lazy as LT

import qualified Lab1

main = defaultMain tests

tests =
  [ testGroup "Lab1"
      [ testGroup "grepForConflicts"
          [ testCase "No conflicts" grepForConflictsTest1
          , testCase "Only shift/reduce conflicts" grepForConflictsTest2
          , testCase "Both kind of conflicts" grepForConflictsTest3 ]]]

grepForConflictsTest1 =
  (0,0) @=? Lab1.grepForConflicts (LT.pack "unused terminals: 1\n")

grepForConflictsTest2 =
  (4,0) @=? Lab1.grepForConflicts (LT.pack "unused terminals: 1\nshift/reduce conflicts:  4")

grepForConflictsTest3 =
  (316,19) @=? Lab1.grepForConflicts (LT.pack "unused terminals: 1\nshift/reduce conflicts:  316\nreduce/reduce conflicts: 19\n")

