module Language.Pig.Parser.Test
       where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Monad (liftM)
import Language.Pig.Parser

parserSuite :: Test
parserSuite = testGroup "Parser"
   [testCase "simple load" (testStmt "x = LOAD x USING y AS z;" "Right boo")]

testStmt :: String -> String -> Assertion
testStmt str expected = expected @=? (show $ parseExpr str)
