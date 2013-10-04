module Language.Pig.Parser.Test
       where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Monad (liftM)
import Language.Pig.Parser
import Language.Pig.Parser.Parser

parserSuite :: Test
parserSuite = testGroup "Parser"
   [testCase "load1" (testStmt "x = LOAD x USING LogStorage() AS z;" 
                               "Right (PigQuery (PigIdentifier \"x\") (PigLoadClause (PigFilename \"x\") (PigFunc \"LogStorage\" (PigArguments [])) (PigSchema \"z\")))")]

testStmt :: String -> String -> Assertion
testStmt str expected = expected @=? (show $ parseString str)
