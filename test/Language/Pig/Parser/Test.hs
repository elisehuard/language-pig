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
   [testCase "load1" (testStmt "users = LOAD 'sorted_log/user_registration/$date/*' USING LogStorage() AS (date:chararray, time:chararray, user_id:long);" 
                               "Right (PigQuery (PigIdentifier \"users\") (PigLoadClause (PigFilename \"sorted_log/user_registration/$date/*\") (PigFunc \"LogStorage\" (PigArguments [])) (PigSchema [PigField (PigFieldName \"date\") (PigFieldType PigCharArray),PigField (PigFieldName \"time\") (PigFieldType PigCharArray),PigField (PigFieldName \"user_id\") (PigFieldType PigLong)])))")]

testStmt :: String -> String -> Assertion
testStmt str expected = expected @=? (show $ parseString str)
