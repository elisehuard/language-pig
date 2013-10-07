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
                               "Right (PigQuery (PigIdentifier \"users\") (PigLoadClause (PigFilename \"sorted_log/user_registration/$date/*\") (PigFunc \"LogStorage\" (PigArguments [])) (PigSchema [PigField (PigFieldName \"date\") (PigFieldType PigCharArray),PigField (PigFieldName \"time\") (PigFieldType PigCharArray),PigField (PigFieldName \"user_id\") (PigFieldType PigLong)])))")
   , testCase "foreach stmt with flatten" (testStmt "users = FOREACH users GENERATE FLATTEN(group) AS (date, herd);" 
                                                    "Right (PigQuery (PigIdentifier \"users\") (PigForeachClause \"users\" (PigTransforms [PigFlatten \"group\" (PigTuple [PigFieldName \"date\",PigFieldName \"herd\"])])))")
   , testCase "foreach stmt with expression" (testStmt "users = FOREACH users GENERATE *, ((user_id % 100) / 10) AS cohort;"
                                                       "Right (PigQuery (PigIdentifier \"users\") (PigForeachClause \"users\" (PigTransforms [PigTupleFieldGlob,PigExpressionTransform (PigBinary PigDivide (PigBinary PigModulo (PigFieldName \"user_id\") (PigNumber (Left 100))) (PigNumber (Left 10))) (PigFieldName \"cohort\")])))")]
{-
users = FOREACH users GENERATE *, (cohort <= 4 ? '04' : '59') AS herd;
report = FOREACH report GENERATE FLATTEN(group) AS (date, herd), COUNT(active_users) AS day_visits;
DESCRIBE report;
report = FOREACH report GENERATE report::date AS date, report::herd AS herd, report::day_visits AS day_visits, visits::visits AS visits;
report = FOREACH report GENERATE '$date' AS date, *;
-}

testStmt :: String -> String -> Assertion
testStmt str expected = expected @=? (show $ parseString str)
