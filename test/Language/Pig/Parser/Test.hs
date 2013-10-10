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
   [testCase "load statement 1" (testStmt "users = LOAD 'sorted_log/user_registration/$date/*' USING LogStorage() AS (date:chararray, time:chararray, user_id:long);" 
                               "Right (PigQuery (PigIdentifier \"users\") (PigLoadClause (PigFilename \"sorted_log/user_registration/$date/*\") (PigFunc \"LogStorage\" (PigArguments [])) (PigSchema [PigField (PigFieldName \"date\") (PigFieldType PigCharArray),PigField (PigFieldName \"time\") (PigFieldType PigCharArray),PigField (PigFieldName \"user_id\") (PigFieldType PigLong)])))")

   , testCase "load statement 2" (testStmt "active_users = LOAD 'warehouse/active_users/daily/point/{$visit_dates}*' USING ColumnStorage(' ') AS (date:chararray, user_id:long);"
                                                                  "Right (PigQuery (PigIdentifier \"active_users\") (PigLoadClause (PigFilename \"warehouse/active_users/daily/point/{$visit_dates}*\") (PigFunc \"ColumnStorage\" (PigArguments [PigString \" \"])) (PigSchema [PigField (PigFieldName \"date\") (PigFieldType PigCharArray),PigField (PigFieldName \"user_id\") (PigFieldType PigLong)])))")

   , testCase "foreach stmt with flatten" (testStmt "users = FOREACH users GENERATE FLATTEN(group) AS (date, herd);" 
                                                    "Right (PigQuery (PigIdentifier \"users\") (PigForeachClause (PigIdentifier \"users\") (PigTransforms [PigFlatten \"group\" (PigTuple [PigFieldName \"date\",PigFieldName \"herd\"])])))")

   , testCase "foreach stmt with expression" (testStmt "users = FOREACH users GENERATE *, ((user_id % 100) / 10) AS cohort;"
                                                       "Right (PigQuery (PigIdentifier \"users\") (PigForeachClause (PigIdentifier \"users\") (PigTransforms [PigTupleFieldGlob,PigExpressionTransform (PigBinary PigDivide (PigBinary PigModulo (PigFieldName \"user_id\") (PigNumber (Left 100))) (PigNumber (Left 10))) (PigFieldName \"cohort\")])))")

   , testCase "foreach stmt with ternary if-then-else" (testStmt "users = FOREACH users GENERATE *, (cohort <= 4 ? '04' : '59') AS herd;"
                                                                 "Right (PigQuery (PigIdentifier \"users\") (PigForeachClause (PigIdentifier \"users\") (PigTransforms [PigTupleFieldGlob,PigExpressionTransform (PigBinCond (PigBinary PigLessEqual (PigFieldName \"cohort\") (PigNumber (Left 4))) (PigString \"04\") (PigString \"59\")) (PigFieldName \"herd\")])))")
   , testCase "foreach stmt with flatten and function" (testStmt "report = FOREACH report GENERATE FLATTEN(group) AS (date, herd), COUNT(active_users) AS day_visits;"
                                                                 "Right (PigQuery (PigIdentifier \"report\") (PigForeachClause (PigIdentifier \"report\") (PigTransforms [PigFlatten \"group\" (PigTuple [PigFieldName \"date\",PigFieldName \"herd\"]),PigExpressionTransform (PigFunc \"COUNT\" (PigArguments [PigFieldName \"active_users\"])) (PigFieldName \"day_visits\")])))")
   , testCase "foreach stmt with qualified field names" (testStmt "report = FOREACH report GENERATE report::date AS date, report::herd AS herd, report::day_visits AS day_visits, visits::visits AS visits;"
                                                                  "Right (PigQuery (PigIdentifier \"report\") (PigForeachClause (PigIdentifier \"report\") (PigTransforms [PigExpressionTransform (PigIdentifier \"report::date\") (PigFieldName \"date\"),PigExpressionTransform (PigIdentifier \"report::herd\") (PigFieldName \"herd\"),PigExpressionTransform (PigIdentifier \"report::day_visits\") (PigFieldName \"day_visits\"),PigExpressionTransform (PigIdentifier \"visits::visits\") (PigFieldName \"visits\")])))")
   , testCase "foreach stmt with quoted string" (testStmt "report = FOREACH report GENERATE '$date' AS date, *;"
                                                                  "Right (PigQuery (PigIdentifier \"report\") (PigForeachClause (PigIdentifier \"report\") (PigTransforms [PigExpressionTransform (PigString \"$date\") (PigFieldName \"date\"),PigTupleFieldGlob])))")

   , testCase "join stmt" (testStmt "active_users = JOIN users BY user_id, active_users BY user_id;"
                                    "Right (PigQuery (PigIdentifier \"active_users\") (PigInnerJoinClause [PigJoin \"users\" \"user_id\",PigJoin \"active_users\" \"user_id\"]))")

   , testCase "group stmt by one field" (testStmt "visits = GROUP active_users BY herd;"
                                     "Right (PigQuery (PigIdentifier \"visits\") (PigGroupClause (PigIdentifier \"active_users\") (PigFieldName \"herd\")))")

   , testCase "group stmt by several fields" (testStmt "report = GROUP active_users BY (date, herd);"
                                                       "Right (PigQuery (PigIdentifier \"report\") (PigGroupClause (PigIdentifier \"active_users\") (PigTuple [PigFieldName \"date\",PigFieldName \"herd\"])))")

   , testCase "describe stmt" (testStmt "DESCRIBE visits;"
                                        "Right (PigDescribe (PigIdentifier \"visits\"))")

   , testCase "define stmt" (testStmt "define RESOLVE `python delta.py $date` SHIP('delta.py');"
                                      "Right (PigDefineUDF (PigIdentifier \"RESOLVE\") (PigExec \"python delta.py $date\") (PigShip (PigPath \"delta.py\")))")

   , testCase "stream stmt" (testStmt "report = STREAM report THROUGH RESOLVE AS (day:chararray, herd:chararray, day_visits:int, visits:int);"
                                      "Right (PigQuery (PigIdentifier \"report\") (PigStreamClause (PigIdentifier \"report\") (PigIdentifier \"RESOLVE\") (PigSchema [PigField (PigFieldName \"day\") (PigFieldType PigCharArray),PigField (PigFieldName \"herd\") (PigFieldType PigCharArray),PigField (PigFieldName \"day_visits\") (PigFieldType PigInt),PigField (PigFieldName \"visits\") (PigFieldType PigInt)])))")

   , testCase "store stmt" (testStmt "STORE report INTO '$output' USING ColumnStorage(',');"
                                     "Right (PigStore (PigIdentifier \"report\") (PigDirectory \"$output\") (PigFunc \"ColumnStorage\" (PigArguments [PigString \",\"])))")

   , testCase "several statements" (testStmt "active_users = LOAD 'warehouse/active_users/daily/point/{$visit_dates}*' USING ColumnStorage(' ') AS (date:chararray, user_id:long);\nactive_users = JOIN users BY user_id, active_users BY user_id;" 
                                             "Right (PigSeq [PigQuery (PigIdentifier \"active_users\") (PigLoadClause (PigFilename \"warehouse/active_users/daily/point/{$visit_dates}*\") (PigFunc \"ColumnStorage\" (PigArguments [PigString \" \"])) (PigSchema [PigField (PigFieldName \"date\") (PigFieldType PigCharArray),PigField (PigFieldName \"user_id\") (PigFieldType PigLong)])),PigQuery (PigIdentifier \"active_users\") (PigInnerJoinClause [PigJoin \"users\" \"user_id\",PigJoin \"active_users\" \"user_id\"])])") 
   ]

testStmt :: String -> String -> Assertion
testStmt str expected = expected @=? (show $ parseString str)
