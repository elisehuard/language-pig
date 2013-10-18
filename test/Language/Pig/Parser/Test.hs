module Language.Pig.Parser.Test
       where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Monad (liftM)
import Language.Pig.Parser
import Language.Pig.Parser.Parser
import Language.Pig.Parser.AST

parserSuite :: Test
parserSuite = testGroup "Parser"
   [testCase "load statement 1" (testStmt "users = LOAD 'sorted_log/user_registration/$date/*' USING LogStorage() AS (date:chararray, time:chararray, user_id:long);" 
                                          "Seq [Assignment (Identifier \"users\") (LoadClause (Filename \"sorted_log/user_registration/$date/*\") (Function \"LogStorage\" []) (TupleDef [Field (Identifier \"date\") PigCharArray,Field (Identifier \"time\") PigCharArray,Field (Identifier \"user_id\") PigLong]))]")

   , testCase "load statement 2" (testStmt "active_users = LOAD 'warehouse/active_users/daily/point/{$visit_dates}*' USING ColumnStorage(' ') AS (date:chararray, user_id:long);"
                                           "Seq [Assignment (Identifier \"active_users\") (LoadClause (Filename \"warehouse/active_users/daily/point/{$visit_dates}*\") (Function \"ColumnStorage\" [StringArgument (PigString \" \")]) (TupleDef [Field (Identifier \"date\") PigCharArray,Field (Identifier \"user_id\") PigLong]))]")

   , testCase "foreach stmt with flatten" (testStmt "users = FOREACH users GENERATE FLATTEN(group) AS (date, herd);" 
                                                    "Seq [Assignment (Identifier \"users\") (ForeachClause (Identifier \"users\") (GenBlock [Flatten \"group\" (Tuple [Identifier \"date\",Identifier \"herd\"])]))]")

   , testCase "foreach stmt with expression" (testStmt "users = FOREACH users GENERATE *, ((user_id % 100) / 10) AS cohort;"
                                                       "Seq [Assignment (Identifier \"users\") (ForeachClause (Identifier \"users\") (GenBlock [TupleFieldGlob,ExpressionTransform (Binary Divide (Binary Modulo (AliasTerm (Identifier \"user_id\")) (ScalarTerm (PigNumber (Left 100)))) (ScalarTerm (PigNumber (Left 10)))) (Identifier \"cohort\")]))]")

   , testCase "foreach stmt with ternary if-then-else" (testStmt "users = FOREACH users GENERATE *, (cohort <= 4 ? '04' : '59') AS herd;"
                                                                 "Seq [Assignment (Identifier \"users\") (ForeachClause (Identifier \"users\") (GenBlock [TupleFieldGlob,ExpressionTransform (BinCond (Binary LessEqual (AliasTerm (Identifier \"cohort\")) (ScalarTerm (PigNumber (Left 4)))) (ScalarTerm (PigString \"04\")) (ScalarTerm (PigString \"59\"))) (Identifier \"herd\")]))]")
   , testCase "foreach stmt with flatten and function" (testStmt "report = FOREACH report GENERATE FLATTEN(group) AS (date, herd), COUNT(active_users) AS day_visits;"
                                                                 "Seq [Assignment (Identifier \"report\") (ForeachClause (Identifier \"report\") (GenBlock [Flatten \"group\" (Tuple [Identifier \"date\",Identifier \"herd\"]),FunctionTransform (Function \"COUNT\" [AliasArgument (Identifier \"active_users\")]) (Identifier \"day_visits\")]))]")
   , testCase "foreach stmt with qualified field names" (testStmt "report = FOREACH report GENERATE report::date AS date, report::herd AS herd, report::day_visits AS day_visits, visits::visits AS visits;"
                                                                  "Seq [Assignment (Identifier \"report\") (ForeachClause (Identifier \"report\") (GenBlock [AliasTransform (Identifier \"report::date\") (Identifier \"date\"),AliasTransform (Identifier \"report::herd\") (Identifier \"herd\"),AliasTransform (Identifier \"report::day_visits\") (Identifier \"day_visits\"),AliasTransform (Identifier \"visits::visits\") (Identifier \"visits\")]))]")
   , testCase "foreach stmt with quoted string" (testStmt "report = FOREACH report GENERATE '$date' AS date, *;"
                                                          "Seq [Assignment (Identifier \"report\") (ForeachClause (Identifier \"report\") (GenBlock [EnvTransform (PigString \"$date\") (Identifier \"date\"),TupleFieldGlob]))]")

   , testCase "join stmt" (testStmt "active_users = JOIN users BY user_id, active_users BY user_id;"
                                    "Seq [Assignment (Identifier \"active_users\") (InnerJoinClause [Join \"users\" \"user_id\",Join \"active_users\" \"user_id\"])]")

   , testCase "group stmt by one field" (testStmt "visits = GROUP active_users BY herd;"
                                                  "Seq [Assignment (Identifier \"visits\") (GroupClause (Identifier \"active_users\") (SingleColumn (Identifier \"herd\")))]")

   , testCase "group stmt by several fields" (testStmt "report = GROUP active_users BY (date, herd);"
                                                       "Seq [Assignment (Identifier \"report\") (GroupClause (Identifier \"active_users\") (MultipleColumn (Tuple [Identifier \"date\",Identifier \"herd\"])))]")

   , testCase "describe stmt" (testStmt "DESCRIBE visits;"
                                        "Seq [Describe (Identifier \"visits\")]")

   , testCase "define stmt" (testStmt "define RESOLVE `python delta.py $date` SHIP('delta.py');"
                                      "Seq [DefineUDF (Identifier \"RESOLVE\") (Exec \"python delta.py $date\") (Ship (Filename \"delta.py\"))]")

   , testCase "stream stmt" (testStmt "report = STREAM report THROUGH RESOLVE AS (day:chararray, herd:chararray, day_visits:int, visits:int);"
                                      "Seq [Assignment (Identifier \"report\") (StreamClause (Identifier \"report\") (Identifier \"RESOLVE\") (TupleDef [Field (Identifier \"day\") PigCharArray,Field (Identifier \"herd\") PigCharArray,Field (Identifier \"day_visits\") PigInt,Field (Identifier \"visits\") PigInt]))]")

   , testCase "store stmt" (testStmt "STORE report INTO '$output' USING ColumnStorage(',');"
                                     "Seq [Store (Identifier \"report\") (Directory \"$output\") (Function \"ColumnStorage\" [StringArgument (PigString \",\")])]")

   , testCase "several statements" (testStmt "active_users = LOAD 'warehouse/active_users/daily/point/{$visit_dates}*' USING ColumnStorage(' ') AS (date:chararray, user_id:long);\nactive_users = JOIN users BY user_id, active_users BY user_id;" 
                                             "Seq [Assignment (Identifier \"active_users\") (LoadClause (Filename \"warehouse/active_users/daily/point/{$visit_dates}*\") (Function \"ColumnStorage\" [StringArgument (PigString \" \")]) (TupleDef [Field (Identifier \"date\") PigCharArray,Field (Identifier \"user_id\") PigLong])),Assignment (Identifier \"active_users\") (InnerJoinClause [Join \"users\" \"user_id\",Join \"active_users\" \"user_id\"])]")
   , testCase "case insensitivity of keywords" (testStmt "store report into '$output' using ColumnStorage(',');"
                                                         "Seq [Store (Identifier \"report\") (Directory \"$output\") (Function \"ColumnStorage\" [StringArgument (PigString \",\")])]")
   ]

testStmt :: String -> String -> Assertion
testStmt str expected = expected @=? (show $ parseString str)
