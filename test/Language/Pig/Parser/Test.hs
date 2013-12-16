module Language.Pig.Parser.Test
       where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Monad (liftM, guard)
import Control.Exception
import Language.Pig.Parser
import Language.Pig.Parser.Parser
import Language.Pig.Parser.AST

parserSuite :: Test
parserSuite = testGroup "Parser"
   [testCase "load statement 1" (testStmt "users = LOAD 'sorted_log/user_registration/$date/*' USING LogStorage() AS (date:chararray, time:chararray, user_id:long);" 
                                          "Seq [Assignment (Identifier \"users\") (LoadClause (Filename \"sorted_log/user_registration/$date/*\") (Function \"LogStorage\" []) (TupleDef [Field (Identifier \"date\") CharArray,Field (Identifier \"time\") CharArray,Field (Identifier \"user_id\") Long]))]")

   , testCase "load statement 2" (testStmt "active_users = LOAD 'warehouse/active_users/daily/point/{$visit_dates}*' USING ColumnStorage(' ') AS (date:chararray, user_id:long);"
                                           "Seq [Assignment (Identifier \"active_users\") (LoadClause (Filename \"warehouse/active_users/daily/point/{$visit_dates}*\") (Function \"ColumnStorage\" [ScalarTerm (String \" \")]) (TupleDef [Field (Identifier \"date\") CharArray,Field (Identifier \"user_id\") Long]))]")

   , testCase "foreach stmt with flatten" (testStmt "users = FOREACH users GENERATE FLATTEN(group) AS (date, herd);" 
                                                    "Seq [Assignment (Identifier \"users\") (ForeachClause (Identifier \"users\") (GenBlock [Flatten \"group\" (Tuple [Identifier \"date\",Identifier \"herd\"])]))]")

   , testCase "foreach stmt with expression" (testStmt "users = FOREACH users GENERATE *, ((user_id % 100) / 10) AS cohort;"
                                                       "Seq [Assignment (Identifier \"users\") (ForeachClause (Identifier \"users\") (GenBlock [TupleFieldGlob,ExpressionTransform (Binary Divide (Binary Modulo (AliasTerm (Identifier \"user_id\")) (ScalarTerm (Number (Left 100)))) (ScalarTerm (Number (Left 10)))) (Identifier \"cohort\")]))]")

   , testCase "foreach stmt with ternary if-then-else" (testStmt "users = FOREACH users GENERATE *, (cohort <= 4 ? '04' : '59') AS herd;"
                                                                 "Seq [Assignment (Identifier \"users\") (ForeachClause (Identifier \"users\") (GenBlock [TupleFieldGlob,ExpressionTransform (BinCond (BooleanExpression LessEqual (AliasTerm (Identifier \"cohort\")) (ScalarTerm (Number (Left 4)))) (ScalarTerm (String \"04\")) (ScalarTerm (String \"59\"))) (Identifier \"herd\")]))]")
   , testCase "foreach stmt with field as is" (testStmt "desktop_client_dates2 = FOREACH desktop_client_dates GENERATE '$date' AS date, (datediff < 0 OR datediff > 30 ? server_date : device_date) AS device_date, user_id;"
                                                        "Seq [Assignment (Identifier \"desktop_client_dates2\") (ForeachClause (Identifier \"desktop_client_dates\") (GenBlock [EnvTransform (String \"$date\") (Identifier \"date\"),ExpressionTransform (BinCond (BooleanBinary Or (BooleanExpression Less (AliasTerm (Identifier \"datediff\")) (ScalarTerm (Number (Left 0)))) (BooleanExpression Greater (AliasTerm (Identifier \"datediff\")) (ScalarTerm (Number (Left 30))))) (AliasTerm (Identifier \"server_date\")) (AliasTerm (Identifier \"device_date\"))) (Identifier \"device_date\"),IdentityTransform (Identifier \"user_id\")]))]")

   , testCase "foreach stmt with ternary if-then-else complex" (testStmt "desktop_client_dates = FOREACH desktop_client GENERATE server_date AS server_date, device_date AS device_date, user_id AS user_id, (server_date != '0' AND device_date != '0' ? (ISOToUnix(CustomFormatToISO(server_date,'YYYY-MM-dd'))-ISOToUnix(CustomFormatToISO(device_date,'YYYY-MM-dd')))/86400000 : 0) AS datediff;"
                                                                          "Seq [Assignment (Identifier \"desktop_client_dates\") (ForeachClause (Identifier \"desktop_client\") (GenBlock [AliasTransform (Identifier \"server_date\") (Identifier \"server_date\"),AliasTransform (Identifier \"device_date\") (Identifier \"device_date\"),AliasTransform (Identifier \"user_id\") (Identifier \"user_id\"),ExpressionTransform (BinCond (BooleanBinary And (BooleanExpression NotEqual (AliasTerm (Identifier \"server_date\")) (ScalarTerm (String \"0\"))) (BooleanExpression NotEqual (AliasTerm (Identifier \"device_date\")) (ScalarTerm (String \"0\")))) (Binary Divide (Binary Subtract (FunctionTerm (Function \"ISOToUnix\" [FunctionTerm (Function \"CustomFormatToISO\" [AliasTerm (Identifier \"server_date\"),ScalarTerm (String \"YYYY-MM-dd\")])])) (FunctionTerm (Function \"ISOToUnix\" [FunctionTerm (Function \"CustomFormatToISO\" [AliasTerm (Identifier \"device_date\"),ScalarTerm (String \"YYYY-MM-dd\")])]))) (ScalarTerm (Number (Left 86400000)))) (ScalarTerm (Number (Left 0)))) (Identifier \"datediff\")]))]")
{-
desktop_client_dates = FOREACH desktop_client GENERATE server_date AS server_date, device_date AS device_date, user_id AS user_id, (server_date != '0' AND device_date != '0' ? (ISOToUnix(CustomFormatToISO(server_date,'YYYY-MM-dd'))-ISOToUnix(CustomFormatToISO(device_date,'YYYY-MM-dd')))/86400000 : 0) AS datediff;
-}
   , testCase "foreach stmt with flatten and function" (testStmt "report = FOREACH report GENERATE FLATTEN(group) AS (date, herd), COUNT(active_users) AS day_visits;"
                                                                 "Seq [Assignment (Identifier \"report\") (ForeachClause (Identifier \"report\") (GenBlock [Flatten \"group\" (Tuple [Identifier \"date\",Identifier \"herd\"]),FunctionTransform (Function \"COUNT\" [AliasTerm (Identifier \"active_users\")]) (Identifier \"day_visits\")]))]")
   , testCase "foreach stmt with qualified field names" (testStmt "report = FOREACH report GENERATE report::date AS date, report::herd AS herd, report::day_visits AS day_visits, visits::visits AS visits;"
                                                                  "Seq [Assignment (Identifier \"report\") (ForeachClause (Identifier \"report\") (GenBlock [AliasTransform (Identifier \"report::date\") (Identifier \"date\"),AliasTransform (Identifier \"report::herd\") (Identifier \"herd\"),AliasTransform (Identifier \"report::day_visits\") (Identifier \"day_visits\"),AliasTransform (Identifier \"visits::visits\") (Identifier \"visits\")]))]")
   , testCase "foreach stmt with quoted string" (testStmt "report = FOREACH report GENERATE '$date' AS date, *;"
                                                          "Seq [Assignment (Identifier \"report\") (ForeachClause (Identifier \"report\") (GenBlock [EnvTransform (String \"$date\") (Identifier \"date\"),TupleFieldGlob]))]")

   , testCase "join stmt" (testStmt "active_users = JOIN users BY user_id, active_users BY user_id;"
                                    "Seq [Assignment (Identifier \"active_users\") (InnerJoinClause [Join \"users\" \"user_id\",Join \"active_users\" \"user_id\"])]")

   , testCase "group stmt by one field" (testStmt "visits = GROUP active_users BY herd;"
                                                  "Seq [Assignment (Identifier \"visits\") (GroupClause (Identifier \"active_users\") (SingleColumn (Identifier \"herd\")))]")

   , testCase "group stmt by several fields" (testStmt "report = GROUP active_users BY (date, herd);"
                                                       "Seq [Assignment (Identifier \"report\") (GroupClause (Identifier \"active_users\") (MultipleColumn (Tuple [Identifier \"date\",Identifier \"herd\"])))]")

   , testCase "describe stmt" (testStmt "DESCRIBE visits;"
                                        "Seq [Describe (Identifier \"visits\")]")

   , testCase "define stmt" (testStmt "define RESOLVE `python delta.py $date` SHIP('delta.py');"
                                      "Seq [DefineUDF (Identifier \"RESOLVE\") (AliasCommand (Exec \"python delta.py $date\")) [Ship (Filename \"delta.py\")]]")

   , testCase "define stmt second type" (testStmt "DEFINE ISOToUnix org.apache.pig.piggybank.evaluation.datetime.convert.ISOToUnix();"
                                                  "Seq [DefineUDF (Identifier \"ISOToUnix\") (AliasFunction (Function \"org.apache.pig.piggybank.evaluation.datetime.convert.ISOToUnix\" [])) []]")

   , testCase "stream stmt" (testStmt "report = STREAM report THROUGH RESOLVE AS (day:chararray, herd:chararray, day_visits:int, visits:int);"
                                      "Seq [Assignment (Identifier \"report\") (StreamClause (Identifier \"report\") (Identifier \"RESOLVE\") (TupleDef [Field (Identifier \"day\") CharArray,Field (Identifier \"herd\") CharArray,Field (Identifier \"day_visits\") Int,Field (Identifier \"visits\") Int]))]")

   , testCase "store stmt" (testStmt "STORE report INTO '$output' USING ColumnStorage(',');"
                                     "Seq [Store (Identifier \"report\") (Directory \"$output\") (Function \"ColumnStorage\" [ScalarTerm (String \",\")])]")

   , testCase "register stmt" (testStmt "REGISTER 'lib/datafu-0.0.10.jar';"
                                        "Seq [Register (Library \"lib/datafu-0.0.10.jar\")]")

   , testCase "several statements" (testStmt "active_users = LOAD 'warehouse/active_users/daily/point/{$visit_dates}*' USING ColumnStorage(' ') AS (date:chararray, user_id:long);\nactive_users = JOIN users BY user_id, active_users BY user_id;" 
                                             "Seq [Assignment (Identifier \"active_users\") (LoadClause (Filename \"warehouse/active_users/daily/point/{$visit_dates}*\") (Function \"ColumnStorage\" [ScalarTerm (String \" \")]) (TupleDef [Field (Identifier \"date\") CharArray,Field (Identifier \"user_id\") Long])),Assignment (Identifier \"active_users\") (InnerJoinClause [Join \"users\" \"user_id\",Join \"active_users\" \"user_id\"])]")
   , testCase "case insensitivity of keywords" (testStmt "store report into '$output' using ColumnStorage(',');"
                                                         "Seq [Store (Identifier \"report\") (Directory \"$output\") (Function \"ColumnStorage\" [ScalarTerm (String \",\")])]")
   , testCase "input file path" (testFilePath "example.pig" "example.pig")
--   , testCase "non existant file path" (testFileError "dummy.pig" "")
   ]

testStmt :: String -> String -> Assertion
testStmt str expected = expected @=? (show $ parseString str)

testFilePath :: String -> String -> Assertion
testFilePath str expected = do path <- getPath $ parseFile str
                               assertEqual "same file" expected path

{-
testFileError :: String -> String -> Assertion
testFileError str expected = assertException IOError $ parseFile str

assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
    handleJust isWanted (const $ return ()) $ do
        action
        assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
-}

getPath :: IO PigFile -> IO String
getPath pig = liftM filePath pig

filePath :: PigFile -> String
filePath (PigFile path _) = path

