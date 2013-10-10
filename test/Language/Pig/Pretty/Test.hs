module Language.Pig.Pretty.Test
where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Pig.Parser
import Language.Pig.Parser.Parser
import Language.Pig.Pretty

prettyPrintSuite :: Test
prettyPrintSuite = testGroup "pretty print"
  [
    testCase "string node" (testPrint (PigString "test") "string: test\n")
  , testCase "load stmt tree" (testPrint (PigAssignment (PigIdentifier "users") (PigLoadClause (PigFilename "sorted_log/user_registration/$date/*") (PigFunc "LogStorage" (PigArguments [])) (PigSchema [PigField (PigFieldName "date") (PigFieldType PigCharArray),PigField (PigFieldName "time") (PigFieldType PigCharArray),PigField (PigFieldName "user_id") (PigFieldType PigLong)]))) "assignment\n|\n+- identifier: users\n|\n`- load stmt\n   |\n   +- filename: \"sorted_log/user_registration/$date/*\"\n   |\n   +- function: LogStorage()\n   |\n   `- schema\n      |\n      +- field date: PigCharArray\n      |\n      +- field time: PigCharArray\n      |\n      `- field user_id: PigLong\n")
  ]

testPrint :: PigNode -> String -> Assertion
testPrint tree expected = expected @=? prettyPrint tree
