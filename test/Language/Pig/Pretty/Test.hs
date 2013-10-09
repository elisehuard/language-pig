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
    testCase "string node" (testPrint (PigString "test") "test")
  ]

testPrint :: PigNode -> String -> Assertion
testPrint tree expected = expected @=? prettyText tree
