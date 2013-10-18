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
    testCase "empty tree" (testPrint (Seq []) "empty ast\n")
  , testCase "load statement" (testPrint (Seq [Assignment (Identifier "active_users") (LoadClause (Filename "warehouse/active_users/daily/point/{$visit_dates}*") (Function "ColumnStorage" [StringArgument (String " ")]) (TupleDef [Field (Identifier "date") CharArray,Field (Identifier "user_id") Long]))])
                                         "                                                                           sequence of statements                                                                           \n                                                                                     |                                                                                      \n                                                                                 assignment                                                                                 \n                                                                                     |                                                                                      \n            --------------------------------------------------------------------------------------                                                                          \n           /                                                                                      \\                                                                         \nidentifier: active_users                                                                     LOAD clause                                                                    \n                                                                                                  |                                                                         \n                                                         -------------------------------------------------------------------------------------                              \n                                                        /                                           |                                         \\                             \n                          filename: \"warehouse/active_users/daily/point/{$visit_dates}*\"  function ColumnStorage                          tuple def                         \n                                                                                                    |                                         |                             \n                                                                                           string argument: \" \"                  -----------------------------              \n                                                                                                                                /                             \\             \n                                                                                                                  field: date of type CharArray  field: user_id of type Long\n")
{-

                                                                           sequence of statements
                                                                                     |
                                                                                 assignment
                                                                                     |
            --------------------------------------------------------------------------------------
           /                                                                                      \
identifier: active_users                                                                     LOAD clause
                                                                                                  |
                                                         -------------------------------------------------------------------------------------
                                                        /                                           |                                         \
                          filename: "warehouse/active_users/daily/point/{$visit_dates}*"  function ColumnStorage                          tuple def
                                                                                                    |                                         |
                                                                                           string argument: " "                  -----------------------------
                                                                                                                                /                             \
                                                                                                                  field: date of type CharArray  field: user_id of type Long

-}
  ]

testPrint :: Root -> String -> Assertion
testPrint tree expected = expected @=? prettyPrint tree
