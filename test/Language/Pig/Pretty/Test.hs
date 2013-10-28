module Language.Pig.Pretty.Test
where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck (Arbitrary, arbitrary, oneof)
import Control.Applicative ((<$>), (<*>))
import Data.Text (isInfixOf, pack)

import Language.Pig.Parser
import Language.Pig.Parser.Parser
import Language.Pig.Pretty

prettyPrintSuite :: Test
prettyPrintSuite = testGroup "pretty print"
  [
    testCase "empty tree" (testPrint (Seq []) "sequence of statements\n")
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
  , testCase "expression" (testPrint (Seq [Assignment (Identifier "users") (ForeachClause (Identifier "users") (GenBlock [TupleFieldGlob,ExpressionTransform (Binary Divide (Binary Modulo (AliasTerm (Identifier "user_id")) (ScalarTerm (Number (Left 100)))) (ScalarTerm (Number (Left 10)))) (Identifier "cohort")]))]) 
                                     "                                                 sequence of statements                                                \n                                                           |                                                           \n                                                       assignment                                                      \n                                                           |                                                           \n         -----------------------------------------------------------                                                   \n        /                                                           \\                                                  \nidentifier: users                                             FOREACH clause                                           \n                                                                    |                                                  \n                            --------------------------------------------------                                         \n                           /                                                  \\                                        \n                   identifier: users                                 transformation block                              \n                                                                              |                                        \n                                       ----------------------------------------                                        \n                                      /                                        \\                                       \n                                      *                                    calculate                                   \n                                                                               |                                       \n                                                                      ---------------------------------------          \n                                                                     /                                       \\         \n                                                             binary expression                       identifier: cohort\n                                                                     |                                                 \n                                            --------------------------------------------------                         \n                                           /                        |                         \\                        \n                                         Divide             binary expression             double:10                    \n                                                                    |                                                  \n                                                    ------------------------------                                     \n                                                   /              |               \\                                    \n                                                 Modulo  identifier: user_id  double:100                               \n")
--  , testProperty "pretty prints to ast" prop_printed
  ]

testPrint :: Root -> String -> Assertion
testPrint tree expected = expected @=? prettyPrint tree

prop_printed tree = (pack "statement") `isInfixOf` (pack $ prettyPrint tree)
      where types = (tree :: Root)

instance Arbitrary Root where
  arbitrary = Seq <$> arbitrary

instance Arbitrary Statement where
  arbitrary = oneof [Assignment <$> arbitrary <*> arbitrary,
                     Describe <$> arbitrary,
                     DefineUDF <$> arbitrary <*> arbitrary <*> arbitrary,
                     Store <$> arbitrary <*> arbitrary <*> arbitrary]

instance Arbitrary OpClause where
  arbitrary = oneof [ LoadClause <$> arbitrary <*> arbitrary <*> arbitrary
                    , ForeachClause <$> arbitrary <*> arbitrary
                    , GroupClause <$> arbitrary <*> arbitrary
                    , InnerJoinClause <$> arbitrary
                    , StreamClause <$> arbitrary <*> arbitrary <*> arbitrary]

instance Arbitrary GenBlock where
  arbitrary = GenBlock <$> arbitrary

instance Arbitrary GroupBy where
  arbitrary = oneof [ SingleColumn <$> arbitrary
                    , MultipleColumn <$> arbitrary ]

instance Arbitrary Transform where
  arbitrary = oneof [ Flatten <$> arbitrary <*> arbitrary
                    , return TupleFieldGlob
                    , AliasTransform <$> arbitrary <*> arbitrary
                    , ExpressionTransform <$> arbitrary <*> arbitrary
                    , FunctionTransform <$> arbitrary <*> arbitrary
                    , EnvTransform <$> arbitrary <*> arbitrary]

instance Arbitrary Join where
  arbitrary = Join <$> arbitrary <*> arbitrary

instance Arbitrary DefineSpec where
  arbitrary = Ship <$> arbitrary

instance Arbitrary Alias where
  arbitrary = Identifier <$> arbitrary

instance Arbitrary Language.Pig.Parser.Parser.Path where
  arbitrary = oneof [ Filename <$> arbitrary
                    , Directory <$> arbitrary ]

instance Arbitrary Command where
  arbitrary = Exec <$> arbitrary

instance Arbitrary Function where
  arbitrary = Function <$> arbitrary <*> arbitrary

instance Arbitrary Argument where
  arbitrary = oneof [ StringArgument <$> arbitrary,
                      AliasArgument <$> arbitrary]
                    
instance Arbitrary TupleDef where
   arbitrary = TupleDef <$> arbitrary

instance Arbitrary Tuple where
  arbitrary = Tuple <$> arbitrary

instance Arbitrary Field where
  arbitrary = Field <$> arbitrary <*> arbitrary

instance Arbitrary Expression where
  arbitrary = oneof [ Unary <$> arbitrary <*> arbitrary
                    , Binary <$> arbitrary <*> arbitrary <*> arbitrary
                    , BooleanUnary <$> arbitrary <*> arbitrary
                    , BooleanBinary <$> arbitrary <*> arbitrary <*> arbitrary
                    , BinCond <$> arbitrary <*> arbitrary <*> arbitrary
                    , ScalarTerm <$> arbitrary
                    , AliasTerm <$> arbitrary ]

instance Arbitrary Scalar where
  arbitrary = oneof [ Number <$> arbitrary
                    , String <$> arbitrary ]
               
instance Arbitrary SimpleType where
  arbitrary = oneof [ return Int , return Long , return Float , return Double , return CharArray , return ByteArray]

instance Arbitrary Operator where
  arbitrary = oneof [ return Neg , return Add , return Subtract , return Multiply , return Divide , return Modulo , return And
              , return Or , return Not , return Equal , return NotEqual , return Greater , return Less , return GreaterEqual , return LessEqual ]
