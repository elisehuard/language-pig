{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Language.Pig.Parser.AST where

#define DERIVE deriving (Eq,Ord,Show,Typeable,Data)

import Data.Data
import Data.Typeable

-- source:
-- http://wiki.apache.org/pig/PigLexer
-- http://wiki.apache.org/pig/PigParser

data PigFile = PigFile String Root

data Root = Seq [Statement]
            DERIVE

data Statement = Assignment Alias OpClause
               | Describe Alias
               | DefineUDF Alias Aliasable [DefineSpec]
               | Store Alias Path Function
               | Register Library
               DERIVE

data OpClause = LoadClause Path (Maybe Function) (Maybe TupleDef)
              | ForeachClause Alias GenBlock
              | GroupClause Alias GroupBy
              | InnerJoinClause [Join]
              | StreamClause Alias Alias TupleDef
              | DistinctClause Alias
              | FilterClause Alias BooleanExpression
              DERIVE

data GenBlock = GenBlock [Transform]
                DERIVE

data GroupBy = SingleColumn Alias
             | MultipleColumn Tuple
             DERIVE

data Transform = Flatten String Tuple -- foreach flatten transform
               | TupleFieldGlob
               | AliasTransform Alias Alias
               | ExpressionTransform Expression Alias
               | FunctionTransform Function Alias
               | EnvTransform Scalar Alias
               | IdentityTransform Alias
               | PositionalTypeTransform SimpleType Pos Alias
               DERIVE

type Pos = Integer

data Join = Join String String
            DERIVE

data DefineSpec = Ship [Path]
                  DERIVE

data Alias = Identifier String
             DERIVE

data Path = Filename String
          | Directory String
          DERIVE

data Library = Library String
               DERIVE

data Aliasable = AliasCommand Command
               | AliasFunction Function
               DERIVE

data Command = Exec String
               DERIVE

data Function = Function String [Expression]
                DERIVE

data TupleDef = TupleDef [Field]
                DERIVE

data Tuple = Tuple [Alias]
             DERIVE

data Field = Field Alias (Maybe SimpleType)
             DERIVE

data Expression = Unary Operator Expression
                | Binary Operator Expression Expression
                | BinCond BooleanExpression Expression Expression
                | ScalarTerm Scalar
                | AliasTerm Alias
                | FunctionTerm Function
                DERIVE

data BooleanExpression = BooleanExpression ComparisonOperator Expression Expression
                       | BooleanUnary BooleanOperator BooleanExpression
                       | BooleanBinary BooleanOperator BooleanExpression BooleanExpression
                       DERIVE

data Scalar = Number (Either Integer Double)
            | String String
            DERIVE

data SimpleType = Int
                | Long
                | Float
                | Double
                | CharArray
                | ByteArray
                DERIVE

data BooleanOperator = And
                     | Or
                     | Not
                     DERIVE

data Operator = Neg
              | Add
              | Subtract
              | Multiply
              | Divide
              | Modulo
              DERIVE

data ComparisonOperator = Equal
                        | NotEqual
                        | Greater
                        | Less
                        | GreaterEqual
                        | LessEqual
                        DERIVE
