{-# LANGUAGE CPP, DeriveDataTypeable #-}
module Language.Pig.Parser.AST where

#define DERIVE deriving (Eq,Ord,Show,Typeable,Data)

import Data.Data
import Data.Typeable

-- source:
-- http://wiki.apache.org/pig/PigLexer
-- http://wiki.apache.org/pig/PigParser

data Root = Seq [Statement]
            DERIVE

data Statement = Assignment Alias OpClause
               | Describe Alias
               | DefineUDF Alias Command DefineSpec
               | Store Alias Path Function
               DERIVE

data OpClause = LoadClause Path Function TupleDef
              | ForeachClause Alias GenBlock
              | GroupClause Alias GroupBy
              | InnerJoinClause [Join]
              | StreamClause Alias Alias TupleDef
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
               DERIVE

data Join = Join String String
            DERIVE

data DefineSpec = Ship Path
                  DERIVE

data Alias = Identifier String
             DERIVE

data Path = Filename String
          | Directory String
          DERIVE

data Command = Exec String
               DERIVE

data Function = Function String [Argument]
                DERIVE

data Argument = StringArgument Scalar
              | AliasArgument Alias
              DERIVE

data TupleDef = TupleDef [Field]
                DERIVE

data Tuple = Tuple [Alias]
             DERIVE

data Field = Field Alias SimpleType
             DERIVE

data Expression = Unary Operator Expression
                | Binary Operator Expression Expression
                | BooleanUnary Operator Expression
                | BooleanBinary Operator Expression Expression
                | BinCond Expression Expression Expression
                | ScalarTerm Scalar
                | AliasTerm Alias
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

data Operator = Neg
              | Add
              | Subtract
              | Multiply
              | Divide
              | Modulo
              | And
              | Or
              | Not
              | Equal
              | NotEqual
              | Greater
              | Less
              | GreaterEqual
              | LessEqual
              DERIVE
