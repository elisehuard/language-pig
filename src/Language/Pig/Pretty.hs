{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.Pig.Pretty
    ( prettyPrint )
where

import Language.Pig.Parser.Parser
import Text.ParserCombinators.Parsec

import Data.List (intercalate)
import Data.Tree
import Data.Tree.Pretty

import Control.Applicative ((<$>), (<*>))

-- patterns:
--  node name + list nodes
--  node name + literal to display = terminalnode
--  node name + arguments used as lists to use as nodes

class Treeable a where
  toTree :: a -> (Tree String)

instance Treeable Root where
  toTree (Seq stmts) = Node "sequence of statements" (map toTree stmts)

instance Treeable Statement where
  toTree (Assignment a b) = Node "assignment" [toTree a, toTree b]
  toTree (Describe a) = Node "describe statement" [toTree a]
  toTree (DefineUDF a b c) = Node "define UDF statement" [toTree a, toTree b, toTree c]
  toTree (Store a b c) = Node "store statement" [toTree a, toTree b, toTree c]

instance Treeable OpClause where
  toTree (LoadClause a b c) = Node "LOAD clause" [toTree a, toTree b, toTree c]
  toTree (ForeachClause a b) = Node "FOREACH clause" [toTree a, toTree b]
  toTree (GroupClause a b) = Node "GROUP clause" [toTree a, toTree b]
  toTree (InnerJoinClause joins) = Node "JOIN clause" (map toTree joins)
  toTree (StreamClause a b c) = Node "STREAM clause" [toTree a, toTree b, toTree c]

instance Treeable GenBlock where
  toTree (GenBlock transforms) = Node "transformation block" (map toTree transforms)

instance Treeable GroupBy where
  toTree (SingleColumn a) = Node "group by" [toTree a]
  toTree (MultipleColumn a) = Node "group by" [toTree a]

instance Treeable Transform where
  toTree (Flatten a b) = Node ("FLATTEN: " ++ a) [toTree b]
  toTree (TupleFieldGlob) = Node "*" []
  toTree (AliasTransform a b) = Node "alias" [toTree a, toTree b]
  toTree (ExpressionTransform a b) = Node "calculate" [toTree a, toTree b]
  toTree (FunctionTransform a b) = Node "function expression" [toTree a, toTree b]
  toTree (EnvTransform a b) = Node "name variable" [toTree a, toTree b]

instance Treeable Join where
  toTree (Join a b) = Node ("join " ++ a ++ " by " ++ b) []

instance Treeable DefineSpec where
  toTree (Ship p) = Node "SHIP" [toTree p]

instance Treeable Alias where
  toTree (Identifier s) = Node ("identifier: " ++ s) []

instance Treeable Path where
  toTree (Filename s) = Node ("filename: \"" ++ s ++ "\"") []
  toTree (Directory s) = Node ("directory: \"" ++ s ++ "\"") []

instance Treeable Command where
  toTree (Exec s) = Node ("execute command: " ++ s) []

instance Treeable Function where
  toTree (Function s a) = Node ("function " ++ s) (map toTree a)

instance Treeable Argument where
  toTree (StringArgument (String s)) = Node ("string argument: \"" ++ s ++ "\"") []
  toTree (StringArgument (Number s)) = Node ("number argument: " ++ show s) []
  toTree (AliasArgument (Identifier s)) = Node ("identifier argument: \"" ++ s ++ "\"") []

instance Treeable TupleDef where
  toTree (TupleDef f) = Node "tuple def" (map toTree f)

instance Treeable Tuple where
  toTree (Tuple t) = Node "tuple" (map toTree t)

instance Treeable Field where
  toTree (Field (Identifier s) t) = Node ("field: " ++ s ++ " of type " ++ show t) []

instance Treeable Expression where
  toTree (Unary o e) = Node "unary expression" [toTree o, toTree e]
  toTree (Binary o e1 e2) = Node "binary expression" [toTree o, toTree e1, toTree e2]
  toTree (BinCond e1 e2 e3) = Node "ternary conditional expression" [toTree e1, toTree e2, toTree e3]
  toTree (ScalarTerm (String s)) = Node ("scalar: string " ++ s) []
  toTree (ScalarTerm number) = toTree number
  toTree (AliasTerm alias) = toTree alias

instance Treeable BooleanExpression where
  toTree (BooleanExpression o e1 e2) = Node "comparison expression" [toTree o, toTree e1, toTree e2]
  toTree (BooleanUnary o e) = Node "boolean unary expression" [toTree o, toTree e]
  toTree (BooleanBinary o e1 e2) = Node "boolean binary expression" [toTree o, toTree e1, toTree e2]

instance Treeable Scalar where
  toTree (Number (Right i)) = Node ("integer:" ++ show i) []
  toTree (Number (Left f)) = Node ("double:" ++ show f) []
  toTree (String s) = Node ("string: \"" ++ s ++ "\"") []

instance Treeable SimpleType where
  toTree c = Node (show c) []

instance Treeable Operator where
  toTree c = Node (show c) []

instance Treeable BooleanOperator where
  toTree c = Node (show c) []

instance Treeable ComparisonOperator where
  toTree c = Node (show c) []

prettyPrint :: Root -> String
prettyPrint (Seq []) = "no statements\n"
prettyPrint (Seq statements) = "sequence of statements:\n" ++
                                (intercalate "\n" (map drawVerticalTree $ map toTree statements))
