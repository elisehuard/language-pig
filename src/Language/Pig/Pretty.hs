{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.Pig.Pretty
    ( prettyPrint )
where

import Language.Pig.Parser.Parser
import Text.ParserCombinators.Parsec

import Data.List (intercalate)
import Data.Tree hiding (Tree)

-- patterns:
--  node name + list nodes
--  node name + literal to display = terminalnode
--  node name + arguments used as lists to use as nodes

{-
-- top level node
toDataTree (Seq list) = Node "statement sequence" (map toDataTree list)

-- non terminal nodes
toDataTree (Assignment a b) = Node "assignment" (map toDataTree [a, b])
toDataTree (LoadClause a b c) = Node "load stmt" (map toDataTree [a, b, c])
toDataTree (TupleDef list) = Node "schema" (map toDataTree list)
toDataTree (ForeachClause a b) = Node "foreach stmt" (map toDataTree [a, b])
toDataTree (GenBlock list) = Node "transforms" (map toDataTree list)
toDataTree (Flatten a b) = Node ("flatten " ++ a) [toDataTree b]
toDataTree (Tuple list) = Node ("tuple") (map toDataTree list)

-- terminal nodes
toDataTree (PigString s) = Node ("string: " ++ s) []
toDataTree (Identifier s) = Node ("identifier: " ++ s) []
toDataTree (Filename s) = Node ("filename: \"" ++ s ++ "\"") []
toDataTree (Function a b) = Node ("function: " ++ a ++ "(" ++ (map toDataTree b) ++ ")") []
toDataTree (Field (Identifier a) b) = Node ("field " ++ a ++ ": " ++ show b) []
-}

prettyPrint ast = id
