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

-- top level node
toDataTree (PigSeq list) = Node "statement sequence" (map toDataTree list)

-- non terminal nodes
toDataTree (PigAssignment a b) = Node "assignment" (map toDataTree [a, b])
toDataTree (PigLoadClause a b c) = Node "load stmt" (map toDataTree [a, b, c])
toDataTree (PigSchema list) = Node "schema" (map toDataTree list)
toDataTree (PigForeachClause a b) = Node "foreach stmt" (map toDataTree [a, b])
toDataTree (PigTransforms list) = Node "transforms" (map toDataTree list)
toDataTree (PigFlatten a b) = Node ("flatten " ++ a) [toDataTree b]
toDataTree (PigTuple list) = Node ("tuple") (map toDataTree list)

-- terminal nodes
toDataTree (PigString s) = Node ("string: " ++ s) []
toDataTree (PigIdentifier s) = Node ("identifier: " ++ s) []
toDataTree (PigFilename s) = Node ("filename: \"" ++ s ++ "\"") []
toDataTree (PigFunc a b) = Node ("function: " ++ a ++ "(" ++ prettyText b ++ ")") []
toDataTree (PigField (PigFieldName a) (PigFieldType b)) = Node ("field " ++ a ++ ": " ++ show b) []
toDataTree (PigFieldName a) = Node ("field: " ++ a) []

prettyText :: PigNode -> String
prettyText (PigArguments b) = intercalate "," $ map prettyText b

prettyPrint ast = drawTree $ toDataTree ast
