{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.Pig.Pretty
    ( prettyPrint )
where

import Language.Pig.Parser.Parser
import Text.ParserCombinators.Parsec

import Data.List (intercalate)
import Data.Tree hiding (Tree)

toDataTree (PigString s) = Node ("string: " ++ s) []
toDataTree (PigAssignment a b) = Node "assignment" (map toDataTree [a, b])
toDataTree (PigIdentifier s) = Node ("identifier: " ++ s) []
toDataTree (PigFilename s) = Node ("filename: \"" ++ s ++ "\"") []
toDataTree (PigLoadClause a b c) = Node "load stmt" (map toDataTree [a, b, c])
toDataTree (PigFunc a b) = Node ("function: " ++ a ++ "(" ++ prettyText b ++ ")") []
toDataTree (PigSchema list) = Node "schema" (map toDataTree list)
toDataTree (PigField (PigFieldName a) (PigFieldType b)) = Node ("field " ++ a ++ ": " ++ show b) []
toDataTree (PigForeachClause a b) = Node "foreach stmt" (map toDataTree [a, b])
toDataTree (PigTransforms list) = Node "transforms" (map toDataTree list)
toDataTree (PigFlatten a b) = Node ("flatten " ++ a) (listToTree b)
toDataTree (PigFieldName a) = Node ("field: " ++ a) []

listToTree (PigTuple list) = map toDataTree list

prettyText :: PigNode -> String
prettyText (PigArguments b) = intercalate "," $ map prettyText b

prettyPrint ast = drawTree $ toDataTree ast