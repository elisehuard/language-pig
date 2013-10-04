module Language.Pig.Parser.AST (
--       PigNode(..)
       ) where

-- source:
-- http://wiki.apache.org/pig/PigLexer
-- http://wiki.apache.org/pig/PigParser

-- PROBLEM: using separate namespace for this causes ld issue on Mac OS X - investigate
{-
data PigNode =  ...
             deriving (Show) -- Eq, Read, Data, Typeable ?
-}
