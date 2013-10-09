{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Language.Pig.Pretty
    ( pretty
    , prettyText)
where

import Language.Pig.Parser.Parser
import Text.PrettyPrint as TextPP

-- | All types which can be transformed into a 'Doc'.
class Pretty a where
   pretty :: a -> Doc

-- | Transform values into strings.
prettyText :: Pretty a => a -> String
prettyText = render . pretty

instance Pretty String where
   pretty s = text s

instance Pretty Integer where
  pretty = integer

instance Pretty PigNode where
   pretty (PigString s) = text s

