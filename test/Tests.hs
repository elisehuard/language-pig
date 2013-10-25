module Main
       where

import Test.Framework (defaultMain)

import Language.Pig.Parser.Test
import Language.Pig.Pretty.Test


main :: IO ()
main = defaultMain [parserSuite, prettyPrintSuite]
