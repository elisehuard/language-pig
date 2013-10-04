module Main
       where

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Pig.Parser.Test


main :: IO ()
main = defaultMain [parserSuite]
