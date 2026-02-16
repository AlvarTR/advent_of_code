module Main where

import Test.Hspec (hspec)
import FreshIngredientsSpec

main :: IO ()
main = hspec FreshIngredientsSpec.spec
