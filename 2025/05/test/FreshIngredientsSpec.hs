module FreshIngredientsSpec where

import FreshIngredients
    ( Interval,
      freshIngredient,
      freshIngredient',
      allFreshInIntervals,
      sortByFirst,
      allFreshInIntervals',
      updateFreshnessList,
      numberOfFreshInIntervals,
      duplify,
      stringListToListOfIntervals )
import Test.Hspec
    ( describe, it, anyErrorCall, shouldBe, shouldThrow, Spec )
import qualified Data.Set as Set
import Control.Exception (evaluate)

spec :: Spec
spec = do
  describe "freshIngredient" $ do
    it "returns 0 for empty interval list" $
      freshIngredient [] 5 `shouldBe` 0

    it "returns 1 when ingredient is in interval" $
      freshIngredient [(1, 10)] 5 `shouldBe` 1

    it "returns 0 when ingredient is not in interval" $
      freshIngredient [(1, 10)] 15 `shouldBe` 0

    it "returns 1 when ingredient is at interval start" $
      freshIngredient [(5, 10)] 5 `shouldBe` 1

    it "returns 1 when ingredient is at interval end" $
      freshIngredient [(5, 10)] 10 `shouldBe` 1

    it "checks multiple intervals" $
      freshIngredient [(1, 3), (5, 7), (9, 12)] 6 `shouldBe` 1

    it "returns 0 when not in any of multiple intervals" $
      freshIngredient [(1, 3), (5, 7), (9, 12)] 4 `shouldBe` 0

  describe "freshIngredient'" $ do
    it "returns 0 for empty interval list" $
      freshIngredient' [] 5 `shouldBe` 0

    it "returns 1 when ingredient is in interval" $
      freshIngredient' [(1, 10)] 5 `shouldBe` 1

    it "returns 0 when ingredient is not in interval" $
      freshIngredient' [(1, 10)] 15 `shouldBe` 0

  describe "allFreshInIntervals" $ do
    it "returns empty set for empty intervals" $
      allFreshInIntervals mempty [] `shouldBe` mempty

    it "returns union of all intervals" $
      allFreshInIntervals mempty [(1, 3), (5, 7)] `shouldBe` Set.fromList [1, 2, 3, 5, 6, 7]

  describe "sortByFirst" $ do
    it "sorts by first element" $
      sortByFirst [(5, 10), (1, 3), (3, 7)] `shouldBe` [(1, 3), (3, 7), (5, 10)]

    it "handles empty list" $
      sortByFirst [] `shouldBe` ([] :: [(Integer, Integer)])

    it "handles single element" $
      sortByFirst [(5, 10)] `shouldBe` [(5, 10)]

  describe "allFreshInIntervals'" $ do
    it "merges overlapping intervals" $
      allFreshInIntervals' [] [(1, 5), (3, 7), (10, 15)] `shouldBe` [(1, 7), (10, 15)]

    it "handles empty list" $
      allFreshInIntervals' [] [] `shouldBe` ([] :: [Interval])

    it "handles non-overlapping intervals" $
      allFreshInIntervals' [] [(1, 3), (5, 7), (9, 12)] `shouldBe` [(1, 3), (5, 7), (9, 12)]

  describe "updateFreshnessList" $ do
    it "adds new interval to empty list" $
      updateFreshnessList [] [] (1, 5) `shouldBe` [(1, 5)]

    it "inserts before when completely before" $
      updateFreshnessList [] [(5, 10)] (1, 3) `shouldBe` [(1, 3), (5, 10)]

    it "inserts after when completely after" $
      updateFreshnessList [] [(1, 3)] (5, 10) `shouldBe` [(1, 3), (5, 10)]

    it "merges when overlapping" $
      updateFreshnessList [] [(3, 7)] (1, 5) `shouldBe` [(1, 7)]

  describe "numberOfFreshInIntervals" $ do
    it "counts fresh in non-overlapping intervals" $
      numberOfFreshInIntervals [(1, 3), (5, 7)] `shouldBe` 6

    it "counts fresh in overlapping intervals" $
      numberOfFreshInIntervals [(1, 5), (3, 7)] `shouldBe` 7

    it "handles empty list" $
      numberOfFreshInIntervals [] `shouldBe` 0

    it "handles single interval" $
      numberOfFreshInIntervals [(1, 10)] `shouldBe` 10

  describe "duplify" $ do
    it "converts 2-element list to tuple" $
      duplify [1, 2] `shouldBe` (1, 2)

    it "throws error on empty list" $
      evaluate (duplify []) `shouldThrow` anyErrorCall

    it "throws error on single element" $
      evaluate (duplify [1]) `shouldThrow` anyErrorCall

    it "throws error on more than 2 elements" $
      evaluate (duplify [1, 2, 3]) `shouldThrow` anyErrorCall

  describe "stringListToListOfIntervals" $ do
    it "parses single interval string" $
      stringListToListOfIntervals ["1-5"] `shouldBe` [(1, 5)]

    it "parses multiple interval strings" $
      stringListToListOfIntervals ["1-5", "10-15"] `shouldBe` [(1, 5), (10, 15)]
