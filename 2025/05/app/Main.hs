module Main where

import Data.List.Split (splitOn)
import qualified Data.Set as Set

freshIngredient :: [String] -> Integer -> Int
freshIngredient [] _ = 0 -- Not fresh
freshIngredient (interval : rest) ingredient_id
  | ingredient_id `elem` [start .. stop] = 1
  | otherwise = freshIngredient rest ingredient_id
  where
    interval_numbers = (map read $ splitOn "-" interval) :: [Integer]
    start = head interval_numbers
    stop = last interval_numbers

freshIngredient' :: [String] -> Integer -> Int
freshIngredient' [] _ = 0 -- Not fresh
freshIngredient' (interval : rest) ingredient_id
  | ingredient_id >= start && ingredient_id <= stop = 1
  | otherwise = freshIngredient' rest ingredient_id
  where
    interval_numbers = (map read $ splitOn "-" interval) :: [Integer]
    start = head interval_numbers
    stop = last interval_numbers

allFreshInIntervals :: Set.Set Integer -> [String] -> Set.Set Integer
allFreshInIntervals set [] = set
allFreshInIntervals set (interval : rest) = allFreshInIntervals (Set.union set $ Set.fromList [start .. stop]) rest
  where
    interval_numbers = (map read $ splitOn "-" interval) :: [Integer]
    start = head interval_numbers
    stop = last interval_numbers

allFreshInIntervals' :: [(Integer, Integer)] -> [String] -> [(Integer, Integer)]
allFreshInIntervals' set [] = set
allFreshInIntervals' set (interval : rest) = allFreshInIntervals' [(0,0)] rest
  where
    interval_numbers = (map read $ splitOn "-" interval) :: [Integer]
    start = head interval_numbers
    stop = last interval_numbers

main :: IO ()
main = do
  let example_list = splitOn "\n\n" "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
  let example_intervals = head example_list
  let example_food_ids = (map read $ lines $ last example_list) :: [Integer]
  -- 1st star
  print "First star example:"
  -- print "Example intervals:"
  -- mapM_ print $ lines example_intervals
  -- print "Example food ids:"
  -- print example_food_ids
  print $ sum $ map (freshIngredient' $ lines example_intervals) example_food_ids

  -- Input text
  contents <- readFile "input.txt"
  let input_list = splitOn "\n\n" contents
  let input_intervals = head input_list
  let input_food_ids = (map read $ lines $ last input_list) :: [Integer]
  -- print "Input intervals:"
  -- mapM_ print $ lines input_intervals
  -- print "Input food ids:"
  -- print input_food_ids
  print "First star input:"
  print $ sum $ map (freshIngredient' $ lines input_intervals) input_food_ids

  print ""

  -- 2nd star
  print "Second star example:"
  print $ length $ allFreshInIntervals Set.empty $ lines example_intervals

  print "Second star input:"
  print $ length $ allFreshInIntervals Set.empty $ lines input_intervals