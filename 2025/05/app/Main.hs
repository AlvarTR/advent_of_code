module Main where

import Data.Function (on)
import Data.List (sortBy)
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

sortByFirst :: (Ord a) => [(a, b)] -> [(a, b)]
sortByFirst = sortBy (compare `on` fst)

allFreshInIntervals' :: [(Integer, Integer)] -> [String] -> [(Integer, Integer)]
allFreshInIntervals' list_intervals [] = list_intervals
allFreshInIntervals' [] (str_interval : str_rest) = allFreshInIntervals' [(start, stop)] str_rest
  where
    interval_numbers = (map read $ splitOn "-" str_interval) :: [Integer]
    start = head interval_numbers
    stop = last interval_numbers
allFreshInIntervals' list_intervals (str_interval : str_rest) = allFreshInIntervals' (sortByFirst $ updateFreshnessList [] list_intervals (start, stop)) str_rest
  where
    interval_numbers = (map read $ splitOn "-" str_interval) :: [Integer]
    start = head interval_numbers
    stop = last interval_numbers

updateFreshnessList :: [(Integer, Integer)] -> [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
updateFreshnessList processed_intervals [] new_interval = processed_intervals ++ [new_interval]
updateFreshnessList processed_intervals interval_list@(interval@(list_start, list_stop) : rest) (start, stop)
  | stop < (list_start - 1) = processed_intervals ++ (start, stop) : interval_list
  | (list_stop + 1) < start = updateFreshnessList (processed_intervals ++ [interval]) rest (start, stop)
  | list_start <= start && stop <= list_stop = processed_intervals ++ interval_list
  | start < list_start = updateFreshnessList processed_intervals rest (start, max list_stop stop)
  | list_stop < stop = updateFreshnessList processed_intervals rest (min list_start start, stop)
  | otherwise = processed_intervals ++ interval_list

-- \| otherwise = processed_intervals ++ interval_list
-- where

numberOfFreshInIntervals :: [String] -> Integer
numberOfFreshInIntervals str_list = (+) (toInteger $ length fresh_in_intervals) $ sum $ map (uncurry (flip (-))) fresh_in_intervals
  where
    fresh_in_intervals = allFreshInIntervals' [] str_list

debugIntervals :: [[String]] -> [String] -> [[String]]
debugIntervals buffer [] = reverse $ map reverse buffer
debugIntervals [] (first : rest) = debugIntervals [[first]] rest
debugIntervals buffer@(buffer_head : _) (first : rest) = debugIntervals ((first : buffer_head) : buffer) rest

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
  -- let scan_example_intervals = debugIntervals [] $ lines example_intervals
  -- mapM_ print $ zip (lines example_intervals) $ map (numberOfFreshInIntervals . flip (:) [] . (lines example_intervals !!)) [0 .. length (lines example_intervals) - 1]
  -- mapM_ print $ zip (lines example_intervals) $ map numberOfFreshInIntervals scan_example_intervals
  print $ numberOfFreshInIntervals $ lines example_intervals

  print "Second star input:"
  -- mapM_ print $ zip (lines input_intervals) $ map numberOfFreshInIntervals $ debugIntervals [] $ lines input_intervals
  print $ numberOfFreshInIntervals $ lines input_intervals