module Main where

import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Interval = (Integer, Integer)

freshIngredient :: [Interval] -> Integer -> Int
freshIngredient [] _ = 0 -- Not fresh
freshIngredient ((start, stop) : rest) ingredient_id
  | ingredient_id `elem` [start .. stop] = 1
  | otherwise = freshIngredient rest ingredient_id

freshIngredient' :: [Interval] -> Integer -> Int
freshIngredient' [] _ = 0 -- Not fresh
freshIngredient' ((start, stop) : rest) ingredient_id
  | ingredient_id >= start && ingredient_id <= stop = 1
  | otherwise = freshIngredient' rest ingredient_id

allFreshInIntervals :: Set.Set Integer -> [Interval] -> Set.Set Integer
allFreshInIntervals set [] = set
allFreshInIntervals set ((start, stop) : rest) = allFreshInIntervals (Set.union set $ Set.fromList [start .. stop]) rest

sortByFirst :: (Ord a) => [(a, b)] -> [(a, b)]
sortByFirst = sortBy (compare `on` fst)

allFreshInIntervals' :: [Interval] -> [Interval] -> [Interval]
allFreshInIntervals' list_intervals [] = list_intervals
allFreshInIntervals' [] ((start, stop) : str_rest) = allFreshInIntervals' [(start, stop)] str_rest
allFreshInIntervals' list_intervals ((start, stop) : str_rest) = allFreshInIntervals' (updateFreshnessList [] list_intervals (start, stop)) str_rest

updateFreshnessList :: [Interval] -> [Interval] -> Interval -> [Interval]
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

numberOfFreshInIntervals :: [Interval] -> Integer
numberOfFreshInIntervals str_list = (+) (toInteger $ length fresh_in_intervals) $ sum $ map (uncurry (flip (-))) fresh_in_intervals
  where
    fresh_in_intervals = allFreshInIntervals' [] str_list

debugLineByLine :: [[a]] -> [a] -> [[a]]
debugLineByLine buffer [] = reverse $ map reverse buffer
debugLineByLine [] (first : rest) = debugLineByLine [[first]] rest
debugLineByLine buffer@(buffer_head : _) (first : rest) = debugLineByLine ((first : buffer_head) : buffer) rest

duplify :: [a] -> (a, a)
duplify [] = error ""
duplify [_] = error ""
duplify [x, y] = (x, y)
duplify (_ : _ : _) = error ""

stringListToListOfIntervals :: [String] -> [Interval]
stringListToListOfIntervals = map (duplify . map read . splitOn "-")

main :: IO ()
main = do
  let example_list = splitOn "\n\n" "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
  let example_intervals = sortByFirst $ stringListToListOfIntervals $ lines $ head example_list
  let example_food_ids = (map read $ lines $ last example_list) :: [Integer]
  -- 1st star
  print "First star example:"
  -- print "Example intervals:"
  mapM_ print example_intervals
  -- print "Example food ids:"
  -- print example_food_ids
  print $ sum $ map (freshIngredient' example_intervals) example_food_ids

  -- Input text
  contents <- readFile "input.txt"
  let input_list = splitOn "\n\n" contents
  let input_intervals = sortByFirst $ stringListToListOfIntervals $ lines $ head input_list
  let input_food_ids = (map read $ lines $ last input_list) :: [Integer]
  -- print "Input intervals:"
  -- mapM_ print input_intervals
  -- print "Input food ids:"
  -- print input_food_ids
  print "First star input:"
  print $ sum $ map (freshIngredient' input_intervals) input_food_ids

  print ""

  -- 2nd star
  print "Second star example:"
  -- let scan_example_intervals = debugLineByLine [] example_intervals
  -- mapM_ print $ zip example_intervals $ map (numberOfFreshInIntervals . flip (:) [] . (example_intervals !!)) [0 .. length example_intervals - 1]
  -- mapM_ print $ zip example_intervals $ map numberOfFreshInIntervals scan_example_intervals
  print $ numberOfFreshInIntervals example_intervals

  print "Second star input:"
  -- mapM_ print $ zip input_intervals $ map numberOfFreshInIntervals $ debugLineByLine [] input_intervals
  print $ numberOfFreshInIntervals input_intervals