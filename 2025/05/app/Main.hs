module Main where

import Data.List.Split (splitOn)
import FreshIngredients
    ( freshIngredient',
      sortByFirst,
      numberOfFreshInIntervals,
      stringListToListOfIntervals )

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