module Main where

import Data.List.Split (splitOn)

main :: IO ()
main = do
  let example_list = splitOn "\n\n" "3-5\n10-14\n16-20\n12-18\n\n1\n5\n8\n11\n17\n32"
  let example_intervals = head example_list
  let example_food_ids = (map read $ lines $ last example_list) :: [Integer]
  -- 1st star
  print "First star example:"
  print "Example intervals:"
  mapM_ print $ lines example_intervals
  print "Example food ids:"
  print example_food_ids

  -- Input text
  contents <- readFile "input.txt"
  let input_list = splitOn "\n\n" contents
  let input_intervals = head input_list
  let input_food_ids = (map read $ lines $ last input_list) :: [Integer]
  print "Input intervals:"
  mapM_ print $ lines input_intervals
  print "Input food ids:"
  print input_food_ids
  print "First star input:"

  print ""

  -- 2nd star
  print "Second star example:"

  print "Second star input:"