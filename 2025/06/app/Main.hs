module Main where

import Data.List.Split (splitOn)

removeSpacesAndEmpties :: [String] -> [[String]]
removeSpacesAndEmpties = map (filter ("" /=) . splitOn " ")

wideMaths :: Integer -> [Int -> Int -> Int] -> [[Int]] -> Integer
wideMaths accum [] _ = accum
wideMaths accum (operation:rest) matrix = wideMaths ((+) accum $ toInteger $ foldr1 operation (map head matrix)) rest $ map tail matrix

stringToOperation :: [Char] -> Int -> Int -> Int
stringToOperation [op]
  | op == '+' = (+)
  | op == '*' = (*)

main :: IO ()
main = do
  let example =
        [ "123 328  51 64 ",
          " 45 64  387 23 ",
          "  6 98  215 314",
          "*   +   *   +  "
        ]
  -- mapM_ print $ removeSpacesAndEmpties example
  let (example_str_operations : example_str_numbers) = reverse $ removeSpacesAndEmpties example
  let example_operations = map stringToOperation example_str_operations
  let example_numbers = map (map read) example_str_numbers :: [[Int]]

  -- 1st star
  print "First star example:"
  print $ wideMaths 0 example_operations example_numbers

  -- Input text
  contents <- readFile "input.txt"
  let (input_str_operations : input_str_numbers) = reverse $ removeSpacesAndEmpties $ lines contents
  let input_operations = map stringToOperation input_str_operations
  let input_numbers = map (map read) input_str_numbers :: [[Int]]
  -- print $ zip input_str_operations input_numbers

  print "First star input:"
  print $ wideMaths 0 input_operations input_numbers

  -- 2nd star
  print "Second star example:"

  print "Second star input:"
