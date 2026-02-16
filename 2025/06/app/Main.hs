module Main where

import Data.List.Split (splitOn)
import qualified Data.Text as Text

stringStrip :: String -> String
stringStrip = Text.unpack . Text.strip . Text.pack

removeSpacesAndEmpties :: [String] -> [[String]]
removeSpacesAndEmpties = map (filter ("" /=) . splitOn " ")

wideMaths :: Integer -> [Int -> Int -> Int] -> [[Int]] -> Integer
wideMaths accum [] ([] : _) = accum
wideMaths accum (operation : rest) matrix = wideMaths ((+) accum $ toInteger $ foldr1 operation (map head matrix)) rest $ map tail matrix

transposeChars :: [String] -> [[Char]] -> [String]
transposeChars str_number_list ([] : _) = str_number_list
transposeChars str_number_list char_matrix = transposeChars (map head char_matrix : str_number_list) $ map tail char_matrix

cephalopodNumberMatrix :: [[Char]] -> [[Integer]]
cephalopodNumberMatrix = map (map read) . splitOn [""] . map stringStrip . transposeChars []

stringToIntOperation :: [Char] -> Int -> Int -> Int
stringToIntOperation [op]
  | op == '+' = (+)
  | op == '*' = (*)

stringToIntegerOperation :: [Char] -> Integer -> Integer -> Integer
stringToIntegerOperation [op]
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
  let example_int_operations = map stringToIntOperation example_str_operations
  let example_numbers = map (map read) example_str_numbers :: [[Int]]

  -- 1st star
  print "First star example:"
  print $ wideMaths 0 example_int_operations example_numbers

  -- Input text
  contents <- readFile "input.txt"
  let (input_str_operations : input_str_numbers) = reverse $ removeSpacesAndEmpties $ lines contents
  let input_int_operations = map stringToIntOperation input_str_operations
  let input_numbers = map (map read) input_str_numbers :: [[Int]]
  -- print $ zip input_str_operations input_numbers

  print "First star input:"
  print $ wideMaths 0 input_int_operations input_numbers

  -- 2nd star
  print "Second star example:"
  let example_integer_operations = map stringToIntegerOperation $ reverse example_str_operations
  -- mapM_ print $ zip example_str_operations $ cephalopodNumberMatrix $ init example
  -- print $ zipWith foldr1 example_integer_operations $ cephalopodNumberMatrix $ init example
  print $ sum $ zipWith foldr1 example_integer_operations $ cephalopodNumberMatrix $ init example

  print "Second star input:"
  let input_integer_operations = map stringToIntegerOperation $ reverse input_str_operations
  -- mapM_ print $ zip input_str_operations $ cephalopodNumberMatrix $ init $ lines contents
  -- print $ zipWith foldr1 input_integer_operations $ cephalopodNumberMatrix $ init $ lines contents
  print $ sum $ zipWith foldr1 input_integer_operations $ cephalopodNumberMatrix $ init $ lines contents
