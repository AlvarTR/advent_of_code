import Data.List (nub)
-- import GHC.Natural (Natural, naturalFromInteger)
-- import GHC.Num (integerFromNatural)

recursiveSplitAt :: [String] -> Int -> String -> [String]
recursiveSplitAt string_list _ "" = string_list
recursiveSplitAt string_list split_index str = recursiveSplitAt (string_list ++ [split_prefix]) split_index rest
  where
    (split_prefix, rest) = splitAt split_index str

maxFromRightToLeft :: [[Integer]] -> [[Integer]]
maxFromRightToLeft = map (init . scanr max 1)

maxFromLeftToRight :: [[Integer]] -> [[Integer]]
maxFromLeftToRight = map (tail . scanl max 1)

joltageApproximation :: [Integer] -> [Integer] -> Integer
joltageApproximation left@(_:(_:_)) (first_right:(second_right:_))
  | first_right > vice_last_left = first_right * 10 + second_right
  | first_right < vice_last_left = vice_last_left * 10 + last_left
  | first_right == vice_last_left && second_right > last_left = first_right * 10 + second_right
  | first_right == vice_last_left && second_right < last_left = vice_last_left * 10 + last_left
  | otherwise = first_right * 10 + second_right
  where
    last_left = last left
    vice_last_left = last $ init left
joltageApproximation from_left@(_:(_:_)) [_] = vice_last_left * 10 + last_left
  where
    last_left = last from_left
    vice_last_left = last $ init from_left
joltageApproximation [_] (first_right:(second_right:_)) = first_right * 10 + second_right
joltageApproximation _ _ = 0


main :: IO ()
main = do
  let example =
        [ "987654321111111",
          "811111111111119",
          "234234234234278",
          "818181911112111"
        ]
  let example_str_matrix = map (recursiveSplitAt [] 1) example
  let example_matrix = map (map (toInteger . read)) example_str_matrix
  let exampleLR = maxFromLeftToRight example_matrix
  let exampleRL = maxFromRightToLeft example_matrix
  -- 1st star
  print "First star example:"
  -- print example_matrix
  -- print $ map nub exampleLR
  -- print $ map nub exampleRL
  -- print $ zip exampleLR (map reverse exampleRL)
  print $ zipWith joltageApproximation (map nub exampleLR) (map nub exampleRL)
  -- print $ countZeroLandings 50 0 example

  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"
  
  let input_str_matrix = map (recursiveSplitAt [] 1) $ lines contents
  let input_matrix = map (map (toInteger . read)) input_str_matrix
  let inputLR = maxFromLeftToRight input_matrix
  let inputRL = maxFromRightToLeft input_matrix
  print $ sum $ zipWith joltageApproximation (map nub inputLR) (map nub inputRL)

  print ""

  -- 2nd star
  print "Second star example:"
  -- print $ countZeroPassings 50 0 example
  -- print $ countZeroPassings' 50 0 example

  -- debugPassings countZeroPassings' [head $ lines contents] $ tail $ lines contents

  print "Second star input:"
  -- print $ countZeroPassings 50 0 $ lines contents
  -- print $ countZeroPassings' 50 0 $ lines contents

-- print ""
