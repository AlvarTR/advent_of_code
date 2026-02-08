enoughRollSpace :: Int = 4

recursiveSplitAt :: [String] -> Int -> String -> [String]
recursiveSplitAt string_list _ "" = string_list
recursiveSplitAt string_list split_index str = recursiveSplitAt (string_list ++ [split_prefix]) split_index rest
  where
    (split_prefix, rest) = splitAt split_index str

rollMapParser :: String -> Int
rollMapParser "." = 0
rollMapParser "@" = 1

feedForkliftRows :: [[Int]] -> Int -> Int
feedForkliftRows input_matrix index = forkliftRollsInterface high middle low
  where
    high = if (index - 1) >= 0 && (index - 1) < length (input_matrix !! 1) then input_matrix !! (index - 1) else []
    middle = if index >= 0 && index < length (input_matrix !! 1) then input_matrix !! index else []
    low = if (index + 1) < length (input_matrix !! 1) then input_matrix !! (index + 1) else []

forkliftRolls :: Int -> Bool -> [Int] -> [Int] -> [Int] -> Int
forkliftRolls accum _ [] [] [] = accum
forkliftRolls accum _ [] [] _ = accum
forkliftRolls accum _ [] _ [] = accum
forkliftRolls accum _ _ [] [] = accum
forkliftRolls accum isFirstColumn high middle [] = forkliftRolls accum isFirstColumn [] middle high
forkliftRolls accum _ [] [_] [_] = accum
forkliftRolls accum _ [_] [_] [_] = accum
forkliftRolls accum False [] (middle_left : [middle_middle]) (low_left : [low_middle]) = accum + middle_middle
forkliftRolls accum False (high_left : [high_middle]) (middle_left : [middle_middle]) (low_left : [low_middle]) = new_accum
  where
    roll_count_high = high_left + high_middle
    roll_count_middle = middle_left
    roll_count_low = low_left + low_middle
    roll_count = roll_count_high + roll_count_middle + roll_count_low
    new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
forkliftRolls accum True [] middle@(middle_middle : middle_right : _) low@(low_middle : low_right : _) = forkliftRolls (accum + middle_middle) False [] middle low
forkliftRolls accum True high@(high_middle : high_right : _) middle@(middle_middle : middle_right : _) low@(low_middle : low_right : _) = forkliftRolls new_accum False high middle low
  where
    roll_count_high = high_middle + high_right
    roll_count_middle = middle_right
    roll_count_low = low_middle + low_right
    roll_count = roll_count_high + roll_count_middle + roll_count_low
    new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
forkliftRolls accum False [] (middle_left : middle_tail@(middle_middle : middle_right : middle_rest)) (low_left : low_tail@(low_middle : low_right : low_rest)) = forkliftRolls new_accum False [] middle_tail low_tail
  where
    roll_count_middle = middle_left + middle_right
    roll_count_low = low_left + low_middle + low_right
    roll_count = roll_count_middle + roll_count_low
    new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
forkliftRolls accum False (high_left : high_tail@(high_middle : high_right : high_rest)) (middle_left : middle_tail@(middle_middle : middle_right : middle_rest)) (low_left : low_tail@(low_middle : low_right : low_rest)) = forkliftRolls new_accum False high_tail middle_tail low_tail
  where
    roll_count_high = high_left + high_middle + high_right
    roll_count_middle = middle_left + middle_right
    roll_count_low = low_left + low_middle + low_right
    roll_count = roll_count_high + roll_count_middle + roll_count_low
    new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0

forkliftRollsInterface :: [Int] -> [Int] -> [Int] -> Int
forkliftRollsInterface = forkliftRolls 0 True

main :: IO ()
main = do
  let example =
        [ "..@@.@@@@.",
          "@@@.@.@.@@",
          "@@@@@.@.@@",
          "@.@@@@..@.",
          "@@.@@@@.@@",
          ".@@@@@@@.@",
          ".@.@.@.@@@",
          "@.@@@.@@@@",
          ".@@@@@@@@.",
          "@.@.@@@.@."
        ]
  -- 1st star
  print "First star example:"
  let example_str_matrix = map (recursiveSplitAt [] 1) example
  let example_matrix = map (map rollMapParser) example_str_matrix
  let example_row_len = length (example_matrix !! 1)
  -- mapM_ print example_matrix
  mapM_ print $ zip example $ map (feedForkliftRows example_matrix) [0 .. example_row_len - 1]
  print $ sum $ map (feedForkliftRows example_matrix) [0 .. example_row_len - 1]
  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"
  
  let input_str_matrix = map (recursiveSplitAt [] 1) $ lines contents
  let input_matrix = map (map rollMapParser) input_str_matrix
  let input_row_len = length (input_matrix !! 1)
  mapM_ print $ zip (lines contents) $ map (feedForkliftRows input_matrix) [0 .. input_row_len - 1]
  print $ sum $ map (feedForkliftRows input_matrix) [0 .. input_row_len - 1]

  -- 2nd star
  print "Second star example:"

  print "Second star input:"
