enoughRollSpace :: Int = 4

recursiveSplitAt :: [String] -> Int -> String -> [String]
recursiveSplitAt string_list _ "" = string_list
recursiveSplitAt string_list split_index str = recursiveSplitAt (string_list ++ [split_prefix]) split_index rest
  where
    (split_prefix, rest) = splitAt split_index str

rollMapParser :: String -> Int
rollMapParser "." = 0
rollMapParser "@" = 1

-- 1st star
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
forkliftRolls
  accum
  True
  high@(high_middle : high_right : _)
  middle@(middle_middle : middle_right : _)
  low@(low_middle : low_right : _) = forkliftRolls new_accum False high middle low
    where
      roll_count_high = high_middle + high_right
      roll_count_middle = middle_right
      roll_count_low = low_middle + low_right
      roll_count = roll_count_high + roll_count_middle + roll_count_low
      new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
forkliftRolls
  accum
  False
  []
  (middle_left : middle_tail@(middle_middle : middle_right : middle_rest))
  (low_left : low_tail@(low_middle : low_right : low_rest)) = forkliftRolls new_accum False [] middle_tail low_tail
    where
      roll_count_middle = middle_left + middle_right
      roll_count_low = low_left + low_middle + low_right
      roll_count = roll_count_middle + roll_count_low
      new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
forkliftRolls
  accum
  False
  (high_left : high_tail@(high_middle : high_right : high_rest))
  (middle_left : middle_tail@(middle_middle : middle_right : middle_rest))
  (low_left : low_tail@(low_middle : low_right : low_rest)) = forkliftRolls new_accum False high_tail middle_tail low_tail
    where
      roll_count_high = high_left + high_middle + high_right
      roll_count_middle = middle_left + middle_right
      roll_count_low = low_left + low_middle + low_right
      roll_count = roll_count_high + roll_count_middle + roll_count_low
      new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0

forkliftRollsInterface :: [Int] -> [Int] -> [Int] -> Int
forkliftRollsInterface = forkliftRolls 0 True

feedForkliftRows :: [[Int]] -> Int -> Int
feedForkliftRows input_matrix index = forkliftRollsInterface high middle low
  where
    high = if (index - 1) >= 0 && (index - 1) < length (input_matrix !! 1) then input_matrix !! (index - 1) else []
    middle = if index >= 0 && index < length (input_matrix !! 1) then input_matrix !! index else []
    low = if (index + 1) < length (input_matrix !! 1) then input_matrix !! (index + 1) else []

-- 2nd star
disappearingForkliftRolls :: Int -> Bool -> [Int] -> [Int] -> [Int] -> [Int] -> (Int, [Int])
disappearingForkliftRolls accum _ middle' [] [] [] = (accum, middle')
disappearingForkliftRolls accum _ middle' [] [] _ = (accum, middle')
disappearingForkliftRolls accum _ middle' [] _ [] = (accum, middle')
disappearingForkliftRolls accum _ middle' _ [] [] = (accum, middle')
disappearingForkliftRolls accum isFirstColumn middle' high middle [] =
  disappearingForkliftRolls
    accum
    isFirstColumn
    middle'
    []
    middle
    high
disappearingForkliftRolls accum _ middle' [] [_] [_] = (accum, middle')
disappearingForkliftRolls accum _ middle' [_] [_] [_] = (accum, middle')
disappearingForkliftRolls
  accum
  False
  middle'
  []
  (middle_left : [middle_middle])
  (low_left : [low_middle]) = (accum + middle_middle, middle' ++ [0])
disappearingForkliftRolls
  accum
  False
  middle'
  (high_left : [high_middle])
  (middle_left : [middle_middle])
  (low_left : [low_middle]) = (new_accum, mod_middle)
    where
      roll_count_high = high_left + high_middle
      roll_count_middle = middle_left
      roll_count_low = low_left + low_middle
      roll_count = roll_count_high + roll_count_middle + roll_count_low
      new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
      mod_middle = middle' ++ [if roll_count < enoughRollSpace then 0 else middle_middle]
disappearingForkliftRolls
  accum
  True
  middle'
  []
  middle@(middle_middle : middle_right : _)
  low@(low_middle : low_right : _) = disappearingForkliftRolls new_accum False mod_middle [] middle low
    where
      new_accum = accum + middle_middle
      mod_middle = middle' ++ [0]
disappearingForkliftRolls
  accum
  True
  middle'
  high@(high_middle : high_right : _)
  middle@(middle_middle : middle_right : _)
  low@(low_middle : low_right : _) = disappearingForkliftRolls new_accum False mod_middle high middle low
    where
      roll_count_high = high_middle + high_right
      roll_count_middle = middle_right
      roll_count_low = low_middle + low_right
      roll_count = roll_count_high + roll_count_middle + roll_count_low
      new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
      mod_middle = middle' ++ [if roll_count < enoughRollSpace then 0 else middle_middle]
disappearingForkliftRolls
  accum
  False
  middle'
  []
  (middle_left : middle_tail@(middle_middle : middle_right : middle_rest))
  (low_left : low_tail@(low_middle : low_right : low_rest)) =
    disappearingForkliftRolls
      new_accum
      False
      mod_middle
      []
      middle_tail
      low_tail
    where
      roll_count_middle = middle_left + middle_right
      roll_count_low = low_left + low_middle + low_right
      roll_count = roll_count_middle + roll_count_low
      new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
      mod_middle = middle' ++ [if roll_count < enoughRollSpace then 0 else middle_middle]
disappearingForkliftRolls
  accum
  False
  middle'
  (high_left : high_tail@(high_middle : high_right : high_rest))
  (middle_left : middle_tail@(middle_middle : middle_right : middle_rest))
  (low_left : low_tail@(low_middle : low_right : low_rest)) =
    disappearingForkliftRolls
      new_accum
      False
      mod_middle
      high_tail
      middle_tail
      low_tail
    where
      roll_count_high = high_left + high_middle + high_right
      roll_count_middle = middle_left + middle_right
      roll_count_low = low_left + low_middle + low_right
      roll_count = roll_count_high + roll_count_middle + roll_count_low
      new_accum = accum + if roll_count < enoughRollSpace then middle_middle else 0
      mod_middle = middle' ++ [if roll_count < enoughRollSpace then 0 else middle_middle]

disappearingForkliftRollsInterface :: [Int] -> [Int] -> [Int] -> (Int, [Int])
disappearingForkliftRollsInterface = disappearingForkliftRolls 0 True []

feedDisappearingForkliftRows :: [[Int]] -> Int -> (Int, [Int])
feedDisappearingForkliftRows input_matrix index = disappearingForkliftRollsInterface high middle low
  where
    high = if (index - 1) >= 0 && (index - 1) < length (input_matrix !! 1) then input_matrix !! (index - 1) else []
    middle = if index >= 0 && index < length (input_matrix !! 1) then input_matrix !! index else []
    low = if (index + 1) < length (input_matrix !! 1) then input_matrix !! (index + 1) else []

loopUntilNoDisappearances :: Int -> [[Int]] -> (Int, [[Int]])
loopUntilNoDisappearances previous_accum matrix
  | num_changes > 0 = loopUntilNoDisappearances (previous_accum + num_changes) new_matrix
  | otherwise = (previous_accum, new_matrix)
  where
    matrix_row_len = length (matrix !! 1)
    (num_changes_per_row, new_matrix) = unzip $ map (feedDisappearingForkliftRows matrix) [0 .. matrix_row_len - 1]
    num_changes = sum num_changes_per_row

loopUntilNoDisappearancesInterface :: [[Int]] -> Int
loopUntilNoDisappearancesInterface = fst . loopUntilNoDisappearances 0

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
  -- mapM_ print $ zip (lines contents) $ map (feedForkliftRows input_matrix) [0 .. input_row_len - 1]
  print $ sum $ map (feedForkliftRows input_matrix) [0 .. input_row_len - 1]

  -- 2nd star
  print "Second star example:"
  print $ loopUntilNoDisappearancesInterface example_matrix

  print "Second star input:"
  print $ loopUntilNoDisappearancesInterface input_matrix
