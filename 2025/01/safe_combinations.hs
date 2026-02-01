import GHC.Natural (Natural, naturalFromInteger)
import GHC.Num (integerFromNatural)

safeModulus = 100

countZeroLandings :: Integer -> Natural -> [String] -> (Integer, Natural)
countZeroLandings state num_zeroes [] = (state, num_zeroes)
countZeroLandings state num_zeroes ((direction : num_turns) : rest) = countZeroLandings new_state new_num_zeroes rest
  where
    new_state = flip mod safeModulus $ (if direction == 'R' then (+) else (-)) state $ read num_turns
    new_num_zeroes = (+) num_zeroes $ if new_state == 0 then 1 else 0

countZeroPassings :: Integer -> Natural -> [String] -> (Integer, Natural)
countZeroPassings state num_zeroes [] = (state, num_zeroes)
countZeroPassings state num_zeroes ((direction : turns_str) : rest)
  | turns == 0 = countZeroPassings state num_zeroes rest
  | turns >= safeModulus = countZeroPassings state ((+) num_zeroes $ naturalFromInteger adding_zeroes) $ (direction : show summarized_turns) : rest
  | turns_and_state == 0 = countZeroPassings 0 (num_zeroes + 1) rest
  | direction == 'R' && turns_and_state < safeModulus = countZeroPassings turns_and_state num_zeroes rest
  | direction == 'R' && turns_and_state >= safeModulus = countZeroPassings (turns_and_state - safeModulus) (num_zeroes + 1) rest
  -- direction == 'L' && state > turns
  | direction == 'L' && turns_and_state < 0 = countZeroPassings (abs turns_and_state) num_zeroes rest
  | direction == 'L' && state == 0 = countZeroPassings new_state num_zeroes rest
  | direction == 'L' && turns_and_state < safeModulus = countZeroPassings new_state (num_zeroes + 1) rest
  where
    direction_sign = if direction == 'R' then (+) else (-)
    turns = read turns_str
    turns_and_state = direction_sign turns state
    new_state = flip mod safeModulus $ direction_sign state turns
    (adding_zeroes, summarized_turns) = turns `divMod` safeModulus

debugPassings :: (Integer -> Natural -> [String] -> (Integer, Natural)) -> [String] -> [String] -> IO ()
debugPassings function storage [] = do
  print (length storage, last storage, function 50 0 storage)
debugPassings function storage (first : rest) = do
  print (length storage, last storage, function 50 0 storage)
  debugPassings function (storage ++ [first]) rest

countZeroPassings' :: Integer -> Natural -> [String] -> (Integer, Natural)
countZeroPassings' state num_zeroes [] = (state, num_zeroes)
countZeroPassings' state num_zeroes ((direction : turns_str) : rest)
  | turns == 0 = countZeroPassings' state num_zeroes rest
  | turns >= safeModulus = countZeroPassings' state ((+) num_zeroes $ naturalFromInteger adding_zeroes) $ (direction : show summarized_turns) : rest
  | otherwise = countZeroPassings' new_state new_num_zeroes rest
  where
    turns = read turns_str
    (adding_zeroes, summarized_turns) = turns `divMod` safeModulus
    state_steps = map (flip mod safeModulus . (if direction == 'R' then (+) else (-)) state) [1 .. turns]
    new_state = last state_steps
    new_num_zeroes = (+) num_zeroes $ (naturalFromInteger . toInteger . length . filter (0 ==)) state_steps

main :: IO ()
main = do
  let example =
        [ "L68",
          "L30",
          "R48",
          "L5",
          "R60",
          "L55",
          "L1",
          "L99",
          "R14",
          "L82"
        ]

  -- 1st star
  print "First star example:"
  print $ countZeroLandings 50 0 example

  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"
  print $ countZeroLandings 50 0 $ lines contents

  print ""

  -- 2nd star
  print "Second star example:"
  print $ countZeroPassings 50 0 example
  print $ countZeroPassings' 50 0 example

  -- debugPassings countZeroPassings' [head $ lines contents] $ tail $ lines contents

  print "Second star input:"
  print $ countZeroPassings 50 0 $ lines contents
  print $ countZeroPassings' 50 0 $ lines contents

-- print ""
