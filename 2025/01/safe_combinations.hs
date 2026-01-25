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
countZeroPassings state num_zeroes ((direction : num_turns) : rest) = countZeroPassings new_state new_num_zeroes rest
  where
    turns = read num_turns
    new_unmod_state = (if direction == 'R' then (+) else (-)) state turns
    (adding_num_zeroes, new_state) = new_unmod_state `divMod` safeModulus
    new_num_zeroes = (+) num_zeroes $ naturalFromInteger $ (+) adding_num_zeroes $ if (state == 0) && (new_state /= 0) then 1 else 0

debugPassings :: [String] -> [String] -> IO ()
debugPassings storage [] = do
  print $ length storage
  print $ last storage
  print $ countZeroPassings 50 0 storage
debugPassings storage (first : rest) = do
  print $ length storage
  print $ last storage
  print $ countZeroPassings 50 0 storage
  print ""
  debugPassings (storage ++ [first]) rest

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

  debugPassings [head $ lines contents] $ tail $ lines contents

  print "Second star input:"
  print $ countZeroPassings 50 0 $ lines contents

-- print ""
