import Data.List (isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)

safe_modulus=100

turnInstruction :: Int -> Int -> [String] -> (Int, Int)
turnInstruction state num_zeroes [] = (state, num_zeroes)
turnInstruction state num_zeroes ((direction:num_turns):rest) = turnInstruction new_state new_num_zeroes rest
  where
    new_state = if direction == 'R' 
                then ((+) state $ read num_turns) `mod` safe_modulus
                else ((-) state $ read num_turns) `mod` safe_modulus
    new_num_zeroes = (+) num_zeroes $ if new_state == 0 then 1 else 0

-- listOfTurns :: String -> (Int, Int)


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
    
  print "Example:"
  print $ turnInstruction 50 0 $ example
  print ""

  -- Input text
  contents <- readFile "input.txt"
  print $ turnInstruction 50 0 $ lines contents
  -- print ""
  