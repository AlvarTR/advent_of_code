import Data.List.Split (splitOn)
import GHC.Natural (Natural, naturalFromInteger)

invalidID :: String -> Natural
invalidID id
  | odd $ length id = 0
  | uncurry (==) halves = read id
  | otherwise = 0
  where
    halves = splitAt (length id `div` 2) id

invalidIDInterval :: [String] -> Natural
invalidIDInterval [interval_start, interval_stop] = invNumSum
  where
    start_nat = (naturalFromInteger . read) interval_start
    stop_nat = (naturalFromInteger . read) interval_stop
    invNumSum = sum $ map (invalidID . show) [start_nat .. stop_nat]

main :: IO ()
main = do
  let example = splitOn "," "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

  -- 1st star
  print "First star example:"
  print $ sum $ map (invalidIDInterval . splitOn "-") example

  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"
  print $ sum $ map (invalidIDInterval . splitOn "-") $ splitOn "," contents

  print ""

  -- 2nd star
  print "Second star example:"
  -- print $ countZeroPassings' 50 0 example

  -- debugPassings [head $ lines contents] $ tail $ lines contents

  print "Second star input:"

-- print $ countZeroPassings' 50 0 $ lines contents