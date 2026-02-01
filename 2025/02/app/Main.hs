module Main where

import Data.List.Split (splitOn)
import GHC.Natural (Natural, naturalFromInteger)

invalidID :: String -> Natural
invalidID "" = 0
invalidID candidateID
  | odd $ length candidateID = 0
  | uncurry (==) halves = read candidateID
  | otherwise = 0
  where
    halves = splitAt (length candidateID `div` 2) candidateID

invalidIDInterval :: [String] -> Natural
invalidIDInterval [] = 0
invalidIDInterval [_] = 0
invalidIDInterval (_ : buffer@(_ : (_ : _))) = invalidIDInterval buffer
invalidIDInterval [interval_start, interval_stop] = invNumSum
  where
    start_nat = (naturalFromInteger . read) interval_start
    stop_nat = (naturalFromInteger . read) interval_stop
    invNumSum = sum $ map (invalidID . show) [start_nat .. stop_nat]

recursiveSplitAt :: [String] -> Int -> String -> [String]
recursiveSplitAt string_list _ "" = string_list
recursiveSplitAt string_list split_index str = recursiveSplitAt (string_list ++ [split_prefix]) split_index rest
  where
    (split_prefix, rest) = splitAt split_index str

invalidPatternID :: String -> String -> Bool
invalidPatternID _ [] = False
invalidPatternID prefix candidateID
  | is_prefix_a_divisor = all (== prefix) $ recursiveSplitAt [] prefix_length candidateID
  | otherwise = False
  where
    prefix_length = length prefix
    is_prefix_a_divisor = (length candidateID `mod` prefix_length) == 0

invalidArbitraryID :: String -> String -> Natural
invalidArbitraryID _ "" = 0
invalidArbitraryID prefix buffer@(first : rest)
  | invalidPattern = naturalFromInteger $ read $ prefix ++ buffer
  | otherwise = invalidArbitraryID new_prefix rest
  where
    new_prefix = prefix ++ [first]
    invalidPattern = invalidPatternID new_prefix rest

invalidIDInterval' :: [String] -> Natural
invalidIDInterval' [] = 0
invalidIDInterval' [_] = 0
invalidIDInterval' (_ : buffer@(_ : (_ : _))) = invalidIDInterval' buffer
invalidIDInterval' [interval_start, interval_stop] = invNumSum
  where
    start_nat = (naturalFromInteger . read) interval_start
    stop_nat = (naturalFromInteger . read) interval_stop
    invNumSum = sum $ map (invalidArbitraryID [] . show) [start_nat .. stop_nat]

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
  print $ sum $ map (invalidIDInterval' . splitOn "-") example

  -- debugPassings [head $ lines contents] $ tail $ lines contents

  print "Second star input:"

  print $ sum $ map (invalidIDInterval' . splitOn "-") $ splitOn "," contents