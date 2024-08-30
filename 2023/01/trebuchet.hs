import Data.List (isPrefixOf, isSuffixOf)
import Text.Read (readMaybe)

firstLeftDigit :: String -> String
firstLeftDigit "" = ""
firstLeftDigit (first : rest) = certainlyRead (readMaybe [first]) rest
  where
    certainlyRead :: Maybe Int -> String -> String
    certainlyRead (Just a) _ = show a
    certainlyRead Nothing rest = firstLeftDigit rest

firstRightDigit :: String -> String
firstRightDigit "" = ""
firstRightDigit buffer = certainlyRead (readMaybe [last buffer]) $ init buffer
  where
    certainlyRead :: Maybe Int -> String -> String
    certainlyRead (Just a) _ = show a
    certainlyRead Nothing rest = firstRightDigit rest

firstLeftSpelledDigit :: String -> String
firstLeftSpelledDigit "" = ""
firstLeftSpelledDigit buffer@(first : rest)
  | "one" `isPrefixOf` buffer = "1"
  | "two" `isPrefixOf` buffer = "2"
  | "three" `isPrefixOf` buffer = "3"
  | "four" `isPrefixOf` buffer = "4"
  | "five" `isPrefixOf` buffer = "5"
  | "six" `isPrefixOf` buffer = "6"
  | "seven" `isPrefixOf` buffer = "7"
  | "eight" `isPrefixOf` buffer = "8"
  | "nine" `isPrefixOf` buffer = "9"
  | otherwise = certainlyRead (readMaybe [first]) rest
  where
    certainlyRead :: Maybe Int -> String -> String
    certainlyRead (Just a) _ = show a
    certainlyRead Nothing rest = firstLeftSpelledDigit rest

firstRightSpelledDigit :: String -> String
firstRightSpelledDigit "" = ""
firstRightSpelledDigit buffer
  | "one" `isSuffixOf` buffer = "1"
  | "two" `isSuffixOf` buffer = "2"
  | "three" `isSuffixOf` buffer = "3"
  | "four" `isSuffixOf` buffer = "4"
  | "five" `isSuffixOf` buffer = "5"
  | "six" `isSuffixOf` buffer = "6"
  | "seven" `isSuffixOf` buffer = "7"
  | "eight" `isSuffixOf` buffer = "8"
  | "nine" `isSuffixOf` buffer = "9"
  | otherwise = certainlyRead (readMaybe [last buffer]) $ init buffer
  where
    certainlyRead :: Maybe Int -> String -> String
    certainlyRead (Just a) _ = show a
    certainlyRead Nothing rest = firstRightSpelledDigit rest

main :: IO ()
main = do
  let firstExample =
        [ "1abc2",
          "pqr3stu8vwx",
          "a1b2c3d4e5f",
          "treb7uchet"
        ]

  print "First left digit"
  let testFirstLeft = map firstLeftDigit firstExample
  print testFirstLeft

  print "First right digit"
  let testFirstRight = map firstRightDigit firstExample
  print testFirstRight

  print "Concatenated"
  let testConcatenated = zipWith (++) testFirstLeft testFirstRight
  print testConcatenated

  print "Test added"
  print $ sum $ map read testConcatenated

  contents <- readFile "input.txt"
  let firstLeft = map firstLeftDigit $ lines contents
  let firstRight = map firstRightDigit $ lines contents
  let concatenated = zipWith (++) firstLeft firstRight
  print "Input added"
  print $ sum $ map read concatenated

  let secondExample =
        [ "two1nine",
          "eightwothree",
          "abcone2threexyz",
          "xtwone3four",
          "4nineeightseven2",
          "zoneight234",
          "7pqrstsixteen"
        ]
  print "First left digit"
  let testFirstSpelledLeft = map firstLeftSpelledDigit secondExample
  print testFirstLeft

  print "First right digit"
  let testFirstSpelledRight = map firstRightSpelledDigit secondExample
  print testFirstRight

  print "Concatenated"
  let testSpelledConcatenated = zipWith (++) testFirstSpelledLeft testFirstSpelledRight
  print testSpelledConcatenated

  print "Test added"
  print $ sum $ map read testSpelledConcatenated

  let firstLeft = map firstLeftSpelledDigit $ lines contents
  let firstRight = map firstRightSpelledDigit $ lines contents
  let concatenated = zipWith (++) firstLeft firstRight
  print "Input added"
  print $ sum $ map read concatenated
  