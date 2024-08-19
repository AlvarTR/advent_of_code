import Text.Read

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

main :: IO ()
main = do
  let x =
        [ "1abc2",
          "pqr3stu8vwx",
          "a1b2c3d4e5f",
          "treb7uchet"]

  print "First left digit"
  let testFirstLeft = map firstLeftDigit x
  print testFirstLeft

  print "First right digit"
  let testFirstRight = map firstRightDigit x
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
  