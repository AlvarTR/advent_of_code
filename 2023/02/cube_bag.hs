module Main where

import Data.Bool (Bool(..))
import Text.Read (readMaybe)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Tuple

isRevealPossible :: Map.Map String Int -> Map.Map String Int -> Bool
isRevealPossible maxMap currentMap = all (isColorValid maxMap currentMap) (Map.keys currentMap)
  where
    resolveMaybeValid :: Maybe Bool -> Bool
    resolveMaybeValid (Just a) = a
    resolveMaybeValid Nothing = False

    isColorValid:: Map.Map String Int -> Map.Map String Int -> String -> Bool
    isColorValid maxMap currentMap key = resolveMaybeValid $ do
      maxColor <- Map.lookup key maxMap
      currentColor <- Map.lookup key currentMap
      return $ maxColor > currentColor

customSplitOn :: String -> String -> [String]
customSplitOn separator string = map Text.unpack $ Text.splitOn (Text.pack separator) $ Text.pack string

parseLine :: [Char] -> Map.Map Int [Map.Map String Int]
parseLine string =
  let gameAndReveals = customSplitOn ": " string
      eachReveal = concatMap (customSplitOn "; ") $ tail gameAndReveals
      eachColor = map (customSplitOn ", ") eachReveal
      perColor = map (\x -> zip (map (last . customSplitOn " ") x) (map (read . head . customSplitOn " ") x::[Int])) eachColor
  in Map.fromList [(read (last $ customSplitOn " " $ head gameAndReveals)::Int, map Map.fromList perColor)]

main :: IO ()
main = do
  let maxColors = Map.fromList [("red", 13), ("green", 14), ("blue", 15)]
  print maxColors

  let firstExample =
        [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
        "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
        "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
        "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
        "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
        ]

  let exampleParsed = foldl1 Map.union $ map parseLine firstExample
  -- print exampleParsed

  let onlyViableExamples = Map.filter (all (isRevealPossible maxColors)) exampleParsed
  -- print onlyViableExamples
  print $ sum $ Map.keys onlyViableExamples

  contents <- readFile "input.txt"
  let inputParsed = foldl1 Map.union $ map parseLine $ lines contents
  let onlyViableInputs = Map.filter (all (isRevealPossible maxColors)) inputParsed
  print $ sum $ Map.keys onlyViableInputs


  print ""
