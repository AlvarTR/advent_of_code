import Data.Bifunctor as Bifunctor (Bifunctor (first, second))
import Data.Char (isDigit)
import Data.Map qualified as Map
import Data.Set qualified as Set

rowInfluence :: Int -> String -> ([Set.Set (Int, Int)], [(Int, Set.Set (Int, Int))])
rowInfluence = influences "" Set.empty 0

influences :: String -> Set.Set (Int, Int) -> Int -> Int -> String -> ([Set.Set (Int, Int)], [(Int, Set.Set (Int, Int))])
influences "" _ _ _ "" = ([], [])
influences partialNumber partialSet _ _ "" = ([], [(read partialNumber, partialSet)])
influences "" _ columnNum rowNum ('.' : rest) = influences "" Set.empty (columnNum + 1) rowNum rest
influences partialNumber partialSet columnNum rowNum ('.' : rest) =
  Bifunctor.second ((read partialNumber, partialSet) :) $
    influences "" Set.empty (columnNum + 1) rowNum rest
influences partialNumber partialSet columnNum rowNum (first : rest)
  | isDigit first =
      influences
        (partialNumber ++ [first])
        (Set.insert (rowNum, columnNum) partialSet)
        (columnNum + 1)
        rowNum
        rest
  | otherwise =
      Bifunctor.first (influenceGrid :) $
        influences partialNumber partialSet columnNum rowNum ('.' : rest)
  where
    influenceGrid =
      Set.delete (rowNum, columnNum) $
        Set.fromList $
          zip (concatMap (replicate 3) [rowNum - 1 .. rowNum + 1]) $
            concat $
              replicate 3 [columnNum - 1 .. columnNum + 1]

numbersInfluencedBySymbols :: [[Set.Set (Int, Int)]] -> [[(Int, Set.Set (Int, Int))]] -> [[Int]]
numbersInfluencedBySymbols _ [] = []
numbersInfluencedBySymbols [upSetList, sameLineSetList] [first] = [map fst $ filter (filterOfSetInSecondPlaceOfTuple (upSetList : [sameLineSetList])) first]
numbersInfluencedBySymbols (upSetList : sameLineSetList : belowSetList : otherSetLists) (first : rest) = map fst intersections : numbersInfluencedBySymbols (sameLineSetList : belowSetList : otherSetLists) rest
  where
    intersections = filter (filterOfSetInSecondPlaceOfTuple (upSetList : sameLineSetList : [belowSetList])) first

filterOfSetInSecondPlaceOfTuple :: [[Set.Set (Int, Int)]] -> (Int, Set.Set (Int, Int)) -> Bool
filterOfSetInSecondPlaceOfTuple listOfListsOfSets = not . Set.null . Set.intersection (Set.unions (map Set.unions listOfListsOfSets)) . snd

main :: IO ()
main = do
  let example =
        [ "467..114..",
          "...*......",
          "..35..633.",
          "......#...",
          "617-......",
          ".....+.58.",
          "..592.....",
          "......755.",
          "...$./....",
          ".664.598.."
        ]

  -- print "Example:"
  -- mapM_ print example

  let exampleListOfInfluences = zipWith rowInfluence [0 ..] example
  -- print "Example parsed:"
  mapM_ print exampleListOfInfluences

  let exampleNumbersWithInfluences = uncurry numbersInfluencedBySymbols $ Bifunctor.first ([] :) $ unzip exampleListOfInfluences
  -- print "Example numbers with symbol influence:"
  print exampleNumbersWithInfluences

  print "Example sum"
  print $ sum $ map sum exampleNumbersWithInfluences

  contents <- readFile "input.txt"
  let inputListOfInfluences = zipWith rowInfluence [0 ..] $ lines contents
  let inputNumbersWithInfluences = uncurry numbersInfluencedBySymbols $ Bifunctor.first ([] :) $ unzip inputListOfInfluences

  print "Input numbers with symbol influence:"
  mapM_ print $ zip [0 ..] inputNumbersWithInfluences

  print "Input sum"
  print $ sum $ map sum inputNumbersWithInfluences

  print ""
