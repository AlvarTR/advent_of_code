import Data.Bifunctor as Bifunctor
import Data.Map qualified as Map
import Data.Set qualified as Set

rowInfluence :: Int -> String -> (Set.Set (Int, Int), Map.Map Int (Set.Set (Int, Int)))
rowInfluence = influences "" Set.empty 0

influences :: String -> Set.Set (Int, Int) -> Int -> Int -> String -> (Set.Set (Int, Int), Map.Map Int (Set.Set (Int, Int)))
influences "" _ _ _ "" = (Set.empty, Map.empty)
influences "" _ columnNum rowNum ('.':rest) = influences "" Set.empty (columnNum+1) rowNum rest
influences partialNumber partialSet _ _ "" = (Set.empty, Map.singleton (read partialNumber) partialSet)
influences partialNumber partialSet columnNum rowNum ('.':rest) =
  Bifunctor.second (Map.union (Map.singleton (read partialNumber) partialSet)) keepProcessing
  where
    keepProcessing = influences "" Set.empty (columnNum+1) rowNum rest
influences partialNumber partialSet columnNum rowNum (first:rest)
  | first `Set.member` Set.fromList "0123456789" =
    influences (partialNumber ++ [first])
    (Set.union partialSet $ Set.singleton (rowNum, columnNum))
    (columnNum+1) rowNum rest
  | otherwise =
    Bifunctor.first (Set.union influenceGrid)
    $ influences partialNumber partialSet columnNum rowNum ('.':rest)
  where
    influenceGrid = Set.delete (rowNum, columnNum)
      $ Set.fromList
      $ zip (concatMap (replicate 3) [rowNum-1 .. rowNum+1])
      $ concat $ replicate 3 [columnNum-1 .. columnNum+1]

numbersInfluencedBySymbols :: [Set.Set (Int, Int)] -> [Map.Map Int (Set.Set (Int, Int))] -> [Int]
numbersInfluencedBySymbols _ [] = []
numbersInfluencedBySymbols [upSet, sameLineSet] [first] =
  Map.keys (Map.filter (not . Set.null . Set.intersection (Set.unions (upSet:[sameLineSet]))) first)
  
numbersInfluencedBySymbols (upSet:sameLineSet:belowSet:otherSets) (first:rest) =
  Map.keys (Map.filter (not . Set.null . Set.intersection (Set.unions (upSet:sameLineSet:[belowSet]))) first) ++ 
  numbersInfluencedBySymbols (sameLineSet:belowSet:otherSets) rest

main :: IO ()
main = do
  let example =
        [ "467..114..",
          "...*......",
          "..35..633.",
          "......#...",
          "617-......",
          ".....+.58.",
          "..592...83",
          "......755.",
          "...$./....",
          ".664.598.."
        ]

  print "Example:"
  mapM_ print example

  let exampleListOfInfluences = zipWith rowInfluence [0 ..] example
  print "Example parsed:"
  mapM_ print exampleListOfInfluences

  let exampleNumbersWithInfluences = uncurry numbersInfluencedBySymbols $ Bifunctor.first (Set.empty:) $ unzip exampleListOfInfluences
  print "Example numbers with symbol influence:"
  print exampleNumbersWithInfluences

  print "Example sum"
  print $ sum exampleNumbersWithInfluences

  contents <- readFile "input.txt"
  let inputListOfInfluences = zipWith rowInfluence [0 ..] $ lines contents
  let inputNumbersWithInfluences = uncurry numbersInfluencedBySymbols $ Bifunctor.first (Set.empty:) $ unzip inputListOfInfluences

  print "Input numbers with symbol influence:"
  print inputNumbersWithInfluences

  print "Input sum"
  print $ sum inputNumbersWithInfluences

  print ""
