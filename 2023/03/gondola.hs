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
numbersInfluencedBySymbols [up, sameLine] [first] = [map fst $ filter (filterOfSetInSecondPlaceOfTuple (up : [sameLine])) first]
numbersInfluencedBySymbols (up : sameLine : below : others) (first : rest) = map fst intersections : numbersInfluencedBySymbols (sameLine : below : others) rest
  where
    intersections = filter (filterOfSetInSecondPlaceOfTuple (up : sameLine : [below])) first

filterOfSetInSecondPlaceOfTuple :: [[Set.Set (Int, Int)]] -> (Int, Set.Set (Int, Int)) -> Bool
filterOfSetInSecondPlaceOfTuple listOfListsOfSets = not . Set.null . Set.intersection (Set.unions (map Set.unions listOfListsOfSets)) . snd

rowGearInfluence :: Int -> String -> ([Set.Set (Int, Int)], [(Int, Set.Set (Int, Int))])
rowGearInfluence = gearInfluences "" Set.empty 0

gearInfluences :: String -> Set.Set (Int, Int) -> Int -> Int -> String -> ([Set.Set (Int, Int)], [(Int, Set.Set (Int, Int))])
gearInfluences "" _ _ _ "" = ([], [])
gearInfluences partialNumber partialSet _ _ "" = ([], [(read partialNumber, partialSet)])
gearInfluences "" _ columnNum rowNum ('.' : rest) = gearInfluences "" Set.empty (columnNum + 1) rowNum rest
gearInfluences partialNumber partialSet columnNum rowNum ('.' : rest) =
  Bifunctor.second ((read partialNumber, partialSet) :) $
    gearInfluences "" Set.empty (columnNum + 1) rowNum rest
gearInfluences partialNumber partialSet columnNum rowNum ('*' : rest) =
  Bifunctor.first (influenceGrid :) $
    gearInfluences partialNumber partialSet columnNum rowNum ('.' : rest)
  where
    influenceGrid =
      Set.delete (rowNum, columnNum) $
        Set.fromList $
          zip (concatMap (replicate 3) [rowNum - 1 .. rowNum + 1]) $
            concat $
              replicate 3 [columnNum - 1 .. columnNum + 1]
gearInfluences partialNumber partialSet columnNum rowNum (first : rest)
  | isDigit first =
      gearInfluences
        (partialNumber ++ [first])
        (Set.insert (rowNum, columnNum) partialSet)
        (columnNum + 1)
        rowNum
        rest
  | otherwise = gearInfluences partialNumber partialSet columnNum rowNum ('.' : rest)

numbersAttachedToEachGear :: [[Set.Set (Int, Int)]] -> [[(Int, Set.Set (Int, Int))]] -> [[[Int]]]
numbersAttachedToEachGear [] _ = []
numbersAttachedToEachGear [[]] _ = []
numbersAttachedToEachGear ([] : rest) (up : sameLine : below : others) = numbersAttachedToEachGear rest $ sameLine : below : others
numbersAttachedToEachGear (gears : rest) (up : sameLine : below : others) =
  map (gatherNumbersWithThisGear $ concat (up : sameLine : [below])) gears : numbersAttachedToEachGear rest (sameLine : below : others)

gatherNumbersWithThisGear :: [(Int, Set.Set (Int, Int))] -> Set.Set (Int, Int) -> [Int]
gatherNumbersWithThisGear numbers gear = map fst $ filter (not . Set.null . Set.intersection gear . snd) numbers

main :: IO ()
main = do
  let example =
        [ "467..114..",
          "...*......",
          "..35..633.",
          "......#...",
          "617*......",
          ".....+.58.",
          "..592.....",
          "......755.",
          "...$.*....",
          ".664.598.."
        ]

  -- print "Example:"
  -- mapM_ print example

  let exampleListOfInfluences = zipWith rowInfluence [0 ..] example
  -- print "Example parsed:"
  -- mapM_ print exampleListOfInfluences

  let exampleNumbersWithInfluences = uncurry numbersInfluencedBySymbols $ Bifunctor.first ([] :) $ unzip exampleListOfInfluences
  -- print "Example numbers with symbol influence:"
  -- print exampleNumbersWithInfluences

  -- print "Example sum"
  -- print $ sum $ map sum exampleNumbersWithInfluences

  contents <- readFile "input.txt"
  let inputListOfInfluences = zipWith rowInfluence [0 ..] $ lines contents
  let inputNumbersWithInfluences = uncurry numbersInfluencedBySymbols $ Bifunctor.first ([] :) $ unzip inputListOfInfluences

  -- print "Input numbers with symbol influence:"
  -- mapM_ print $ zip [0 ..] inputNumbersWithInfluences

  -- print "Input sum"
  -- print $ sum $ map sum inputNumbersWithInfluences

  let exampleListOfGearInfluences = zipWith rowGearInfluence [0 ..] example
  -- print "Example parsed for gears:"
  -- mapM_ print exampleListOfGearInfluences

  let exampleNumbersWithGears = uncurry numbersAttachedToEachGear $ Bifunctor.second ([] :) $ unzip exampleListOfGearInfluences
  -- print "Example numbers with gear influence:"
  -- print exampleNumbersWithGears

  let exampleGearsWithOnlyTwoNumbers = concatMap (filter ((==) 2 . length)) exampleNumbersWithGears
  -- print "Example gears with only two numbers:"
  -- print exampleGearsWithOnlyTwoNumbers

  let exampleSumOfGearRatios = sum $ map product exampleGearsWithOnlyTwoNumbers
  -- print "Example sum of gear ratios:"
  -- print exampleSumOfGearRatios

  let inputNumbersWithGears = uncurry numbersAttachedToEachGear $ Bifunctor.second ([] :) $ unzip inputListOfInfluences

  let inputGearsWithOnlyTwoNumbers = concatMap (filter ((==) 2 . length)) inputNumbersWithGears
  -- print "Input gears with only two numbers:"
  -- print inputGearsWithOnlyTwoNumbers

  let inputSumOfGearRatios = sum $ map product inputGearsWithOnlyTwoNumbers
  print "Input sum of gear ratios:"
  print inputSumOfGearRatios

  print ""
