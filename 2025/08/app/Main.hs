module Main where

import Data.List.Split (splitOn)
import Data.List (minimumBy)
import Data.Ord (comparing)


-- import Data.IntMap.Lazy qualified as IntMap
-- import Data.Text qualified as Text

type Point3D = (Int, Int, Int)

lineToCoor :: String -> Point3D
lineToCoor string = (x, y, z)
  where
    [x, y, z] = map read $ splitOn "," string

xCoor (x, _, _) = x

yCoor (_, y, _) = y

zCoor (_, _, z) = z

cutEachAxis :: [Point3D] -> Point3D
cutEachAxis point_list =
  ( minX + maxX `div` 2,
    minY + maxY `div` 2,
    minZ + maxZ `div` 2
  )
  where
    minX = minimum $ map xCoor point_list
    maxX = maximum $ map xCoor point_list
    minY = minimum $ map yCoor point_list
    maxY = maximum $ map yCoor point_list
    minZ = minimum $ map zCoor point_list
    maxZ = maximum $ map zCoor point_list

corralledPointsWorker ::
  [[Point3D]] ->
  [Point3D] ->
  Point3D ->
  [[Point3D]]
corralledPointsWorker storage [] _ = storage
corralledPointsWorker storage (first@(x, y, z) : rest) cut_point@(xCut, yCut, zCut) = corralledPointsWorker new_storage rest cut_point
  where
    xIndex 
      | x < xCut = 0 
      | otherwise = 4
    yIndex 
      | y < yCut = 0 
      | otherwise = 2
    zIndex 
      | z < zCut = 0 
      | otherwise = 1
    index = xIndex + yIndex + zIndex
    new_sector = first : (storage !! index)
    new_storage = take (index - 1) storage ++ (new_sector : drop index storage)

-- storage = ((storage !! xIndex) !! yIndex) !! zIndex = new_sector

corralledPoints ::
  [Point3D] ->
  [[Point3D]]
corralledPoints points = corralledPointsWorker [[], [], [], [], [], [], [], []] points $ cutEachAxis points

distance :: Point3D -> Point3D -> Float
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral (xDiff*xDiff + yDiff*yDiff + zDiff*zDiff)
  where
    xDiff = x2 - x1
    yDiff = y2 - y1
    zDiff = z2 - z1

smallestDistancesInsideCorral :: [(Int, Float)] -> [Point3D] -> [Point3D] -> [(Int, Float)]
smallestDistancesInsideCorral storage [] [_] = storage
smallestDistancesInsideCorral storage _ [] = storage
smallestDistancesInsideCorral storage previous_points (first : rest) = smallestDistancesInsideCorral new_storage (previous_points ++ [first]) rest
  where
    lowIndexedDistances = zip [0..] $ map (distance first) previous_points
    highIndexedDistances = zip [1 + length previous_points..] $ map (distance first) rest
    minIndexedDistance = minimumBy (comparing snd) (lowIndexedDistances ++ highIndexedDistances)
    new_storage = storage ++ [minIndexedDistance]


main :: IO ()
main = do
  let example =
        [ "162,817,812",
          "57,618,57",
          "906,360,560",
          "592,479,940",
          "352,342,300",
          "466,668,158",
          "542,29,236",
          "431,825,988",
          "739,650,466",
          "52,470,668",
          "216,146,977",
          "819,987,18",
          "117,168,530",
          "805,96,715",
          "346,949,466",
          "970,615,88",
          "941,993,340",
          "862,61,35",
          "984,92,344",
          "425,690,689"
        ]
  let example_cables = 100
  -- 1st star
  print "First star example:"
  let exampleCoords = map lineToCoor example
  -- mapM_ print exampleCoords
  print $ cutEachAxis exampleCoords
  let exampleCorralled = corralledPoints exampleCoords
  mapM_ print exampleCorralled

  -- print $ smallestDistancesInsideCorral [] $ head exampleCorralled
  mapM_ (print . smallestDistancesInsideCorral [] []) exampleCorralled

  -- Input text
  contents <- readFile "input.txt"
  let firstStarCoords = map lineToCoor $ lines contents

  print "First star input:"

  -- 2nd star
  print "Second star example:"

  print "Second star input:"
