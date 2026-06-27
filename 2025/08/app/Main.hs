module Main where

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Data.Ord (comparing)

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
  ( (minX + maxX) `div` 2,
    (minY + maxY) `div` 2,
    (minZ + maxZ) `div` 2
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
  [[Point3D]] ->
  [Point3D] ->
  Point3D ->
  ([[Point3D]], [[Point3D]])
corralledPointsWorker point_storage cut_axis_dist_storage [] _ = (point_storage, cut_axis_dist_storage)
corralledPointsWorker point_storage cut_axis_dist_storage (first@(x, y, z) : rest) cut_point@(xCut, yCut, zCut) = corralledPointsWorker new_point_storage new_cut_axis_dist_storage rest cut_point
  where
    cut_axis_dist = (x - xCut, y - yCut, z - zCut)
    xIndex
      | xCoor cut_axis_dist < 0 = 0
      | otherwise = 4
    yIndex
      | yCoor cut_axis_dist < 0 = 0
      | otherwise = 2
    zIndex
      | zCoor cut_axis_dist < 0 = 0
      | otherwise = 1
    index = sum [xIndex, yIndex, zIndex]
    new_sector = first : (point_storage !! index)
    new_distance_sector = cut_axis_dist : (cut_axis_dist_storage !! index)
    new_point_storage = (++) (take index point_storage) (new_sector : drop (index + 1) point_storage)
    new_cut_axis_dist_storage = (++) (take index cut_axis_dist_storage) (new_distance_sector : drop (index + 1) cut_axis_dist_storage)

corralledPoints ::
  [Point3D] ->
  ([[Point3D]], [[Point3D]])
corralledPoints points = corralledPointsWorker [[], [], [], [], [], [], [], []] [[], [], [], [], [], [], [], []] points $ cutEachAxis points

distance :: Point3D -> Point3D -> Float
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral (xDiff * xDiff + yDiff * yDiff + zDiff * zDiff)
  where
    xDiff = x2 - x1
    yDiff = y2 - y1
    zDiff = z2 - z1

pointListSmallestDistance :: [(Int, Float)] -> [Point3D] -> [Point3D] -> [(Int, Float)]
pointListSmallestDistance storage [] [_] = storage
pointListSmallestDistance storage _ [] = storage
pointListSmallestDistance storage previous_points (first : rest) = pointListSmallestDistance new_storage (previous_points ++ [first]) rest
  where
    lowIndexedDistances = zip [0 ..] $ map (distance first) previous_points
    highIndexedDistances = zip [1 + length previous_points ..] $ map (distance first) rest
    minIndexedDistance = minimumBy (comparing snd) (lowIndexedDistances ++ highIndexedDistances)
    new_storage = storage ++ [minIndexedDistance]

corralboundMinimum :: [[(Int, Float)]] -> (Int, (Int, (Int, Float)))
corralboundMinimum =
  minimumBy (comparing (snd . snd . snd))
    . zip [0 ..]
    . map (minimumBy (comparing (snd . snd)) . zip [0 ..])
    . filter (not . null)

edgyPoints :: [Point3D] -> Float -> [Point3D] -> [Point3D] -> [Point3D]
edgyPoints storage _ [] _ = storage
edgyPoints storage _ _ [] = storage
edgyPoints storage ref_distance (point : rest_points) (dist@(x, y, z) : rest_dist) = edgyPoints new_storage ref_distance rest_points rest_dist
  where
    new_storage
      | all (ref_distance <) [fromIntegral $ abs x, fromIntegral $ abs y, fromIntegral $ abs z] = storage
      | otherwise = point : storage

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
  print "Parsed coords:"
  mapM_ print exampleCoords

  let exampleDistances = pointListSmallestDistance [] [] exampleCoords
  print "Distances:"
  print exampleDistances
  print "Abs min distance:"
  print $ minimumBy (comparing snd) exampleDistances

  print "Cut point:"
  print $ cutEachAxis exampleCoords

  let (exampleCorralled, exampleCorralledDist) = corralledPoints exampleCoords
  print "Corralled:"
  mapM_ print exampleCorralled
  print "Cut distances:"
  mapM_ print exampleCorralledDist

  -- print $ pointListSmallestDistance [] $ head exampleCorralled
  let exampleDistancesInsideCorral = map (pointListSmallestDistance [] []) exampleCorralled
  print "Distances inside corrales:"
  mapM_ print exampleDistancesInsideCorral

  let exampleMin@(exampleCorralIndex, (exampleIndex, (examplePartnerIndex, exampleMinDistance))) = corralboundMinimum exampleDistancesInsideCorral
  print "Min distance of corralled points:"
  print exampleMin

  let exampleEdgyPoints = zipWith (edgyPoints [] exampleMinDistance) exampleCorralled exampleCorralledDist
  print "Edgy points:"
  mapM_ print exampleEdgyPoints

  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"

  let firstStarCoords = map lineToCoor $ lines contents

  let firstStarDistances = pointListSmallestDistance [] [] firstStarCoords
  print "Distances:"
  print firstStarDistances

  print "Abs min distance:"
  print $ minimumBy (comparing snd) firstStarDistances

  -- print "Cut point:"
  -- print $ cutEachAxis firstStarCoords

  let (firstStarCorralled, firstStarCorralledDist) = corralledPoints firstStarCoords
  -- print "Corralled:"
  -- mapM_ print firstStarCorralled
  -- print "Cut distances:"
  -- mapM_ print firstStarCorralledDist

  let firstStarDistancesInsideCorral = map (pointListSmallestDistance [] []) firstStarCorralled
  -- print "Distances inside corrales:"
  -- mapM_ print firstStarDistancesInsideCorral

  -- 2nd star
  print "Second star example:"

  print "Second star input:"
