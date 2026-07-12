module Main where

import Data.List (sortBy, maximumBy)
import Data.List.Split (splitOn)
import Data.Ord (compare, comparing)

type Point2D = (Int, Int)


listToPoint [x, y] = (x, y)


lineToCoor :: String -> Point2D
lineToCoor = listToPoint . map read . splitOn ","


square :: (Num a) => a -> a
square n = n * n

distance :: Point2D -> Point2D -> Integer
distance (x1, y1) (x2, y2) = sum $ map (square . toInteger) $ zipWith (-) [x2, y2] [x1, y1]

pointListDistances :: [Point2D] -> [Point2D] -> [[(Int, Integer)]]
pointListDistances _ [] = []
pointListDistances previous_points (first : rest) = sortedDistances : pointListDistances (previous_points ++ [first]) rest
  where
    lowIndexedDistances = zip [0 ..] $ map (distance first) previous_points
    highIndexedDistances = zip [1 + length previous_points ..] $ map (distance first) rest
    sortedDistances = sortBy (comparing snd) (lowIndexedDistances ++ highIndexedDistances)

sortDistances :: [[(Int, Integer)]] -> [(Int, (Int, Integer))]
sortDistances = sortBy (comparing (negate . snd . snd)) . concat . zipWith (zip . repeat) [0 ..]

rectangleArea :: [Point2D] -> (Int, (Int, Integer)) -> (Int, (Int, Integer))
rectangleArea [] (index1, (index2, _)) = (index1, (index2, 0)) 
rectangleArea db (index1, (index2, _)) = (index1, (index2, area))
  where
    (point1_x, point1_y) = db !! index1
    (point2_x, point2_y) = db !! index2
    area = abs $ product $ map toInteger $ zipWith (-) [point1_x+1, point1_y+1] [point2_x, point2_y]

main :: IO ()
main = do
  let example =
        [ "7,1",
          "11,1",
          "11,7",
          "9,7",
          "9,5",
          "2,5",
          "2,3",
          "7,3"
        ]
  -- 1st star
  print "First star example:"
  let example_points = map lineToCoor example
  -- let example_sorted_points = sortBy (comparing fst) $ sortBy (comparing snd) example_points
  -- mapM_ print example_points
  let example_distances = sortDistances $ pointListDistances [] example_points
  -- print example_distances

  let example_rectangle_areas = map (rectangleArea example_points) example_distances
  print $ take 10 example_rectangle_areas
  print $ maximumBy (comparing (snd . snd)) example_rectangle_areas



  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"
  let first_star_points = map lineToCoor $ lines contents
  -- print first_star_points
  let first_star_distances = sortDistances $ pointListDistances [] first_star_points
  let first_star_rectangle_areas = map (rectangleArea first_star_points) first_star_distances
  print $ maximumBy (comparing (snd . snd)) first_star_rectangle_areas



  -- 2nd star
  print "Second star example:"

  print "Second star input:"
