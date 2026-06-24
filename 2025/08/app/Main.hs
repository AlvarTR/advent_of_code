module Main where

import Data.List.Split (splitOn)
import qualified Data.Text as Text

type Point3D = (Int, Int, Int)
cutEachAxis :: [Point3D] -> (Point3D, Point3D, Point3D)
cutEachAxis point_list = ((minX,minY,minZ),(minX + maxX `div` 2,minY + maxY  `div` 2,minZ + maxZ  `div` 2),(maxX,maxY,maxZ))
  where
    first (x,_,_) = x
    second (_,y,_) = y
    third (_,_,z) = z
    minX = minimum $ map first point_list
    maxX = maximum $ map first point_list
    minY = minimum $ map second point_list
    maxY = maximum $ map second point_list
    minZ = minimum $ map third point_list
    maxZ = maximum $ map third point_list


lineToCoor :: String -> Point3D
lineToCoor string = (x, y, z)
  where
    [x, y, z] = map read $ splitOn "," string

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
  let num_cables = 100
  -- 1st star
  print "First star example:"
  mapM_ (print . lineToCoor) example
  -- Input text
  contents <- readFile "input.txt"

  print "First star input:"

  -- 2nd star
  print "Second star example:"

  print "Second star input:"
