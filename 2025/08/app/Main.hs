module Main where

import Data.Ord (compare, comparing)

import Data.List (minimumBy, sort, sortBy, (\\), product, concat, nub, any)
import Data.List.Split (splitOn)

import qualified Data.Set as Set

type Point3D = (Int, Int, Int)

pointToList :: Point3D -> [Int]
pointToList (x, y, z) = [x, y, z]

listToPoints :: [Int] -> [Point3D]
listToPoints (x : y : z : rest) = (x, y, z) : listToPoints rest
listToPoints _ = []

lineToCoor :: String -> Point3D
lineToCoor string = head $ listToPoints $ map read $ splitOn "," string

xCoor (x, _, _) = x

substituteListElement :: Int -> a -> [a] -> [a]
substituteListElement n element list = (++) (take n list) (element : drop (n + 1) list)

cutEachAxis :: [Point3D] -> Point3D
cutEachAxis point_list =
  ( (minX + maxX) `div` 2,
    (minY + maxY) `div` 2,
    (minZ + maxZ) `div` 2
  )
  where
    (xs, ys, zs) = unzip3 point_list
    [minX, minY, minZ] = map minimum [xs, ys, zs]
    [maxX, maxY, maxZ] = map maximum [xs, ys, zs]

corralledPointsWorker ::
  [[Point3D]] ->
  [[Point3D]] ->
  [Point3D] ->
  Point3D ->
  ([[Point3D]], [[Point3D]])
corralledPointsWorker point_storage cut_axis_dist_storage [] _ = (point_storage, cut_axis_dist_storage)
corralledPointsWorker point_storage cut_axis_dist_storage (first@(x, y, z) : rest) cut_point@(xCut, yCut, zCut) = corralledPointsWorker new_point_storage new_cut_axis_dist_storage rest cut_point
  where
    cut_axis_dist_list@[xDiff, yDiff, zDiff] = zipWith (-) [x, y, z] [xCut, yCut, zCut]
    cut_axis_dist = head $ listToPoints cut_axis_dist_list
    index = sum $ map fst $ filter ((>=) 0 . snd) $ zip [4, 2, 1] cut_axis_dist_list
    new_sector = first : (point_storage !! index)
    new_distance_sector = cut_axis_dist : (cut_axis_dist_storage !! index)
    new_point_storage = substituteListElement index new_sector point_storage
    new_cut_axis_dist_storage = substituteListElement index new_distance_sector cut_axis_dist_storage

corralledPoints ::
  [Point3D] ->
  ([[Point3D]], [[Point3D]])
corralledPoints points = corralledPointsWorker [[], [], [], [], [], [], [], []] [[], [], [], [], [], [], [], []] points $ cutEachAxis points

square :: (Num a) => a -> a
square n = n * n

distance :: Point3D -> Point3D -> Float
distance (x1, y1, z1) (x2, y2, z2) = sqrt $ fromIntegral $ sum $ map square $ zipWith (-) [x2, y2, z2] [x1, y1, z1]

pointListDistances :: [Point3D] -> [Point3D] -> [[(Int, Float)]]
pointListDistances _ [] = []
pointListDistances previous_points (first : rest) = sortedDistances : pointListDistances (previous_points ++ [first]) rest
  where
    lowIndexedDistances = zip [0 ..] $ map (distance first) previous_points
    highIndexedDistances = zip [1 + length previous_points ..] $ map (distance first) rest
    sortedDistances = sortBy (comparing snd) (lowIndexedDistances ++ highIndexedDistances)

corralBoundMinimum :: [[(Int, Float)]] -> (Int, (Int, (Int, Float)))
corralBoundMinimum =
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
      | all ((ref_distance <) . fromIntegral . abs) [x, y, z] = storage
      | otherwise = point : storage

sortDistances :: [[(Int, Float)]] -> [(Int, (Int, Float))]
sortDistances = sortBy (comparing (snd . snd)) . concat . zipWith (\ x y -> zip (repeat x) y) [0 ..]

connectionsInterface :: [(Int, (Int, Float))] -> Int -> [(Int, Set.Set Int)]
connectionsInterface distances = filter (not . null . snd) . zip [0..] . connections (replicate (length distances) Set.empty) distances

connections :: [Set.Set Int] -> [(Int, (Int, Float))] -> Int -> [Set.Set Int]
connections storage [] _ = storage
connections storage _ 0 = storage
connections storage ((index1,(index2, _)): rest) cables
  | new_connection == old_connection = connections storage rest cables
  | otherwise = connections new_storage rest (cables - 1)
  where
    [min_index, max_index] = sort [index1, index2]
    old_connection = storage !! min_index
    new_connection = max_index `Set.insert` old_connection
    new_storage = substituteListElement min_index new_connection storage

jboxesGroups :: [Set.Set Int] -> [(Int, Set.Set Int)] -> [Set.Set Int]
jboxesGroups storage [] = storage
jboxesGroups [] ((index, jboxes):rest) = jboxesGroups [index `Set.insert` jboxes] rest
jboxesGroups storage ((index, jboxes):rest) = jboxesGroups new_storage rest
  where
    new_connections = index `Set.insert` jboxes
    groups_intersected = map fst $ filter (not . null . snd) $ zip [0..] $ map (Set.intersection new_connections) storage
    groups_not_intersected = [0..length storage - 1] \\ groups_intersected
    new_group = Set.unions $ (:) new_connections $ map (storage !!) groups_intersected
    new_storage = new_group : map (storage !!) groups_not_intersected

groupScore :: [Set.Set a] -> Int
groupScore = product . take 3 . sortBy (flip compare) . nub . map length

jboxGroupLastMergeInterface :: [(Int, (Int, Float))] -> [Int]
jboxGroupLastMergeInterface distances = jboxGroupLastMerge [] [-1, -1] distances $ floor $ sqrt $ fromIntegral $ 2 * length distances

jboxGroupLastMerge :: [Set.Set Int] -> [Int] -> [(Int, (Int, Float))] -> Int -> [Int]
jboxGroupLastMerge _ last_connection [] _ = last_connection
jboxGroupLastMerge [] _ ((index1, (index2, _)):rest) num_points = jboxGroupLastMerge [Set.fromList [index1, index2]] [index1, index2] rest num_points
jboxGroupLastMerge storage last_connection ((index1, (index2, _)): rest) num_points
  | num_points == length (head storage) = last_connection
  | new_connection `elem` intersections = jboxGroupLastMerge storage last_connection rest num_points
  | null intersections = jboxGroupLastMerge (new_connection : storage) index_list rest num_points
  | otherwise = jboxGroupLastMerge new_storage index_list rest num_points
  where
    index_list = [index1, index2]
    new_connection = Set.fromList index_list
    intersections = map (Set.intersection new_connection) storage
    groups_intersected = map fst $ filter (not . null . snd) $ zip [0..] intersections
    groups_not_intersected = [0..length storage - 1] \\ groups_intersected
    new_group = Set.unions $ (:) new_connection $ map (storage !!) groups_intersected
    new_storage = new_group : map (storage !!) groups_not_intersected


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
  let example_cables = 10
  -- let example_cables = length example
  -- 1st star
  print "First star example:"

  let example_coords = map lineToCoor example
  -- print "Parsed coords:"
  -- mapM_ print example_coords

  -- print "Cut point:"
  -- print $ cut_each_axis example_coords

  -- let (example_corralled, example_corralled_dist) = corralledPoints example_coords
  -- print "Corralled:"
  -- mapM_ print example_corralled
  -- print "Cut distances:"
  -- mapM_ print example_corralled_dist

  -- -- print $ pointListDistances [] $ head example_corralled
  -- let example_distances_inside_corral = map (pointListDistances []) example_corralled
  -- print "Distances inside corrales:"
  -- mapM_ print example_distances_inside_corral

  -- let example_min@(example_corral_index, (example_index, (example_partner_index, example_min_distance))) = corralBoundMinimum example_distances_inside_corral
  -- print "Min distance of corralled points:"
  -- print example_min

  -- let example_edgy_points = zipWith (edgyPoints [] example_min_distance) example_corralled example_corralled_dist
  -- print "Edgy points:"
  -- mapM_ print example_edgy_points

  let example_distances = pointListDistances [] example_coords
  -- print "Distances:"
  -- print example_distances

  let example_sorted_distances = sortDistances example_distances
  -- print "Sorted distances:"
  -- print example_sorted_distances

  let example_connections = connectionsInterface example_sorted_distances example_cables
  -- print $ show example_cables ++" smallest connections:"
  -- mapM_ print example_connections

  print "Num connections:"
  print $ sum $ map (length . snd) example_connections

  let example_groups = jboxesGroups [] example_connections
  print "JBoxes groups:"
  print example_groups

  print "Cables used:"
  print $ sum $ map (flip (-) 1 . length) example_groups

  let example_group_score = groupScore example_groups
  print "Group points:"
  print example_group_score

  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"
  let first_star_cables = 1000

  let first_star_coords = map lineToCoor $ lines contents

  let first_star_distances = pointListDistances [] first_star_coords
  -- print "Distances:"
  -- print first_star_distances

  let first_star_sorted_distances = sortDistances first_star_distances
  -- print "Sorted distances:"
  -- print first_star_sorted_distances

  let first_star_connections = connectionsInterface first_star_sorted_distances first_star_cables
  -- print show first_star_cables ++" smallest connections:"
  -- mapM_ print first_star_connections

  print "Num connections:"
  print $ sum $ map (length . snd) first_star_connections

  let first_star_groups = jboxesGroups [] first_star_connections
  print "JBoxes groups:"
  print first_star_groups

  print "Cables used:"
  print $ sum $ map (flip (-) 1 . length) first_star_groups

  let first_star_group_score = groupScore first_star_groups
  print "Group score:"
  print first_star_group_score

  -- 2nd star
  print "Second star example:"

  let snd_example_last_connection = jboxGroupLastMergeInterface example_sorted_distances
  let snd_example_points = map (example_coords !!) snd_example_last_connection
  print "Last connection points:"
  mapM_ print snd_example_points

  print "Last connection score:"
  print $ product $ map xCoor snd_example_points

  print "Second star input:"

  let snd_star_last_connection = jboxGroupLastMergeInterface first_star_sorted_distances
  let snd_star_points = map (first_star_coords !!) snd_star_last_connection
  print "Last connection points:"
  mapM_ print snd_star_points

  print "Last connection score:"
  print $ product $ map xCoor snd_star_points
