module FreshIngredients where

import Data.Function (on)
import Data.List (sortBy)
import Data.List.Split (splitOn)
import qualified Data.Set as Set

type Interval = (Integer, Integer)

freshIngredient :: [Interval] -> Integer -> Int
freshIngredient [] _ = 0
freshIngredient ((start, stop) : rest) ingredient_id
  | ingredient_id `elem` [start .. stop] = 1
  | otherwise = freshIngredient rest ingredient_id

freshIngredient' :: [Interval] -> Integer -> Int
freshIngredient' [] _ = 0
freshIngredient' ((start, stop) : rest) ingredient_id
  | ingredient_id >= start && ingredient_id <= stop = 1
  | otherwise = freshIngredient' rest ingredient_id

allFreshInIntervals :: Set.Set Integer -> [Interval] -> Set.Set Integer
allFreshInIntervals set [] = set
allFreshInIntervals set ((start, stop) : rest) = allFreshInIntervals (Set.union set $ Set.fromList [start .. stop]) rest

sortByFirst :: (Ord a) => [(a, b)] -> [(a, b)]
sortByFirst = sortBy (compare `on` fst)

allFreshInIntervals' :: [Interval] -> [Interval] -> [Interval]
allFreshInIntervals' list_intervals [] = list_intervals
allFreshInIntervals' [] ((start, stop) : str_rest) = allFreshInIntervals' [(start, stop)] str_rest
allFreshInIntervals' list_intervals ((start, stop) : str_rest) = allFreshInIntervals' (updateFreshnessList [] list_intervals (start, stop)) str_rest

updateFreshnessList :: [Interval] -> [Interval] -> Interval -> [Interval]
updateFreshnessList processed_intervals [] new_interval = processed_intervals ++ [new_interval]
updateFreshnessList processed_intervals interval_list@(interval@(list_start, list_stop) : rest) (start, stop)
  | stop < (list_start - 1) = processed_intervals ++ (start, stop) : interval_list
  | (list_stop + 1) < start = updateFreshnessList (processed_intervals ++ [interval]) rest (start, stop)
  | list_start <= start && stop <= list_stop = processed_intervals ++ interval_list
  | start < list_start = updateFreshnessList processed_intervals rest (start, max list_stop stop)
  | list_stop < stop = updateFreshnessList processed_intervals rest (min list_start start, stop)
  | otherwise = processed_intervals ++ interval_list

numberOfFreshInIntervals :: [Interval] -> Integer
numberOfFreshInIntervals str_list = (+) (toInteger $ length fresh_in_intervals) $ sum $ map (uncurry (flip (-))) fresh_in_intervals
  where
    fresh_in_intervals = allFreshInIntervals' [] str_list

debugLineByLine :: [[a]] -> [a] -> [[a]]
debugLineByLine buffer [] = reverse $ map reverse buffer
debugLineByLine [] (first : rest) = debugLineByLine [[first]] rest
debugLineByLine buffer@(buffer_head : _) (first : rest) = debugLineByLine ((first : buffer_head) : buffer) rest

duplify :: [a] -> (a, a)
duplify [] = error ""
duplify [_] = error ""
duplify [x, y] = (x, y)
duplify (_ : _ : _) = error ""

stringListToListOfIntervals :: [String] -> [Interval]
stringListToListOfIntervals = map (duplify . map read . splitOn "-")
