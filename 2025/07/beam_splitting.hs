import Data.IntMap.Lazy qualified as IntMap
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

firstLineSource :: Int -> String -> Int
firstLineSource state line
  | state >= length line = -1
  | line !! state == 'S' = state
  | otherwise = firstLineSource (state + 1) line

lineSplitterCounter :: Set.Set Int -> Int -> Set.Set Int -> String -> (Int, Set.Set Int)
-- lineSplitterCounter newBeams splitsSoFar oldBeams "" = (splitsSoFasr, newBeams `Set.union` oldBeams)
lineSplitterCounter newBeams splitsSoFar unprocessedBeams line
  | Set.null unprocessedBeams = (splitsSoFar, newBeams)
  | line !! currentBeam == '^' = lineSplitterCounter (Set.union newBeams $ Set.fromList [currentBeam - 1, currentBeam + 1]) (splitsSoFar + 1) lessUnprocessedBeams line
  | otherwise = lineSplitterCounter (newBeams `Set.union` Set.singleton currentBeam) splitsSoFar lessUnprocessedBeams line
  where
    currentBeam = Set.elemAt 0 unprocessedBeams
    lessUnprocessedBeams = Set.deleteAt 0 unprocessedBeams

beamSplitter :: Int -> Set.Set Int -> [String] -> Int
beamSplitter numSplits beams [] = numSplits
beamSplitter numSplits beams (line : rest) = beamSplitter splitsSoFar newBeams rest
  where
    (splitsSoFar, newBeams) = lineSplitterCounter Set.empty numSplits beams line

splitterInterface :: [String] -> Int
splitterInterface (first : rest) = beamSplitter 0 (Set.singleton $ firstLineSource 0 first) rest

timelineCounter :: IntMap.IntMap Int -> IntMap.IntMap Int -> String -> IntMap.IntMap Int
-- timelineCounter newBeams splitsSoFar oldBeams "" = (splitsSoFasr, newBeams `IntMap.union` oldBeams)
timelineCounter newBeams unprocessedBeams line
  | IntMap.null unprocessedBeams = newBeams
  | line !! currentBeam == '^' = timelineCounter (IntMap.unionWith (+) newBeams $ IntMap.fromAscList [(currentBeam - 1, beamLegacy), (currentBeam + 1, beamLegacy)]) lessUnprocessedBeams line
  | otherwise = timelineCounter (IntMap.insertWith (+) currentBeam beamLegacy newBeams) lessUnprocessedBeams line
  where
    (currentBeam, beamLegacy) = fromMaybe (-1, 0) $ IntMap.lookupMin unprocessedBeams
    lessUnprocessedBeams = IntMap.deleteMin unprocessedBeams

timelineSplitter :: IntMap.IntMap Int -> [String] -> Int
timelineSplitter beams [] = sum beams
timelineSplitter beams (line : rest) = timelineSplitter newBeams rest
  where
    newBeams = timelineCounter IntMap.empty beams line

timelineInterface :: [String] -> Int
timelineInterface (first : rest) = timelineSplitter (IntMap.singleton (firstLineSource 0 first) 1) rest

main :: IO ()
main = do
  let example =
        [ ".......S.......",
          "...............",
          ".......^.......",
          "...............",
          "......^.^......",
          "...............",
          ".....^.^.^.....",
          "...............",
          "....^.^...^....",
          "...............",
          "...^.^...^.^...",
          "...............",
          "..^...^.....^..",
          "...............",
          ".^.^.^.^.^...^.",
          "..............."
        ]
  -- 1st star
  print "First star example:"
  print $ splitterInterface example

  -- Input text
  contents <- readFile "input.txt"

  print "First star input:"
  print $ splitterInterface $ lines contents

  -- 2nd star
  print "Second star example:"
  print $ timelineInterface example

  print "Second star input:"
  print $ timelineInterface $ lines contents
