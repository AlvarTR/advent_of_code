import Data.Set (Set, deleteAt, elemAt, empty, fromList, null, singleton, union)

firstLineSource :: Int -> String -> Int
firstLineSource state line
  | state >= length line = -1
  | line !! state == 'S' = state
  | otherwise = firstLineSource (state + 1) line

lineSplitterCounter :: Set Int -> Int -> Set Int -> String -> (Int, Set Int)
-- lineSplitterCounter newBeams splitsSoFar oldBeams "" = (splitsSoFasr, newBeams `union` oldBeams)
lineSplitterCounter newBeams splitsSoFar unprocessedBeams line
  | Data.Set.null unprocessedBeams = (splitsSoFar, newBeams)
  | line !! currentBeam == '^' = lineSplitterCounter (union newBeams $ fromList [currentBeam - 1, currentBeam + 1]) (splitsSoFar + 1) lessUnprocessedBeams line
  | otherwise = lineSplitterCounter (newBeams `union` singleton currentBeam) splitsSoFar lessUnprocessedBeams line
  where
    currentBeam = elemAt 0 unprocessedBeams
    lessUnprocessedBeams = deleteAt 0 unprocessedBeams

beamSplitter :: Int -> Set Int -> [String] -> Int
beamSplitter numSplits beams [] = numSplits
beamSplitter numSplits beams (line : rest) = beamSplitter splitsSoFar newBeams rest
  where
    (splitsSoFar, newBeams) = lineSplitterCounter empty numSplits beams line

splitterInterface :: [String] -> Int
splitterInterface (first : rest) = beamSplitter 0 (singleton $ firstLineSource 0 first) rest

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

  print "Second star input:"
