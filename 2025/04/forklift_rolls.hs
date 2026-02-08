main :: IO ()
main = do
  let example =
        [ "..@@.@@@@.",
          "@@@.@.@.@@",
          "@@@@@.@.@@",
          "@.@@@@..@.",
          "@@.@@@@.@@",
          ".@@@@@@@.@",
          ".@.@.@.@@@",
          "@.@@@.@@@@",
          ".@@@@@@@@.",
          "@.@.@@@.@."
        ]
  -- 1st star
  print "First star example:"

  -- Input text
  contents <- readFile "input.txt"
  print "First star input:"

  -- 2nd star
  print "Second star example:"

  print "Second star input:"
