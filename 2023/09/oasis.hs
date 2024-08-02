oasisExtrapolate :: [Integer] -> Integer
oasisExtrapolate [] = 0
oasisExtrapolate list
  | all (== 0) list = 0
oasisExtrapolate list = extrapolated + last list
  where
    extrapolated = oasisExtrapolate $ zipWith (-) (tail list) list

main :: IO ()
main = do
  let x =
        [ [0, 3, 6, 9, 12, 15],
          [1, 3, 6, 10, 15, 21],
          [10, 13, 16, 21, 30, 45]
        ]
  print $ sum $ map oasisExtrapolate x