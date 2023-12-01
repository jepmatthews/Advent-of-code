main = do
  contents <- readFile "input.txt"
  let test = processLines (lines contents)
  print (product test)

processLines :: [String] -> [Int]
processLines [l1, l2] =
  let times = map read (words (tail (dropWhile (/= ':') l1)))
      distances = map read (words (tail (dropWhile (/= ':') l2)))
   in zipWith (curry processLine) times distances

processLine :: (Int, Int) -> Int
processLine (time, distance)
  | dif < 0 = 0
  | odd time = (ceiling ((sqrt (fromIntegral dif) + 1) / 2) - 1) * 2
  | otherwise = (ceiling (sqrt (fromIntegral dif) / 2) * 2) - 1
  where
    dif = (time ^ 2) - (4 * distance)
