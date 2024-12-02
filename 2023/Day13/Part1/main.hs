main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let rockMaps = splitOn "" lins
  let horizontalMirrors = map (`findMirrorIndex` []) rockMaps
  let verticalMirrors = map (\x -> findMirrorIndex (transpose x) []) rockMaps
  print horizontalMirrors
  print verticalMirrors
  print $ sum (map (100 *) horizontalMirrors) + sum verticalMirrors

transpose :: [[a]] -> [[a]]
transpose listOfLists
  | all null listOfLists = []
  | otherwise = map head listOfLists : transpose (map (drop 1) listOfLists)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn char [] = []
splitOn char str = start : splitOn char (drop 1 xs)
  where
    (start, xs) = span (/= char) str

findMirrorIndex :: (Eq a) => [[a]] -> [[a]] -> Int
findMirrorIndex [] [] = 0
findMirrorIndex [] endList = 0
findMirrorIndex startList [] = findMirrorIndex (drop 1 startList) (take 1 startList)
findMirrorIndex startList endList
  | take (length endList) startList == take (length startList) endList = length endList
  | otherwise = findMirrorIndex xs (x : endList)
  where
    x : xs = startList