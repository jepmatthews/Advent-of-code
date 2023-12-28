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
  | onlyOneDifference (take (length endList) startList) (take (length startList) endList) 0 = length endList
  | otherwise = findMirrorIndex xs (x : endList)
  where
    x : xs = startList

onlyOneDifference :: (Eq a) => [[a]] -> [[a]] -> Int -> Bool
onlyOneDifference xs ys _
  | length xs /= length ys = error "Mismatching list lengths"
onlyOneDifference _ _ currentDifferences
  | currentDifferences > 1 = False
onlyOneDifference [] [] currentDifferences
  | currentDifferences == 1 = True
  | otherwise = False
onlyOneDifference (x : xs) (y : ys) currentDifferences
  | differences + currentDifferences > 1 = False
  | otherwise = onlyOneDifference xs ys (differences + currentDifferences)
  where
    differences = numberOfDifferences x y

numberOfDifferences :: (Eq a) => [a] -> [a] -> Int
numberOfDifferences [] _ = 0
numberOfDifferences _ [] = 0
numberOfDifferences (x : xs) (y : ys)
  | x == y = numberOfDifferences xs ys
  | otherwise = 1 + numberOfDifferences xs ys