import Data.Array (listArray, Array, assocs)
main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let withExpansion = addAllExpansion lins
  let array = listArray ((0,0),(length withExpansion - 1, length (head withExpansion) - 1)) (concat withExpansion)
  let stars = getStars array
  print $ stars
  print $ getAllPairDistances stars


transpose :: [[Char]] -> [[Char]]
transpose listOfLists
  | all null listOfLists = []
  | otherwise = map head listOfLists : transpose (map (drop 1) listOfLists)

addAllExpansion :: [[Char]] -> [[Char]]
addAllExpansion lins = transpose $ addExpansionToOneDimension $ transpose $ addExpansionToOneDimension lins

addExpansionToOneDimension :: [[Char]] -> [[Char]]
addExpansionToOneDimension [] = []
addExpansionToOneDimension (line:lines)
  | all (== '.') line = line : line : addExpansionToOneDimension lines
  | otherwise = line : addExpansionToOneDimension lines

getStars :: Array (Int,Int) Char -> [(Int,Int)]
getStars skyArray = map fst $ filter (\x -> snd x == '#') $ assocs skyArray

getAllPairDistances :: [(Int,Int)] -> Int
getAllPairDistances [] = 0
getAllPairDistances [x] = 0
getAllPairDistances (x:xs) = sum (getDistancesToStars x xs) + getAllPairDistances xs

getDistancesToStars :: (Int,Int) -> [(Int,Int)] -> [Int]
getDistancesToStars (y,x) = map (\(compy,compx) -> abs (compy - y) + abs (compx - x))

