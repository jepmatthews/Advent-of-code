

main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let withExpansion = addAllExpansion lins
  let withFirstCoordinate = map (\x -> zip x (getCoordinatesForRow x [])) withExpansion
  let withBothCoordinates = transpose $ map (\p -> zipWith (\y (char,x) -> (char,(x,y))) (getCoordinatesForRow (map fst p) []) p) (transpose $ withFirstCoordinate)
  print $ head withExpansion
  print $ getAllPairDistances $ getStars withBothCoordinates


transpose :: [[a]] -> [[a]]
transpose listOfLists
  | all null listOfLists = []
  | otherwise = map head listOfLists : transpose (map (drop 1) listOfLists)

addAllExpansion :: [[Char]] -> [[Char]]
addAllExpansion lins = transpose $ addExpansionToOneDimension $ transpose $ addExpansionToOneDimension lins

getCoordinatesForRow :: [Char] -> [(Char,Int)] -> [Int]
getCoordinatesForRow [] acc = map snd acc
getCoordinatesForRow (x:xs) acc = getCoordinatesForRow xs (acc ++ [(x,length (filter (\x -> fst x /= 'x') acc)+(length (filter (\x -> fst x == 'x') acc)*1000000))])

addExpansionToOneDimension :: [[Char]] -> [[Char]]
addExpansionToOneDimension [] = []
addExpansionToOneDimension (line:lines)
  | all (\x -> x == '.' || x == 'x') line = replicate (length line) 'x' : addExpansionToOneDimension lines
  | otherwise = line : addExpansionToOneDimension lines

getStars :: [[(Char,(Int,Int))]] -> [(Int,Int)]
getStars skyArray = map snd $ filter (\x -> fst x == '#') $ concat skyArray

getAllPairDistances :: [(Int,Int)] -> Int
getAllPairDistances [] = 0
getAllPairDistances [x] = 0
getAllPairDistances (x:xs) = sum (getDistancesToStars x xs) + getAllPairDistances xs

getDistancesToStars :: (Int,Int) -> [(Int,Int)] -> [Int]
getDistancesToStars (y,x) = map (\(compy,compx) -> abs (compy - y) + abs (compx - x))

