main = do
  contents <- readFile "input.txt"
  let sequences = map parseLine (lines contents)
  let lins = map getAllDifferences sequences
  let nextResults = map getNextResult lins
  print (sum nextResults)

getNextResult :: [[Int]] -> Int
getNextResult [] = 0
getNextResult differences = getNextResult (tail differences) + head (head differences)

parseLine :: String -> [Int]
parseLine string = map read (words string)

getAllDifferences :: [Int] -> [[Int]]
getAllDifferences xs = map reverse (recursiveDifferences [xs] xs)

recursiveDifferences :: [[Int]] -> [Int] -> [[Int]]
recursiveDifferences acc xs
    | all (0 ==) difs = acc
    | otherwise = recursiveDifferences (acc ++ [difs]) difs
    where difs = findDifferences xs []

findDifferences :: [Int] -> [Int] -> [Int]
findDifferences [] acc = acc
findDifferences [_] acc = acc
findDifferences (x:y) acc = findDifferences y (acc ++ [head y-x])
