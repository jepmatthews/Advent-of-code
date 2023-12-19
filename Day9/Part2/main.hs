main = do
  contents <- readFile "input.txt"
  let sequences = map parseLine (lines contents)
  let lins = map getAllDifferences sequences
  let nextResults = map getPreviousResult lins
  print (sum nextResults)


getPreviousResult :: [[Int]] -> Int
getPreviousResult [] = 0
getPreviousResult differences =  head (head differences) - getPreviousResult (tail differences)

parseLine :: String -> [Int]
parseLine string = map read (words string)

getAllDifferences :: [Int] -> [[Int]]
getAllDifferences xs = recursiveDifferences [xs] xs

recursiveDifferences :: [[Int]] -> [Int] -> [[Int]]
recursiveDifferences acc xs
    | all (0 ==) difs = acc
    | otherwise = recursiveDifferences (acc ++ [difs]) difs
    where difs = findDifferences xs []

findDifferences :: [Int] -> [Int] -> [Int]
findDifferences [] acc = acc
findDifferences [_] acc = acc
findDifferences (x:y) acc = findDifferences y (acc ++ [head y-x])
