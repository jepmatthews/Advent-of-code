import Data.List (groupBy, sort)
data PlatformObject = Round | Cube | Empty deriving (Show, Eq, Enum, Ord)
type Platform = [[PlatformObject]]

main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let platform = map (map parsePlaformObject) lins
  let tiltedPlatform = transpose $ map rollRocksLeft $ transpose platform
  print $ calculateWeight tiltedPlatform

transpose :: [[a]] -> [[a]]
transpose listOfLists
  | all null listOfLists = []
  | otherwise = map (\(x:xs)-> x) listOfLists : transpose (map (drop 1) listOfLists)


parsePlaformObject :: Char -> PlatformObject
parsePlaformObject '.' = Empty
parsePlaformObject '#' = Cube
parsePlaformObject 'O' = Round
parsePlaformObject char = error $ "Unexpected character: " ++ [char]


rollRocksLeft :: [PlatformObject] -> [PlatformObject]
rollRocksLeft row = concatMap sort (groupBy (\x y -> x == y || (x == Round && y == Empty) || (x == Empty && y == Round)) row)

calculateWeight :: Platform -> Int
calculateWeight [] = 0
calculateWeight (row:rows) = (length (filter (== Round) row) * (length rows +1)) + calculateWeight rows
