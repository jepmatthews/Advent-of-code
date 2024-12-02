import Data.List (groupBy, sort, sortBy)
import Data.Map (Map, empty, insert, lookup)
import Data.Ord (Down (Down), comparing)
import Prelude hiding (lookup)

data PlatformObject = Round | Cube | Empty deriving (Show, Eq, Enum, Ord)

type Platform = [[PlatformObject]]

main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let platform = map (map parsePlaformObject) lins
  print $ calculateWeight $ getStateAfterNCycles platform 1000000000

transpose :: [[a]] -> [[a]]
transpose listOfLists
  | all null listOfLists = []
  | otherwise = map (\(x : xs) -> x) listOfLists : transpose (map (drop 1) listOfLists)

parsePlaformObject :: Char -> PlatformObject
parsePlaformObject '.' = Empty
parsePlaformObject '#' = Cube
parsePlaformObject 'O' = Round
parsePlaformObject char = error $ "Unexpected character: " ++ [char]

rollRocksLeft :: [PlatformObject] -> [PlatformObject]
rollRocksLeft row = concatMap sort (groupBy (\x y -> x == y || (x == Round && y == Empty) || (x == Empty && y == Round)) row)

rollRocksRight :: [PlatformObject] -> [PlatformObject]
rollRocksRight row = concatMap (sortBy (comparing Data.Ord.Down)) (groupBy (\x y -> x == y || (x == Round && y == Empty) || (x == Empty && y == Round)) row)

calculateWeight :: Platform -> Int
calculateWeight [] = 0
calculateWeight (row : rows) = (length (filter (== Round) row) * (length rows + 1)) + calculateWeight rows

cyclePlatform :: (Platform, Map Platform Platform, Bool) -> (Platform, Map Platform Platform, Bool)
cyclePlatform (platform, cacheMap, _)
  | Just result <- lookup platform cacheMap = (result, cacheMap, True)
cyclePlatform (platform, cacheMap, _) = (result, insert platform result cacheMap, False)
  where
    result = map rollRocksRight $ transpose $ map rollRocksRight $ transpose $ map rollRocksLeft $ transpose $ map rollRocksLeft $ transpose platform

getCyclesToFirstLoopStartAndLoopLength :: Platform -> (Int, Int)
getCyclesToFirstLoopStartAndLoopLength platform = (indexOfFirstLoopStart, loopLength)
  where
    cycleList = iterate cyclePlatform (platform, empty, False)
    (preLoop, remainder) = span (\(_, _, x) -> not x) cycleList
    ((firstRepeatedState, _, _) : _) = remainder
    loopLength = length (takeWhile (\(x, _, _) -> x /= firstRepeatedState) (drop 1 remainder)) + 1
    indexOfFirstLoopStart = length preLoop - loopLength + 1

getStateAfterNCycles :: Platform -> Int -> Platform
getStateAfterNCycles platform n
  | n <= indexOfFirstLoopStart = first (cycleList !! n)
  | otherwise = first (cycleList !! adjustedIndex)
  where
    cycleList = iterate cyclePlatform (platform, empty, False)
    (indexOfFirstLoopStart, loopLength) = getCyclesToFirstLoopStartAndLoopLength platform
    adjustedIndex = indexOfFirstLoopStart + ((n - indexOfFirstLoopStart) `mod` loopLength)

first :: (a, b, c) -> a
first (x, _, _) = x