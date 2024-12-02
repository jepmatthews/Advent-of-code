import Data.Ix (Ix (range, inRange, index, rangeSize))
import Data.Array (Array, array, assocs, bounds, (!))
import Data.List (groupBy, sort)
data Coordinate = Coordinate {xCoor :: Int, yCoor :: Int} deriving (Show, Eq)

instance Ord Coordinate where
  compare (Coordinate x1 y1) (Coordinate x2 y2)
    | yCompare /= EQ = yCompare
    | otherwise = xCompare
    where
      yCompare = y1 `compare` y2
      xCompare = x1 `compare` x2

instance Ix Coordinate where
  range :: (Coordinate, Coordinate) -> [Coordinate]
  range (Coordinate x1 y1, Coordinate x2 y2) = [Coordinate x y | y <- [y1 .. y2], x <- [x1 .. x2]]
  inRange :: (Coordinate, Coordinate) -> Coordinate -> Bool
  inRange (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y) = x >= x1 && x <= x2 && y >= y1 && y <= y2
  index (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y)
    | inRange (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y) = (x - x1 + 1) + ((y - y1) * (x2 - x1 + 1))
    | otherwise = -1
  rangeSize (m, n) = index (m, n) n + 1

data Tile = Empty | Rock | Step | Start deriving (Eq,Ord)

instance Show Tile where
  show Empty = "."
  show Rock = "#"
  show Step = "O"
  show Start = "S"

main = do
  contents <- readFile "input.txt"
  let tileArray = parseGrid $ lines contents
  let step64 = nthStep tileArray 64
  print tileArray
  putStrLn $ unlines $ displayArray tileArray
  putStrLn $ unlines $ displayArray step64
  print $ length $ filter (\(_,t)->t == Step) (assocs step64)

nthStep :: Array Coordinate Tile -> Int -> Array Coordinate Tile
nthStep tileArr n = iterate executeStep tileArr !! max 0 n

executeStep :: Array Coordinate Tile -> Array Coordinate Tile
executeStep tileArray = array (bounds tileArray) (map (evaluateTile tileArray) (assocs tileArray))

evaluateTile :: Array Coordinate Tile -> (Coordinate, Tile) -> (Coordinate, Tile)
evaluateTile _ (coor, Rock) = (coor, Rock)
evaluateTile tileArr (coor, _)
    | Just Step `elem` neighbours || Just Start `elem` neighbours = (coor,Step)
    | otherwise = (coor,Empty)
    where neighbours = map (arrayLookup tileArr) (getNeighbours coor)

arrayLookup :: Ix k => Array k a -> k -> Maybe a
arrayLookup arr key
    | not $ inRange (bounds arr) key = Nothing
    | otherwise = Just (arr ! key)

getNeighbours :: Coordinate -> [Coordinate]
getNeighbours (Coordinate x y) = [Coordinate (x - 1) y,Coordinate x (y + 1),Coordinate (x + 1) y,Coordinate x (y - 1)]

parseGrid :: [String] -> Array Coordinate Tile
parseGrid strings = array (Coordinate 0 0,Coordinate (length (head strings) - 1) (length strings - 1)) $ concatMap (\(y,line) -> zipWith (\ x char -> (Coordinate x y, parseTile char)) [0..] line ) (zip [0..] (reverse strings))

parseTile :: Char -> Tile
parseTile '.' = Empty
parseTile '#' = Rock
parseTile 'O' = Step
parseTile 'S' = Start

displayArray :: (Show a, Ord a) => Array Coordinate a -> [String]
displayArray array = map (concatMap (\(_, stat) -> show stat)) $ reverse $ groupBy (\(Coordinate _ y1, _) (Coordinate _ y2, _) -> y1 == y2) (sort (assocs array))
