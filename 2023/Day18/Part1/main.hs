import Data.Array (Array, array, assocs, bounds, listArray, (!), (//))
import Data.Ix (Ix (inRange, index, range, rangeSize))
import Data.List (groupBy, sort)
import Data.Set (fromList, lookupMax, lookupMin, member)

data Coordinate = Coordinate {xCoor :: Int, yCoor :: Int} deriving (Show, Eq)

data Direction = U | R | D | L deriving (Show, Eq, Ord, Enum, Read)

type Distance = Int

data Command = Command Direction Distance deriving (Show, Eq)

data DugoutStatus = Wall | Ground | Dug deriving (Eq, Ord, Enum)

instance Show DugoutStatus where
  show Wall = "#"
  show Ground = "."
  show Dug = "+"

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
    | inRange (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y) = (x - x1) + ((y - y1) * (x2 - x1 + 1))
    | otherwise = -1
  rangeSize (m, n) = index (m, n) n + 1

main = do
  contents <- readFile "input.txt"
  let coordinateArray = moveAllCommands (Coordinate 0 0) $ map parseLine $ lines contents
  let dugoutArray = populateDugOutAllRows $ generateArray coordinateArray
  putStrLn $ unlines $ reverse $ displayArray dugoutArray
  print $ length $ filter (\x -> snd x /= Ground) $ assocs dugoutArray

-- putStrLn $ show test

moveAllCommands :: Coordinate -> [Command] -> [Coordinate]
moveAllCommands coord [] = [coord]
moveAllCommands coord (command : commands) = moveAllCommands step commands ++ steps
  where
    (step : steps) = moveCommand coord command

getMinAndMaxX :: [Coordinate] -> (Int, Int)
getMinAndMaxX coordinates = (minimum xs, maximum xs)
  where
    xs = map (\(Coordinate x _) -> x) coordinates

generateArray :: [Coordinate] -> Array Coordinate DugoutStatus
generateArray coordinates =
  array
    (Coordinate minX minY, Coordinate maxX maxY)
    [(Coordinate x y, getStatus x y) | x <- [minX .. maxX], y <- [minY .. maxY]]
  where
    (minX, maxX) = getMinAndMaxX coordinates
    (minY, maxY) = getMinAndMaxY coordinates
    coordinateSet = fromList coordinates
    getStatus x y =
      if member (Coordinate x y) coordinateSet
        then Wall
        else Ground

populateDugOutAllRows :: Array Coordinate DugoutStatus -> Array Coordinate DugoutStatus
populateDugOutAllRows array = foldl (\arr y -> populateDugOutRow (Coordinate minX y) (False, False) arr) array [minY .. maxY]
  where
    (Coordinate _ minY, Coordinate _ maxY) = bounds array
    (Coordinate minX _, _) = bounds array

populateDugOutRow :: Coordinate -> (Bool, Bool) -> Array Coordinate DugoutStatus -> Array Coordinate DugoutStatus
populateDugOutRow coordinate previousNeighbours array
  | not inBounds = array
  | currentState == Wall = populateDugOutRow (Coordinate (x + 1) y) newNeighbours array
  | previousNeighbours == (True, True) = populateDugOutRow (Coordinate (x + 1) y) previousNeighbours (array // [(coordinate, Dug)])
  | otherwise = populateDugOutRow (Coordinate (x + 1) y) previousNeighbours array
  where
    inBounds = inRange (bounds array) coordinate
    currentState = array ! coordinate
    (Coordinate x y) = coordinate
    coordAbove = Coordinate x (y + 1)
    coordBelow = Coordinate x (y - 1)
    cellAbove = if inRange (bounds array) coordAbove then array ! coordAbove else Ground
    cellBelow = if inRange (bounds array) coordBelow then array ! coordBelow else Ground
    newNeighbours = (fst previousNeighbours /= (cellAbove == Wall), snd previousNeighbours /= (cellBelow == Wall))

getMinAndMaxY :: [Coordinate] -> (Int, Int)
getMinAndMaxY coordinates = (minimum xs, maximum xs)
  where
    xs = map (\(Coordinate _ y) -> y) coordinates

displayArray :: Array Coordinate DugoutStatus -> [String]
displayArray array = map (concatMap (\(_, stat) -> show stat)) $ groupBy (\(Coordinate _ y1, _) (Coordinate _ y2, _) -> y1 == y2) (sort $ assocs array)

moveCommand :: Coordinate -> Command -> [Coordinate]
moveCommand coord (Command dir 0) = [coord]
moveCommand coord (Command dir dist) = moveDirection dir step : step : steps
  where
    (step : steps) = moveCommand coord (Command dir (dist - 1))

moveDirection :: Direction -> Coordinate -> Coordinate
moveDirection U (Coordinate x y) = Coordinate x (y + 1)
moveDirection R (Coordinate x y) = Coordinate (x + 1) y
moveDirection D (Coordinate x y) = Coordinate x (y - 1)
moveDirection L (Coordinate x y) = Coordinate (x - 1) y

parseLine :: String -> Command
parseLine str = Command (read direction) (read distance)
  where
    (direction : distance : _) = words str

