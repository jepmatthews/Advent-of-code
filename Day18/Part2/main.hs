import Data.Array (Array, array, assocs, bounds, listArray, (!), (//))
import Data.Ix (Ix (inRange, index, range, rangeSize))
import Data.List (groupBy, sort)
import Data.Set (fromList, lookupMax, lookupMin, member)
import Numeric(readHex)

data Coordinate = Coordinate {xCoor :: Int, yCoor :: Int} deriving (Show, Eq)

data Direction = R | D | L | U deriving (Show, Eq, Ord, Enum, Read)

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
    | inRange (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y) = (x - x1 + 1) + ((y - y1) * (x2 - x1 + 1))
    | otherwise = -1
  rangeSize (m, n) = index (m, n) n + 1

main = do
  contents <- readFile "input.txt"
  let commands = map parseLine $ lines contents
  let vertices = Coordinate 0 0 : moveAllCommands (Coordinate 0 0) commands
  let area = abs (calculateArea vertices `div` 2) + getAreaAdjustment commands
  print vertices
  print area

moveAllCommands :: Coordinate -> [Command] -> [Coordinate]
moveAllCommands _ [] = []
moveAllCommands coord ((Command dir dist) : commands) = newCoordinate : moveAllCommands newCoordinate commands
  where newCoordinate = moveDirection dir coord dist

getAreaAdjustment :: [Command] -> Int
getAreaAdjustment commands = 3 + ((length commands - 4) `div` 2) + (sum (map (\(Command _ dist) -> dist - 1) commands)`div`2)

calculateArea :: [Coordinate] -> Int
calculateArea [_] = 0
calculateArea ((Coordinate x1 y1):(Coordinate x2 y2):vertices)
  = ((y1 + y2) * (x1 - x2)) + calculateArea (Coordinate x2 y2:vertices)

getDirection :: Command -> Direction
getDirection (Command dir _) = dir

moveDirection :: Direction -> Coordinate -> Int -> Coordinate
moveDirection U (Coordinate x y) dist = Coordinate x (y + dist)
moveDirection R (Coordinate x y) dist = Coordinate (x + dist) y
moveDirection D (Coordinate x y) dist = Coordinate x (y - dist)
moveDirection L (Coordinate x y) dist = Coordinate (x - dist) y

parseLine :: String -> Command
parseLine str = Command direction distance
  where
    (_:_:commStr:_) = words str
    direction = toEnum $ read $ take 1 $ drop 7 commStr
    [(distance,_)] = readHex $ take 5 $ drop 2 commStr


