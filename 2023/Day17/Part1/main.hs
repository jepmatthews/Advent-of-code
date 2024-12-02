import Data.Ix (Ix (inRange, index, range, rangeSize))
import Data.Array (listArray)

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
  inRange (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y) = x >= x1 && x <= x2 && y >= y1 && y <= y2
  index (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y)
    | inRange (Coordinate x1 y1, Coordinate x2 y2) (Coordinate x y) = (x - x1) + ((y - y1) * (x2 - x1 + 1))
    | otherwise = -1
  rangeSize (m, n) = index (m, n) n + 1

main = do
  contents <- readFile "sampleInput.txt"
  let lins = map (map (\y -> read [y])) (lines contents)  :: [[Int]]
  let array = listArray (Coordinate 0 0, Coordinate (length (head lins) - 1) (length lins - 1)) (concat lins)
  print lins
  print array

