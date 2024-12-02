import Data.Array (Ix, listArray, Array, bounds, (!))
import Data.Ix
import Data.Sequence (Seq (Empty))
import Data.Set (Set, member, insert, empty)
import Data.List (nub)

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

data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

data PathStep
  = SingleStep {location :: Coordinate, next :: PathStep}
  | SplitStep {location :: Coordinate, nextLeft :: PathStep, nextRight :: PathStep}
  | End
  | Loop
  deriving (Show, Eq)

data BeamComponent = HorizonalSplitter | VerticalSplitter | DownwardMirror | UpwardMirror | None deriving (Show, Eq, Enum)

main = do
  contents <- readFile "input.txt"
  let lins = map (map parseBeamComponent) (lines contents)
  let array = listArray (Coordinate 0 0, Coordinate (length (head lins) - 1) (length lins - 1)) (concat lins)
  let (Coordinate xmin ymin, Coordinate xmax ymax) = bounds array
  print $ maximum (map (getNumberOfEnergisedSquares array East . Coordinate xmin) [ymin..ymax] ++
              map (getNumberOfEnergisedSquares array West . Coordinate xmax) [ymin..ymax] ++
              map (\x -> getNumberOfEnergisedSquares array South (Coordinate x ymin)) [xmin..xmax] ++
              map (\x -> getNumberOfEnergisedSquares array North (Coordinate x ymax)) [xmin..xmax])


getNumberOfEnergisedSquares :: Array Coordinate BeamComponent -> Direction -> Coordinate -> Int
getNumberOfEnergisedSquares array direction coordinate = length $ nub $ getAllCoordinates $ fst $ getPath array coordinate direction empty

getPath :: Array Coordinate BeamComponent -> Coordinate -> Direction -> Set (Coordinate, Direction)-> (PathStep, Set (Coordinate, Direction))
getPath array coordinate _ seen
    | not (inRange (bounds array) coordinate) = (End, seen)
getPath _ coordinate direction seen
    | member (coordinate,direction) seen = (Loop, seen)
getPath array coordinate direction seen
  | component == None ||
    (component == HorizonalSplitter && (direction == East || direction == West)) ||
    (component == VerticalSplitter && (direction == North || direction == South))
      = let output = nextStepInDirection direction
        in (SingleStep coordinate (fst output), snd output)
  | component == DownwardMirror || component == UpwardMirror
      = let output = nextStepInDirection mirroredDirection
        in (SingleStep coordinate (fst output), snd output)
  | component == VerticalSplitter
      = let northOutput = nextStepInDirection North
            southOutput = nextStep (move South coordinate) South (insert (coordinate,direction) (snd northOutput))
        in (SplitStep coordinate (fst northOutput) (fst southOutput), snd southOutput)
  | component == HorizonalSplitter
      = let eastOutput = nextStepInDirection East
            westOutput = nextStep (move West coordinate) West (insert (coordinate,direction) (snd eastOutput))
        in (SplitStep coordinate (fst eastOutput) (fst westOutput), snd westOutput)

  where component = array ! coordinate
        mirroredDirection = mirrorDirection direction component
        nextStep = getPath array
        (nextStepInDirection) dir = nextStep (move dir coordinate) dir (insert (coordinate,direction) seen)
mirrorDirection :: Direction -> BeamComponent -> Direction
mirrorDirection North DownwardMirror = West
mirrorDirection East DownwardMirror = South
mirrorDirection South DownwardMirror = East
mirrorDirection West DownwardMirror = North
mirrorDirection direction UpwardMirror = opposite $ mirrorDirection direction DownwardMirror

getAllCoordinates :: PathStep -> [Coordinate]
getAllCoordinates End = []
getAllCoordinates Loop = []
getAllCoordinates (SingleStep loc nxt) = loc:getAllCoordinates nxt
getAllCoordinates (SplitStep loc nxtLft nxtRght) = loc:getAllCoordinates nxtLft ++ getAllCoordinates nxtRght

opposite :: Direction -> Direction
opposite direction = toEnum $ (fromEnum direction + 2) `mod` 4

parseBeamComponent :: Char -> BeamComponent
parseBeamComponent '-' = HorizonalSplitter
parseBeamComponent '|' = VerticalSplitter
parseBeamComponent '\\' = DownwardMirror
parseBeamComponent '/' = UpwardMirror
parseBeamComponent '.' = None

move :: Direction -> Coordinate -> Coordinate
move North (Coordinate x y) = Coordinate x (y - 1)
move East (Coordinate x y) = Coordinate (x + 1) y
move South (Coordinate x y) = Coordinate x (y + 1)
move West (Coordinate x y) = Coordinate (x - 1) y

