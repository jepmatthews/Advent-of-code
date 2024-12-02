import Data.Array (listArray, Array, (!), bounds, assocs)
import Data.List (find, elemIndex, sort)
import Data.Maybe (fromJust)

data Pipe = Vertical | Horizontal | NE | NW | SW | SE | Ground | Start deriving (Show, Eq, Enum, Ord)
data Direction = North | East | South | West deriving (Show, Eq, Enum, Ord)

main = do
  contents <- readFile "input.txt"
  let lins = map (map parsePipe) (lines contents)
  let array = listArray ((0,0),(length lins - 1, length (head lins) - 1)) (concat (reverse lins))
  let startPosition = fst $ fromJust $ find ((== Start) . snd) (assocs array)
  let (secondPosition,entryDirection) = head $ cellsConnected array startPosition
  let perimeter = startPosition : takeWhile (/= startPosition) (followPath array secondPosition entryDirection)
  let mapWithJustPerimeter = removeNonPathFromArray array perimeter
  print $ length $ concatMap (filter id . mapToInsidePerimeter) mapWithJustPerimeter

  
mapToInsidePerimeter :: [Pipe] -> [Bool]
mapToInsidePerimeter pipes = map fst $ tail $ scanl isInsidePermieter (False,[]) pipes


isInsidePermieter :: (Bool,[Direction]) -> Pipe -> (Bool,[Direction])
isInsidePermieter (_,directions) Ground = (North `elem` directions && South `elem` directions,directions)
isInsidePermieter (_,directions) pipe = (False, symmetricDifference directions $ pipeOpenings pipe)


symmetricDifference :: Eq a => [a] -> [a] -> [a]
symmetricDifference xs ys = filter (`notElem` ys) xs  ++ filter (`notElem` xs) ys

followPath :: Array (Int,Int) Pipe -> (Int,Int) -> Direction -> [(Int,Int)]
followPath pipeArray position entryDirection = position : followPath pipeArray nextLocation newEntryDirection
  where (newEntryDirection,nextLocation) = takeStep pipeArray position entryDirection


takeStep :: Array (Int,Int) Pipe -> (Int,Int) -> Direction -> (Direction,(Int,Int))
takeStep pipeArray startCell entryDirection =
  (opposite exitDirection, moveDirection exitDirection startCell)
  where openings = connectedOpenings pipeArray startCell
        exitDirection = fromJust $ find (/= entryDirection) openings

removeNonPathFromArray :: Array (Int, Int) Pipe -> [(Int, Int)] -> [[Pipe]]
removeNonPathFromArray pipeArray perimeter =
  map (map (mapPipePartRemovingPerimeter pipeArray perimeter) . (\x' -> map (x',) [0..x])) [0..y]
  where ((_,_),(y,x)) = bounds pipeArray

mapPipePartRemovingPerimeter :: Array (Int, Int) Pipe -> [(Int,Int)] -> (Int,Int) -> Pipe
mapPipePartRemovingPerimeter  pipeArray permiter index
  | currentType == Start = pipeFromOpenings (connectedOpenings pipeArray index)
  | index `elem` permiter = currentType
  | otherwise = Ground
  where currentType = pipeArray ! index

pipeFromOpenings :: [Direction] -> Pipe
pipeFromOpenings openings
  | sortedOpenings == [North,South] = Vertical
  | sortedOpenings == [East,West] = Horizontal
  | sortedOpenings == [North,East] = NE
  | sortedOpenings == [North,West] = NW
  | sortedOpenings == [South,West] = SW
  | sortedOpenings == [East,South] = SE
  | otherwise = error $ show openings
  where sortedOpenings = sort openings


moveDirection :: Direction -> (Int,Int) -> (Int,Int)
moveDirection North (y,x) = (y+1,x)
moveDirection East (y,x) = (y,x+1)
moveDirection South (y,x) = (y-1,x)
moveDirection West (y,x) = (y,x-1)

pipeOpenings :: Pipe -> [Direction]
pipeOpenings Vertical = [North,South]
pipeOpenings Horizontal = [East,West]
pipeOpenings NE = [North,East]
pipeOpenings NW = [North,West]
pipeOpenings SW = [South,West]
pipeOpenings SE = [South,East]
pipeOpenings Ground = []
pipeOpenings Start = [North,East,South,West]

opposite :: Direction -> Direction
opposite North = South
opposite South = North
opposite East = West
opposite West = East

connectedOpenings :: Array (Int, Int) Pipe -> (Int, Int) -> [Direction]
connectedOpenings pipeArray cell =
  map (\(_,x) -> opposite x) (cellsConnected pipeArray cell)

cellsConnected :: Array (Int, Int) Pipe -> (Int, Int) -> [((Int, Int), Direction)]
cellsConnected pipeArray cell =
  filter (\(cell,direction) -> cellInBounds pipeArray cell && (direction `elem` pipeOpenings (pipeArray ! cell))) $
  map (\direction -> (moveDirection direction cell,opposite direction)) (pipeOpenings (pipeArray ! cell))

parsePipe :: Char -> Pipe
parsePipe char
  | char == '|' = Vertical
  | char == '-' = Horizontal
  | char == 'L' = NE
  | char == 'J' = NW
  | char == '7' = SW
  | char == 'F' = SE
  | char == '.' = Ground
  | char == 'S' = Start
  | otherwise = error "Error parsing"

cellInBounds :: Array (Int, Int) Pipe -> (Int, Int) -> Bool
cellInBounds array (y,x) = x >= xMin && x <= xMax && y >= yMin && y <= yMax
    where ((yMin,xMin),(yMax,xMax)) = bounds array