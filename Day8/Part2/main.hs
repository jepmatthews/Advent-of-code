import Data.Map (fromList, Map, lookup, elems)
import Prelude hiding (Left, Right, lookup)
import Data.Maybe (fromJust)

data Turn = Left | Right deriving (Show, Eq)

data Cycle = Cycle {offset :: Int, len :: Int} deriving (Show, Eq)

data Node = Node {left :: String, right :: String, index :: String}
    deriving (Show, Eq)

main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let nodesStrings = drop 2 lins
  let path = cycle (parsePath (head lins))
  let nodes = fromList $ map initialiseNode nodesStrings
  let nodesStartWithA = filter (`indexEndsWith` 'A') (elems nodes)
  let cycles = map (getCycleOffsetAndLength nodes path) nodesStartWithA
  let result = head (filter (\n -> all (`lengthIsInCycle` n) cycles) (cycleList (head cycles)))
  print result



getCycleOffsetAndLength :: Map String Node -> [Turn] -> Node -> Cycle
getCycleOffsetAndLength nodeMap turns firstNode
    = Cycle (length toFirst) (length (takeWhile (`indexDoesntEndWith` 'Z') (tail remainder)) + 1)
        where route = navigatePath nodeMap firstNode turns
              (toFirst,remainder) = span (`indexDoesntEndWith` 'Z') route



navigatePath ::  Map String Node -> Node -> [Turn] -> [Node]
navigatePath nodeMap = scanl (navigateSingleTurn nodeMap)


navigateSingleTurn :: Map String Node -> Node -> Turn -> Node
navigateSingleTurn nodeMap node turn
    | turn == Left = fromJust (lookup (left node) nodeMap)
    | turn == Right = fromJust (lookup (right node) nodeMap)

initialiseNode :: String -> (String, Node)
initialiseNode string = (index,Node left right index)
    where index = take 3 string
          left = take 3 (drop 7 string)
          right = take 3 (drop 12 string)

lengthIsInCycle :: Cycle -> Int -> Bool
lengthIsInCycle cycle length = (length - offset cycle) `mod` len cycle == 0

cycleList :: Cycle -> [Int]
cycleList cycle = map (\x -> offset cycle + (len cycle * x)) [0..]

parsePath :: String -> [Turn]
parsePath = map parseTurn

indexEndsWith :: Node -> Char -> Bool
indexEndsWith (Node _ _ [_,_,x]) char = x == char

indexDoesntEndWith :: Node -> Char -> Bool
indexDoesntEndWith node char = not (indexEndsWith node char)

parseTurn :: Char -> Turn
parseTurn char
    | char == 'L' = Left
    | char == 'R' = Right
    | otherwise = error "Unexpected character"
