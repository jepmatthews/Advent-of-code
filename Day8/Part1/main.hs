import Data.Map (fromList, Map, lookup)
import Distribution.Compat.CharParsing (CharParsing(char))
import Prelude hiding (Left, Right, lookup)
import Data.Maybe (fromJust)

data Turn = Left | Right deriving (Show, Eq)

data Node = Node {left :: String, right :: String, index :: String}
    deriving (Show)

main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let nodesStrings = drop 2 lins
  let path = parsePath (head lins)
  let nodes = fromList $ map initialiseNode nodesStrings
  let pathToEnd = takeWhile (\node -> index node /= "ZZZ") (navigatePath nodes (fromJust (lookup "AAA" nodes)) (cycle path))
  print (length pathToEnd)


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

parsePath :: String -> [Turn]
parsePath = map parseTurn

parseTurn :: Char -> Turn
parseTurn char
    | char == 'L' = Left
    | char == 'R' = Right
    | otherwise = error "Unexpected character"
