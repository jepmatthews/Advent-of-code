import Data.Array (Array, array, assocs, listArray, (!), (//))
import Data.Char (ord)

data Step = Add {box :: Int, label :: String, focalLength :: Int} | Remove {box :: Int, label :: String} deriving (Show, Eq)

data Lens = Lens {fLength :: Int, lab :: String} deriving (Show, Eq)

type Box = [Lens]

main = do
  contents <- readFile "input.txt"
  let steps = map parseStep $ splitOn ',' contents
  let endState = foldl operateStepOnArray emptyBoxArray steps
  let focusingPower = sum $ map (uncurry getBoxContribution) (assocs endState)
  print focusingPower

emptyBoxArray :: Array Int Box
emptyBoxArray = listArray (0, 256) (repeat [])

calculateHash :: [Int] -> Int
calculateHash = foldl (\currentValue x -> ((currentValue + x) * 17) `mod` 256) 0

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn match [] = []
splitOn match list = start : splitOn match (drop 1 xs)
  where
    (start, xs) = span (/= match) list

getBoxContribution :: Int -> Box -> Int
getBoxContribution boxNo box = sum $ zipWith (getLensContribution boxNo) [1 ..] box

getLensContribution :: Int -> Int -> Lens -> Int
getLensContribution boxNo positionInBox lens = (boxNo + 1) * positionInBox * fLength lens

operateStepOnArray :: Array Int Box -> Step -> Array Int Box
operateStepOnArray array step = array // [(boxNo, operateStepOnBox (array ! boxNo) step)]
  where
    boxNo = box step

operateStepOnBox :: Box -> Step -> Box
operateStepOnBox box (Add _ l fL)
  | (lens : afterLens) <- postLens = preLens ++ [newLens] ++ afterLens
  | otherwise = box ++ [newLens]
  where
    (preLens, postLens) = span (\lens -> lab lens /= l) box
    newLens = Lens fL l
operateStepOnBox box (Remove _ l)
  | (_ : afterLens) <- postLens = preLens ++ afterLens
  | otherwise = box
  where
    (preLens, postLens) = span (\lens -> lab lens /= l) box

parseStep :: String -> Step
parseStep str
  | ('=' : n) <- remainder = Add (calculateHash $ map ord label) label (read n)
  | remainder == "-" = Remove (calculateHash $ map ord label) label
  | otherwise = error $ "Error parsing string: " ++ str
  where
    (label, remainder) = span (\x -> x /= '=' && x /= '-') str