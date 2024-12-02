import Data.Map (Map, empty, insert, lookup)
import Prelude hiding (lookup)

main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let parsedLines = map parseLines lins
  let numberOfArrangements = map (`generatePossibleArrangements` empty) parsedLines
  print $ map fst numberOfArrangements
  print $ sum $ map fst numberOfArrangements

generatePossibleArrangements :: (String, [Int]) -> Map (String, [Int]) Int -> (Int, Map (String, [Int]) Int)
generatePossibleArrangements stringSpringArray resultMap
  | Just result <- lookup stringSpringArray resultMap = (result, resultMap)
generatePossibleArrangements ([], []) resultMap = (1, resultMap)
generatePossibleArrangements ([], springMap) resultMap = (0, resultMap)
generatePossibleArrangements (str, []) resultMap
  | '#' `elem` str = (0, insert (str, []) 0 resultMap)
  | otherwise = (1, insert (str, []) 1 resultMap)
generatePossibleArrangements ('#' : str, firstSpring : springs) resultMap
  | (length springString == firstSpring)
      && all (\x -> x == '#' || x == '?') springString
      && (end == "." || end == "") =
        (fst result, uncurry (insert ('#' : str, firstSpring : springs)) result)
  | otherwise = (0, insert ('#' : str, firstSpring : springs) 0 resultMap)
  where
    springString = take firstSpring ('#' : str)
    end = case take 1 (drop firstSpring ('#' : str)) of
      "?" -> "."
      str -> str
    result = generatePossibleArrangements (drop firstSpring str, springs) resultMap
generatePossibleArrangements ('.' : str, springs) resultMap =
  (fst result, uncurry (insert ('.':str,springs)) result)
  where result = generatePossibleArrangements (str, springs) resultMap
generatePossibleArrangements ('?' : str, springs) resultMap =
  (fst springResult + fst emptyResult, uncurry (insert ('.' : str, springs)) emptyResult)
  where springResult = generatePossibleArrangements ('#' : str, springs) resultMap
        emptyResult = generatePossibleArrangements ('.' : str, springs) (uncurry (insert ('#' : str, springs)) springResult)

parseLines :: String -> (String, [Int])
parseLines line = (repeatList 5 "?" str, repeatList 5 [] (map read (splitOn ',' springMap)))
  where
    (str, _ : springMap) = span (/= ' ') line

splitOn :: Char -> String -> [String]
splitOn char [] = []
splitOn char str = num : splitOn char (drop 1 xs)
  where
    (num, xs) = span (/= char) str

repeatList :: Int -> [a] -> [a] -> [a]
repeatList n separator list = take (n * length list + ((n - 1) * length separator)) (cycle (list ++ separator))