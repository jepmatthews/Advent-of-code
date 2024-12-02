main = do
  contents <- readFile "input.txt"
  let lins = lines contents
  let parsedLines = map parseLines lins
  let possibleArangements = map generatePossibleArrangements parsedLines
  let arrangementsThatFit = map (length . filterOnlyStringsThatFit) possibleArangements
  print arrangementsThatFit
  print $ sum arrangementsThatFit


stringFitsMap :: String -> [Int] -> Bool
stringFitsMap str springMap =
  filter (/= 0 ) (map length (splitOn '.' str)) == springMap

filterOnlyStringsThatFit :: ([String],[Int]) -> [String]
filterOnlyStringsThatFit (strings,springMap) = filter (`stringFitsMap` springMap) strings

generatePossibleArrangements :: (String,[Int]) -> ([String],[Int])
generatePossibleArrangements ([],springMap) = ([[]],springMap)
generatePossibleArrangements (char:str,springMap)
  | char == '?' = (map ('.' :) str' ++ map ('#' :) str',springMap)
  | otherwise = (map (char :) str',springMap)
  where (str',_) = generatePossibleArrangements (str,springMap)

parseLines :: String -> (String,[Int])
parseLines line = (str,map read (splitOn ',' springMap))
  where (str,_:springMap) = span (/= ' ') line


splitOn :: Char -> String -> [String]
splitOn char [] = []
splitOn char str = num : splitOn char (drop 1 xs)
  where (num,xs) = span (/= char) str