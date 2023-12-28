main = do
  contents <- readFile "input.txt"
  let output = processAllLines (lines contents)
  print (sum output)

processAllLines lines = [parseGameNumber x | x <- lines, maxColourValue x "red" <= 12, maxColourValue x "green" <= 13, maxColourValue x "blue" <= 14]

maxColourValue line colour =
  maximum (map (read . fst) (filter (\pair -> snd pair == colour) (splitLineIntoPart line)))

splitLineIntoPart line = pairElements (wordsWhen (== ' ') (filter (`notElem` [':', ';', ',']) (dropWhile (/= ':') line))) []

pairElements :: [[b]] -> [([b], [b])] -> [([b], [b])]
pairElements list acc =
  if length list < 2
    then acc
    else pairElements (drop 2 list) (acc ++ [(head list, list !! 1)])

wordsWhen :: (Char -> Bool) -> [Char] -> [[Char]]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

parseGameNumber :: [Char] -> Integer
parseGameNumber line =
    read (tail (dropWhile (/= ' ') (takeWhile (/= ':') line)))