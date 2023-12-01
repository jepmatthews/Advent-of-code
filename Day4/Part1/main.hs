import Data.List (intersect)

main = do
  contents <- readFile "input.txt"
  let lins = map processLine (lines contents)
  print (sum lins)

processLine line = calculateScore (uncurry intersect (splitLine line))

calculateScore [] = 0
calculateScore list = 2 ^ (length list - 1)

splitLine line = splitWinningFromHave (removeCardNumber line)

splitWinningFromHave line = (words (tail (dropWhile (/= '|') line)), words (takeWhile (/= '|') line))

removeCardNumber line = tail (dropWhile (/= ':') line)