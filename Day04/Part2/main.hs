import Data.List (genericLength, intersect)

main = do
  contents <- readFile "input.txt"
  let lins = zip [0 ..] (map scoreLine (lines contents))
  let output = numberOfCardsStart lins
  print (sum (map third output))

numberOfCardsStart list = numberOfCards2 list []

numberOfCards2 [] acc = acc
numberOfCards2 list acc = numberOfCards2 (tail list) (acc ++ [(fst (head list), snd (head list), numberOfCardsForRow (head list) acc + 1)])

numberOfCardsForRow tuple acc = sum (map third (filter (`newListFilter` fst tuple) acc))

listFilter tuple index = ((index - fst tuple) <= snd tuple) && (index > fst tuple)

newListFilter tuple index = (index - first tuple) <= second tuple

scoreLine line = genericLength (uncurry intersect (splitLine line))

splitLine line = splitWinningFromHave (removeCardNumber line)

splitWinningFromHave line = (words (tail (dropWhile (/= '|') line)), words (takeWhile (/= '|') line))

removeCardNumber line = tail (dropWhile (/= ':') line)

third (_, _, a) = a

first (a, _, _) = a

second (_, a, _) = a