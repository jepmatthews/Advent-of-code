import Data.Char (ord)

main = do
  contents <- readFile "input.txt"
  print contents
  let steps = splitOn ',' contents
  let hashes = map (calculateHash . map ord) steps
  print hashes
  print $ sum hashes

calculateHash :: [Int] -> Int
calculateHash = foldl (\ currentValue x -> ((currentValue + x) * 17) `mod` 256) 0

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn match [] = []
splitOn match list = start : splitOn match (drop 1 xs)
  where
    (start, xs) = span (/= match) list