import System.IO

main = do
  contents <- readFile "input.txt"
  let lin = map processLine (lines contents)
  print (lin)

processLine str = read (firstAndLastElements (filterNonNumbers str)) :: Integer

firstAndLastElements [] = []
firstAndLastElements ls = head ls : [last ls]

filterNonNumbers str = [x | x <- str, x `elem` ['0' .. '9']]