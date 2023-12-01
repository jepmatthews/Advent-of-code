import Data.List (find, isPrefixOf)

main = do
  contents <- readFile "input.txt"
  let lin = map processLine (lines contents)
  print (sum lin)

processLine str = read (firstAndLastElements (convertTextToNumbers str "")) :: Integer

firstAndLastElements [] = []
firstAndLastElements ls = head ls : [last ls]

convertTextToNumbers str acc =
  if null str
    then acc
    else case find (\text -> fst text `isPrefixOf` str) textNumberDigits of
      Nothing -> convertTextToNumbers (tail str) acc
      Just (txt, value) -> convertTextToNumbers (tail str) (acc ++ value)

textNumberDigits :: [(String, String)]
textNumberDigits =
  [ ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9"),
    ("1", "1"),
    ("2", "2"),
    ("3", "3"),
    ("4", "4"),
    ("5", "5"),
    ("6", "6"),
    ("7", "7"),
    ("8", "8"),
    ("9", "9")
  ]