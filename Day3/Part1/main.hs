import Data.Char (isDigit)
import Data.List (intersect)

main = do
  contents <- readFile "input.txt"
  let output = partNumbersOfPartsAdjacentToSymbols (addCoordinates (lines contents))
  print (sum output)

addCoordinates lines = zipWith addCoordinate lines [0 ..]

addCoordinate line lineNumber = zipWith (curry (combine lineNumber)) [0 ..] line

partNumbersOfPartsAdjacentToSymbols charsAndCoordinateLines =
  let symbolCoordinates = getSymbolCoordinates charsAndCoordinateLines
      partNumbers = getAllPartNumbers charsAndCoordinateLines
   in [getNumber x | x <- partNumbers, partNumberIsNextToSymbol x symbolCoordinates]

partNumberIsNextToSymbol partNumber symbols = not (null (getAdjacentCoordinates partNumber `intersect` symbols))

getNumber partNumber = read (map third partNumber) :: Integer

getCoordinate (x, y, _) = (x, y)

getAdjacentCoordinates number =
  let lineNumber = first $ head number
      startIndex = second $ head number
      endIndex = second $ last number
   in cartProd [lineNumber - 1 .. lineNumber + 1] [startIndex - 1 .. endIndex + 1]

getAllPartNumbers = concatMap (filter (not . null) . findPartNumbers)

findPartNumbers line = takeNumber line []

takeNumber [] acc = acc
takeNumber line acc =
  let number = takeWhile (charMeetsCondition isDigit) line
   in skipNonNumber (drop (length number) line) (number : acc)

skipNonNumber [] acc = acc
skipNonNumber line acc = takeNumber (dropWhile (charMeetsCondition (not . isDigit)) line) acc

charMeetsCondition condition t = condition (third t)

getSymbolCoordinates charsAndCoordinateLines = map getCoordinate (filter isSymbol (concat charsAndCoordinateLines))

isSymbol t =
  let char = third t
   in char /= '.' && not (isDigit char)

first (a, _, _) = a

second (_, a, _) = a

third (_, _, a) = a

combine a (b, c) = (a, b, c)

cartProd xs ys = [(x, y) | x <- xs, y <- ys]