import Data.Map (Map, fromList, (!))

data PartProperty = X | M | A | S deriving (Ord, Eq, Show)

data Rule
  = Comparison {target :: PartProperty, compType :: Ordering, compValue :: Int, result :: Result}
  | Always Result
  deriving (Ord, Eq, Show)

data Result = Forward String | Accept | Reject deriving (Ord, Eq, Show)

data Workflow = Workflow String [Rule] deriving (Ord, Eq, Show)

data Range = Range Int Int deriving (Ord, Eq, Show)

data Part = Part {x :: Range, m :: Range, a :: Range, s :: Range} deriving (Ord, Eq, Show)

main = do
  contents <- readFile "input.txt"
  let (workflowStrs, _ : partStrs) = span (/= "") $ lines contents
  let workflows = fromList $ map ((\(Workflow key a) -> (key, Workflow key a)) . parseWorkflowLine) workflowStrs
  print $ splitPartRange GT (-7000) initialPart X 
  print $ getNumberOfAccepted workflows initialPart "in"

initialPart = Part initialRange initialRange initialRange initialRange
initialRange = Range 1 4000

getNumberOfAccepted :: Map String Workflow -> Part -> String -> Int
getNumberOfAccepted workflowMap part key
  | not $ allRangesValid part = 0
  | otherwise = getNumberOfAcceptedForRules workflowMap rules part
  where
    (Workflow _ rules) = workflowMap ! key


getNumberOfAcceptedForRules :: Map String Workflow -> [Rule] -> Part -> Int
getNumberOfAcceptedForRules _ [] _ = 0
getNumberOfAcceptedForRules _ _ part
  | not $ allRangesValid part = 0
getNumberOfAcceptedForRules workflowMap (rule:rules) part
  | Always result <- rule = getNumberOfAcceptedForResult workflowMap part result
  | Comparison targ comp val result <- rule =
      let (matchesPart,otherPart) = splitPartRange comp val part targ
      in getNumberOfAcceptedForResult workflowMap matchesPart result + getNumberOfAcceptedForRules workflowMap rules otherPart


splitPartRange :: Ordering -> Int -> Part -> PartProperty -> (Part,Part)
splitPartRange GT val part prop = (updatePartProperty part prop (Range adjustedVal maxRange), updatePartProperty part prop (Range minRange (min maxRange (adjustedVal - 1))))
  where
    (Range minRange maxRange) = getPartProperty part prop
    adjustedVal = max minRange (val + 1)
splitPartRange LT val part prop = (updatePartProperty part prop (Range minRange adjustedVal), updatePartProperty part prop (Range (max minRange (adjustedVal + 1)) maxRange))
  where
    (Range minRange maxRange) = getPartProperty part prop
    adjustedVal = min maxRange (val - 1)

updatePartProperty :: Part -> PartProperty -> Range -> Part
updatePartProperty (Part _ m a s) X range = Part range m a s
updatePartProperty (Part x _ a s) M range = Part x range a s
updatePartProperty (Part x m _ s) A range = Part x m range s
updatePartProperty (Part x m a _) S range = Part x m a range

getNumberOfAcceptedForResult :: Map String Workflow -> Part -> Result -> Int
getNumberOfAcceptedForResult _ part _
  | not $ allRangesValid part = 0
getNumberOfAcceptedForResult _ part Accept = getNumberOfCombinations part
getNumberOfAcceptedForResult _ _ Reject = 0
getNumberOfAcceptedForResult workflowMap part (Forward nextKey) = getNumberOfAccepted workflowMap part nextKey



getNumberOfCombinations :: Part -> Int
getNumberOfCombinations (Part xRange mRange aRange sRange) = rangeSize xRange * rangeSize mRange * rangeSize aRange * rangeSize sRange

isValidRange :: Range -> Bool
isValidRange (Range min max) = min <= max

rangeSize :: Range -> Int
rangeSize (Range min max) = max - min + 1

allRangesValid :: Part -> Bool
allRangesValid (Part xRange mRange aRange sRange) = isValidRange xRange && isValidRange mRange &&isValidRange aRange &&isValidRange sRange

getPartProperty :: Part -> PartProperty -> Range
getPartProperty part X = x part
getPartProperty part M = m part
getPartProperty part A = a part
getPartProperty part S = s part

parseWorkflowLine :: String -> Workflow
parseWorkflowLine str = Workflow key (map parseRule ruleStrings)
  where
    (key, flow) = span (/= '{') str
    ruleStrings = splitOn ',' $ takeWhile (/= '}') $ drop 1 flow

parseRule :: String -> Rule
parseRule str
  | ':' `elem` str = Comparison targ comp (read value) result
  | otherwise = Always (readResult str)
  where
    (cond, _ : resultStr) = span (/= ':') str
    targChar : compChar : value = cond
    comp = readComparison compChar
    targ = readTarget targChar
    result = readResult resultStr

readComparison :: Char -> Ordering
readComparison '>' = GT
readComparison '<' = LT
readComparison char = error $ "Unexpected comparison " ++ [char]

readTarget :: Char -> PartProperty
readTarget 'x' = X
readTarget 'm' = M
readTarget 'a' = A
readTarget 's' = S
readTarget char = error $ "Unexpected part property " ++ [char]

readResult :: String -> Result
readResult "A" = Accept
readResult "R" = Reject
readResult str = Forward str

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn char [] = []
splitOn char str = start : splitOn char (drop 1 xs)
  where
    (start, xs) = span (/= char) str