import Data.Map (Map, fromList, (!))

data PartProperty = X | M | A | S deriving (Ord, Eq, Show)

data Rule
  = Comparison {target :: PartProperty, compType :: Ordering, compValue :: Int, result :: Result}
  | Always Result
  deriving (Ord, Eq, Show)

data Result = Forward String | Accept | Reject deriving (Ord, Eq, Show)

data Workflow = Workflow String [Rule] deriving (Ord, Eq, Show)

data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Ord, Eq, Show)

main = do
  contents <- readFile "input.txt"
  let (workflowStrs, _ : partStrs) = span (/= "") $ lines contents
  let workflows = fromList $ map ((\(Workflow key a) -> (key, Workflow key a)) . parseWorkflowLine) workflowStrs
  let parts = map parsePart partStrs
  print $ sum $ map (getScore workflows) parts

getScore :: Map String Workflow -> Part -> Int
getScore workflowMap part
  | executeWorkflow workflowMap part "in" == Accept = xval + mval + aval + sval
  | otherwise = 0
  where
    (Part xval mval aval sval) = part

executeWorkflow :: Map String Workflow -> Part -> String -> Result
executeWorkflow workflowMap part key
  | Forward nextKey <- workflowResult = executeWorkflow workflowMap part nextKey
  | otherwise = workflowResult
  where
    (Workflow _ rules) = workflowMap ! key
    workflowResult = getWorkflowResult rules part

getWorkflowResult :: [Rule] -> Part -> Result
getWorkflowResult [] _ = error "Workflow should end with a terminal rule"
getWorkflowResult ((Always res) : rules) part = res
getWorkflowResult ((Comparison prop comp compVal res) : rules) part
  | meetsRule = res
  | otherwise = getWorkflowResult rules part
  where
    meetsRule = compare (getPartProperty part prop) compVal == comp

getPartProperty :: Part -> PartProperty -> Int
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

parsePart :: String -> Part
parsePart str = Part (read xstr) (read mstr) (read astr) (read sstr)
  where
    ['{' : 'x' : '=' : xstr, 'm' : '=' : mstr, 'a' : '=' : astr, 's' : '=' : sstr] = splitOn ',' $ takeWhile (/= '}') str

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn char [] = []
splitOn char str = start : splitOn char (drop 1 xs)
  where
    (start, xs) = span (/= char) str