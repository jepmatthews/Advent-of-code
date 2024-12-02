import Data.List (find)
data MapRange = MapRange { sourceStart :: Int
                         , sourceEnd :: Int
                         , shiftToDestination :: Int
                         } deriving (Show)


main = do
  contents <- readFile "input.txt"
  let seeds = processAll (lines contents)
  print (minimum seeds)

processAll lines = 
    let mapsSplit = splitIntoMapStrings lines []
        seeds = parseSeeds mapsSplit
        maps = parseAllMapStrings mapsSplit
    in 
        mapAllSeeds seeds maps


mapAllSeeds :: [Int] -> [[MapRange]] -> [Int]
mapAllSeeds seeds mapRanges = map (`applyAllMaps` mapRanges) seeds

applyAllMaps :: Int -> [[MapRange]] -> Int
applyAllMaps = foldl mapToDestinationFromSet


parseSeeds :: [[String]] -> [Int]
parseSeeds (seedsString:_) = map read (words (dropWhile (/= ' ') (head seedsString)))

parseAllMapStrings :: [[String]] -> [[MapRange]]
parseAllMapStrings (_:rawMaps) = map parseMapRangeSet rawMaps

parseMapRangeSet :: [String] -> [MapRange]
parseMapRangeSet (_:lines) = map parseLineToMapRange lines
parseMapRangeSet _ = []

parseLineToMapRange :: String -> MapRange
parseLineToMapRange line = let [part1,part2,part3] = map read (words line)
    in MapRange{sourceStart = part2, sourceEnd = part2 + part3 - 1, shiftToDestination = part1 - part2}

splitIntoMapStrings :: [String] -> [[String]] -> [[String]]
splitIntoMapStrings [] acc = acc
splitIntoMapStrings lines acc =
    let map = takeWhile (not . null) lines
        remain = drop (length map + 1) lines
    in splitIntoMapStrings remain (acc ++ [map])

mapToDestination :: Int -> Maybe MapRange -> Int
mapToDestination source (Just MapRange { shiftToDestination = shift}) = source + shift
mapToDestination source Nothing = source

sourceInMap :: Int -> MapRange -> Bool
sourceInMap source MapRange { sourceStart = start, sourceEnd = end} = source >= start && source <= end

mapToDestinationFromSet :: Int -> [MapRange] -> Int
mapToDestinationFromSet source mapRanges = mapToDestination source (find (sourceInMap source) mapRanges)

