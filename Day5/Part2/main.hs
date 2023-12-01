import Data.List (find)
data MapRange = MapRange { sourceStart :: Integer
                         , sourceEnd :: Integer
                         , shiftToDestination :: Integer
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


mapAllSeeds :: [Integer] -> [[MapRange]] -> [Integer]
mapAllSeeds seeds mapRanges = map (`applyAllMaps` mapRanges) seeds

applyAllMaps :: Integer -> [[MapRange]] -> Integer
applyAllMaps = foldl mapToDestinationFromSet


parseSeeds :: [[String]] -> [Integer]
parseSeeds (seedsString:_) = concatMap mapSeedRange  (pairElements (map read (words (dropWhile (/= ' ') (head seedsString)))) [])

mapSeedRange :: (Integer,Integer) -> [Integer]
mapSeedRange (start,length) = [start..start+length]

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

mapToDestination :: Integer -> Maybe MapRange -> Integer
mapToDestination source (Just MapRange { shiftToDestination = shift}) = source + shift
mapToDestination source Nothing = source

sourceInMap :: Integer -> MapRange -> Bool
sourceInMap source MapRange { sourceStart = start, sourceEnd = end} = source >= start && source <= end

mapToDestinationFromSet :: Integer -> [MapRange] -> Integer
mapToDestinationFromSet source mapRanges = mapToDestination source (find (sourceInMap source) mapRanges)

pairElements :: [b] -> [(b, b)] -> [(b, b)]
pairElements list acc =
  if length list < 2
    then acc
    else pairElements (drop 2 list) (acc ++ [(head list, list !! 1)])



