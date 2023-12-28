import Data.List

data Card
  = Jack
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Queen
  | King
  | Ace
  deriving (Eq, Ord, Enum, Show)

data HandType = HighCard | Pair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving (Eq, Ord, Enum, Show)

data Hand = Hand {cards :: [Card]} deriving (Eq, Show)

instance Ord Hand where
  compare a b
    | getHandType a > getHandType b = GT
    | getHandType a < getHandType b = LT
    | cards a > cards b = GT
    | cards a < cards b = LT
    | otherwise = EQ

type Bet = Int

main = do
  contents <- readFile "input.txt"
  let scores = zipWith calculateScore (sort (parseLines (lines contents))) [1 ..]
  print (sum scores)

calculateScore :: Num a1 => (a2, a1) -> a1 -> a1
calculateScore (hand, bet) rank = bet * rank

parseLines :: [String] -> [(Hand, Bet)]
parseLines = map parseLine

parseLine :: String -> (Hand, Bet)
parseLine string =
  let (handString, _ : betString) = span (/= ' ') string
   in (parseHand handString, read betString)

parseHand :: String -> Hand
parseHand string = Hand (map parseCard string)

parseCard :: Char -> Card
parseCard char
  | char == 'T' = Ten
  | char == 'J' = Jack
  | char == 'Q' = Queen
  | char == 'K' = King
  | char == 'A' = Ace
  | otherwise = toEnum (read [char] - 1)

getHandType :: Hand -> HandType
getHandType hand
  | head numberSetSizes == 5 = FiveOfAKind
  | head numberSetSizes == 4 = FourOfAKind
  | head numberSetSizes == 3 && numberSetSizes !! 1 == 2 = FullHouse
  | head numberSetSizes == 3 = ThreeOfAKind
  | head numberSetSizes == 2 && numberSetSizes !! 1 == 2 = TwoPair
  | head numberSetSizes == 2 = Pair
  | otherwise = HighCard
  where
    numberSetSizes = getNumberSetSizes hand

getNumberSetSizes :: Hand -> [Int]
getNumberSetSizes hand =
  let cardsWithoutJacks = filter (/= Jack) (cards hand)
      numberOfJacks = length (cards hand) - length cardsWithoutJacks
      groupingsWithoutJacks = sortOn negate (map length (group (sort cardsWithoutJacks)))
   in addJacksToLargestGrouping groupingsWithoutJacks numberOfJacks

addJacksToLargestGrouping :: [Int] -> Int -> [Int]
addJacksToLargestGrouping (x:xs) n = x+n:xs
addJacksToLargestGrouping [] n = [n]


