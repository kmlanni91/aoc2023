module Main where
import Data.List
import Data.Ord


data HandType = FiveOfAKind
                | FourOfAKind
                | FullHouse
                | ThreeOfAKind
                | TwoPair
                | OnePair
                | HighCard deriving (Show, Eq, Ord)


parseHandType :: String -> HandType
parseHandType str = case cardGroups of [5] -> FiveOfAKind
                                       [4, 1] -> FourOfAKind
                                       [3, 2] -> FullHouse
                                       [3, 1, 1] -> ThreeOfAKind
                                       [2, 2, 1] -> TwoPair
                                       [2, 1, 1, 1] -> OnePair
                                       [1, 1, 1, 1, 1] -> HighCard
    where
        groups = group $ sort str
        jokers = filter (elem 'J') groups
        numJokers = case jokers of [] -> 0
                                   _ -> length $ head jokers

        otherLengths = sortBy (comparing Down) . map length $ filter (notElem 'J') groups
        cardGroups = case otherLengths of [] -> [numJokers]
                                          x:xs -> (x + numJokers):xs


parseCardValue :: Char -> Int
parseCardValue 'A' = 13
parseCardValue 'K' = 12
parseCardValue 'Q' = 11
parseCardValue 'T' = 10
parseCardValue '9' = 9
parseCardValue '8' = 8
parseCardValue '7' = 7
parseCardValue '6' = 6
parseCardValue '5' = 5
parseCardValue '4' = 4
parseCardValue '3' = 3
parseCardValue '2' = 2
parseCardValue 'J' = 1

parseHandValues :: String -> [Int]
parseHandValues = map parseCardValue


parseLine :: String -> (String, Int)
parseLine line = (hand, bid)
    where
        [hand, bidStr] = words line
        bid = read bidStr


compareHand :: String -> String -> Ordering
compareHand left right = case typeOrdering of EQ -> compare (parseHandValues left) (parseHandValues right)
                                              _ -> typeOrdering
    where
        typeOrdering = compare (Down $ parseHandType left) (Down $ parseHandType right)


main :: IO ()
main = do
    input <- readFile "data/day7/input.txt"
    let parsed = (map parseLine . lines) input
    let ordered = sortBy (\(a, b) (c, d) -> compareHand a c) parsed
    let winnings = zipWith (\a b -> snd a * b) ordered [1..]
    print $ sum winnings
