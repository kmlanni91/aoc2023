module Main where


type Card = ([Int], [Int])


parseInts:: String -> [Int]
parseInts "" = []
parseInts str = map read $ words str


parseCard:: String -> Card
parseCard line = (winners, picks)
    where
        noPrefix = drop 2 $ dropWhile (/=':') line
        (winnerStr, leftover) = span (/='|') noPrefix
        winners = parseInts winnerStr
        picks = parseInts $ drop 1 leftover


countMatches :: [Int] -> [Int] -> Int
countMatches winners picks = length matches
    where
        matches = filter (`elem` winners) picks


calculateScore:: Int -> Int
calculateScore 0 = 0
calculateScore 1 = 1
calculateScore n = 2 ^ (n - 1) 


getCopies :: [Card] -> Int -> [Card]
getCopies cards cardNum = original:copies
    where
        original = cards !! cardNum
        matches = uncurry countMatches original
        leftovers = drop cardNum cards
        copies = concatMap (getCopies cards) [cardNum + 1..cardNum + matches]



main :: IO ()
main = do
    input <- readFile "data/day4/input2.txt"
    let cards = map parseCard $ lines input
    print $ length $ concatMap (getCopies cards) [0..length cards - 1]
