module Main where


parseInts:: String -> [Int]
parseInts "" = []
parseInts str = map read $ words str


parseCard:: String -> ([Int], [Int])
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


main :: IO ()
main = do
    input <- readFile "data/day4/input.txt"
    print $ sum $ map (calculateScore . uncurry countMatches . parseCard) $ lines input 
