module Main where
import Data.List
import Data.Char (isDigit)


type LineGroup = (String, String, String)


pullLines :: (String, String, [String]) -> Maybe (LineGroup, (String, String, [String]))
pullLines (_, "", []) = Nothing
pullLines (lag, current, []) = Just ((lag, current, ""), (current, "", []))
pullLines  (lag, current, x:xs) = Just ((lag, current, x), (current, x, xs))


findInts :: [(Char, Int)] -> [[(Char, Int)]]
findInts [] = []
findInts chars = case result of ([], []) -> []
                                (val, []) -> [val]
                                (val, recursed) -> val:recursed
    where
        prepped = dropWhile (\(a, b) -> (not . isDigit) a) chars
        (val', leftover) = span (\(a, b) -> isDigit a) prepped 
        recursed' = findInts leftover
        result = (val', recursed')


parsePossiblePart :: [(Char, Int)] -> (Int, [Int])
parsePossiblePart chars = (read part, indexes)
    where
    (part, indexes) = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], []) chars


isPart :: LineGroup -> (Int, [Int]) -> Bool
isPart (lag, current, lead) (part, indexes) = hasPrev || hasCurrent || hasNext
    where
        isSymbol x = not (isDigit x || x == '.')
        findSymbols xs = map fst $ filter snd $ zip [0..] $ map isSymbol xs
        isAdjacent ls x = x `elem` ls || x - 1 `elem` ls || x + 1 `elem` ls 
        hasPrev = any (isAdjacent (findSymbols lag)) indexes
        hasCurrent = any (isAdjacent (findSymbols current)) indexes
        hasNext = any (isAdjacent (findSymbols lead)) indexes



findParts :: LineGroup -> [Int]
findParts lineGroup = map fst (filter (isPart lineGroup) possibleParts) --map fst $ filter (isPart lineGroup) possibleParts
    where
        (lag, current, lead) = lineGroup
        possibleParts = map parsePossiblePart $ findInts $ zip current [0..]


main :: IO()
main = do
    input <- readFile "data/day3/input.txt"
    print $ sum $ concatMap findParts $ filter (\(a,b,c) -> b /= "") $ unfoldr pullLines ("", "", lines input)

