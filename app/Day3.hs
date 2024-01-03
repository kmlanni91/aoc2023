module Main where
import Data.List
import Data.Char (isDigit)
import GHC.Read (list)
import Data.Ix (Ix(range))
import Foreign (throwIfNeg)


type LineGroup = (String, String, String)
type PartIndexes = [Int]
type PartValue = Int
type PossiblePart = (PartValue, PartIndexes)
type PossiblePartGroup = ([PossiblePart], [PossiblePart], [PossiblePart])



pullLines :: (String, String, [String]) -> Maybe (LineGroup, (String, String, [String]))
pullLines (_, "", []) = Nothing
pullLines (lag, current, []) = Just ((lag, current, ""), (current, "", []))
pullLines  (lag, current, x:xs) = Just ((lag, current, x), (current, x, xs))


-- Groups consecutive ints into a list
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


-- Find Int representing possible part and its character indexes from the line it belongs to.
parsePossiblePart :: [(Char, Int)] -> PossiblePart
parsePossiblePart chars = (read part, indexes)
    where
    (part, indexes) = foldr (\(a,b) (as,bs) -> (a:as, b:bs)) ([], []) chars


-- Check if a '*' is in a possible parts adjacent range
isAdjacent :: [Int] -> Int -> Bool
isAdjacent ls x = x `elem` ls || x - 1 `elem` ls || x + 1 `elem` ls 


isPart :: LineGroup -> PossiblePart -> Bool
isPart (lag, current, lead) (part, indexes) = hasPrev || hasCurrent || hasNext
    where
        isSymbol x = not (isDigit x || x == '.')
        findSymbols xs = map fst $ filter snd $ zip [0..] $ map isSymbol xs
        hasPrev = any (isAdjacent (findSymbols lag)) indexes
        hasCurrent = any (isAdjacent (findSymbols current)) indexes
        hasNext = any (isAdjacent (findSymbols lead)) indexes


findParts :: LineGroup -> PossiblePartGroup
findParts (lag, current, lead) = (parsePossibleParts lag, parsePossibleParts current, parsePossibleParts lead)
    where
        parsePossibleParts strLine = map parsePossiblePart $ findInts $ zip strLine [0..]



findPossibleGears :: String -> [Int]
findPossibleGears str = map fst $ filter ((=='*') . snd) $ zip [0..] str


matchLine :: [[Int]] -> Int -> Bool
matchLine parts gear = any (elem gear) parts


--isGear :: ([[Int]], [[Int]], [[Int]]) -> [Int] -> Bool
--isGear (lag, current, lead) gears = (length matches) >= 2
--    where
--        prevMatches = map (matchLine lag) gears
--        currentMatches = map (matchLine current) gears
--        nextMatches = map (matchLine lead) gears
        -- need to switch this to separately apply to each gear       
        


-- Find adjacent parts to current '*' in lag, current, lead of possible parts
findAdjacentParts :: PossiblePartGroup -> Int -> [PartValue]
findAdjacentParts (lag, current, lead) gearIndex = concat [lagMatches, currentMatches, leadMatches]
    where
        find = map fst . filter (flip isAdjacent gearIndex . snd)
        lagMatches = find lag
        currentMatches = find current
        leadMatches = find lead



generateLineGroups :: [String] -> [LineGroup]
generateLineGroups inputLines = filter (\(a,b,c) -> b /= "") $ unfoldr pullLines ("", "", inputLines)


mapPartGroup :: PossiblePartGroup -> [Int] -> [(PossiblePartGroup, Int)]
mapPartGroup pgroup = map group
    where
        group gear = (pgroup, gear)



main :: IO()
main = do
    input <- readFile "data/day3/input2.txt"
    let strLines = lines input
    let partGroups = map findParts $ generateLineGroups strLines
    let gearLines = map findPossibleGears strLines
    let possibleGears = concatMap (map $ uncurry findAdjacentParts) $ zipWith mapPartGroup partGroups gearLines
    print $ sum $ map product $ filter (\a -> 2 == length a) possibleGears

