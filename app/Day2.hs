module Main where
import Data.List
import Data.Maybe (fromMaybe)
import System.Environment (getProgName)


-- 1. Parse game lines
-- 2. Get Game ID as int and each cube set per game
-- 3. Sum cube sets by color
-- 4. Check sums against bag condition

data CubeSet = CubeSet { red :: Int
                       , green :: Int
                       , blue :: Int
                       } deriving (Show, Ord, Eq)


type Game = [CubeSet]


condition :: CubeSet
condition = CubeSet {red=12,green=13,blue=14}


--parseGame :: String -> [CubeSet]


splitString :: Char -> String -> [String]
splitString delim str
    | delim `elem` str = let (first, rest) = span (/=delim) str
                         in first : splitString delim (dropWhile (==delim) rest)
    | otherwise = [str]


parseGameId :: String -> Int
parseGameId str = read strId :: Int
    where        
        [_, strId] = splitString ' ' str


safeLookup :: Eq a => a -> [(a, Int)] -> Int
safeLookup x lookupList = fromMaybe 0 $ lookup x lookupList


parseCubeSet :: String -> CubeSet
parseCubeSet str =
    CubeSet { red=safeLookup "red" parsed
            , green=safeLookup "green" parsed
            , blue=safeLookup "blue" parsed
            }
    where
        splitted = splitString ',' str
        parsed = map ((\[a, b] -> (b, read a :: Int)) . words) splitted 
        
            
parseGame :: String -> (Int, [CubeSet])
parseGame str = (gameId, cubeSets)
    where
        [title, gameStr] = splitString ':' str
        gameId = parseGameId title
        cubeSets = map parseCubeSet $ splitString ';' gameStr


isPossibleSet :: CubeSet -> CubeSet -> Bool
isPossibleSet condition cubeSet = and checks
    where
        checks = [ red cubeSet <= red condition
                 , green cubeSet <= green condition
                 , blue cubeSet <= blue condition 
                 ]


resolveGame :: (Int, [CubeSet]) -> (Int, Bool)
resolveGame (gameId, game) = (gameId, all (isPossibleSet condition) game)


maxCubeSet :: CubeSet -> CubeSet -> CubeSet
maxCubeSet a b = CubeSet { red = max (red a) (red b)
                         , green = max (green a) (green b)
                         , blue = max (blue a) (blue b)
                         }


minPossible :: [CubeSet] -> CubeSet
minPossible = foldl1 maxCubeSet


powerCubeSet :: CubeSet -> Int
powerCubeSet a = red a * green a * blue a


main :: IO ()
main = do
    input <- readFile "data/day2/input2.txt"
    let games = map (minPossible . snd . parseGame) (lines input)
        gamePowers = map powerCubeSet games
    mapM_ putStrLn [show $ sum gamePowers]


