module Main where
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)


data Position = Position Int Int deriving (Show) 

data Direction = West | East | North | South deriving (Show, Eq)

type GridItem = (Char, Position)

type Grid = [[GridItem]]


parseLine :: Int -> String -> [GridItem]
parseLine lineno line =  zipWith (\c i -> (c, Position i lineno)) line [0..]

parseGrid :: [String] -> Grid
parseGrid = map (uncurry parseLine) .  zip [0..]


findInGrid :: Grid -> Position -> Maybe GridItem
findInGrid grid (Position x y) = (grid !? y) >>= (!?x)


nextDirection :: Char -> Direction -> Direction
nextDirection '|' South = South
nextDirection '|' North = North
nextDirection '-' West = West
nextDirection '-' East = East
nextDirection 'L' South = East
nextDirection 'L' West = North
nextDirection 'J' South = West
nextDirection 'J' East = North
nextDirection '7' East = South
nextDirection '7' North = West
nextDirection 'F' North = East
nextDirection 'F' West = South


toPosition :: Direction -> Position -> Position
toPosition direction (Position x y) = case direction of North -> Position x (y - 1)
                                                        South -> Position x (y + 1)
                                                        East -> Position (x + 1) y
                                                        West -> Position (x - 1) y


validStartNext :: Direction -> GridItem -> Bool
validStartNext North (c, _) = c `elem` "|7F"
validStartNext East (c, _) = c `elem` "-J7"
validStartNext South (c, _) = c `elem` "|LJ"


findStartNext :: Grid -> Position -> Direction
findStartNext grid startPos = fst
        $ NE.head
        $ NE.fromList
        $ filter (uncurry validStartNext)
        $ map (\(d, mp) -> (d, fromJust mp))
        $ filter (\(d, mp) -> isJust mp) possibleNexts
    where
        northItem = findInGrid grid $ toPosition North startPos
        eastItem = findInGrid grid $ toPosition East startPos
        southItem = findInGrid grid $ toPosition South startPos
        possibleNexts = [(North, northItem), (East, eastItem), (South, southItem)]




findStart :: Grid -> GridItem
findStart = (fromJust . join) . find isJust . map (find (\(c, _) -> c == 'S'))
        

buildLoop :: Grid -> Direction -> NE.NonEmpty GridItem -> NE.NonEmpty GridItem
buildLoop grid nextDir loop
    | c == 'S' = loop
    | otherwise = buildLoop grid (nextDirection c nextDir) $ NE.appendList loop [(c, nextPos)]
    where
        (c, nextPos) = fromJust $ findInGrid grid $ toPosition nextDir $ snd $ NE.last loop



main :: IO ()
main = do
    input <- readFile "data/day10/input.txt"
    let grid = parseGrid $ lines input
    let (startChar, startPos) = findStart grid
    let startNext = findStartNext grid startPos
    let loop = NE.toList $ buildLoop grid startNext $ NE.fromList [(startChar, startPos)]
    let t = zipWith min (zipWith (\a b -> a) [0..] loop) (reverse (zipWith (\a b -> a) [1..] (reverse loop))) 
    print $ maximum t
