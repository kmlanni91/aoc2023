module Main where
import Data.List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, fromJust)
import Control.Monad (join)


data Position = Position Int Int deriving (Show, Eq) 

data Direction = West | East | North | South deriving (Show, Eq, Enum, Bounded)

type GridItem = (Char, Position)

type Grid = [[GridItem]]

data RegionState = InRegion | NotInRegion deriving (Show, Eq)


parseLine :: Int -> String -> [GridItem]
parseLine lineno line =  zipWith (\c i -> (c, Position i lineno)) line [0..]

parseGrid :: [String] -> Grid
parseGrid = map (uncurry parseLine) .  zip [0..]


findInGrid :: Grid -> Position -> Maybe GridItem
findInGrid grid (Position x y) = (grid !? y) >>= (!?x)


findStart :: Grid -> GridItem
findStart = (fromJust . join) . find isJust . map (find (\(c, _) -> c == 'S'))


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
toPosition direction (Position x y) = case direction of 
    North -> Position x (y - 1)
    South -> Position x (y + 1)
    East -> Position (x + 1) y
    West -> Position (x - 1) y


validNext :: Direction -> GridItem -> Maybe GridItem
validNext _ ('S', pos) = Just ('S', pos)
validNext dir (c, pos)
    | (dir == North) && (c `elem` "|7F") = Just (c, pos)
    | (dir == South) && (c `elem` "|LJ") = Just (c, pos)
    | (dir == East) && (c `elem` "J7-") = Just (c, pos)
    | (dir == West) && (c `elem` "FL-") = Just (c, pos)
    | otherwise = Nothing


getNext :: Grid -> Direction -> GridItem -> Maybe GridItem
getNext grid dir item = result 
    where
        next = ((findInGrid grid) . (toPosition dir) . snd) item
        result = next >>= (validNext dir)


followPath :: Grid -> Direction -> NE.NonEmpty (Maybe GridItem) -> NE.NonEmpty (Maybe GridItem)
followPath grid dir path = result where
    current = NE.last path
    next = current >>= (getNext grid dir)
    result = case next of
        Nothing -> NE.appendList path [Nothing]
        Just ('S', pos) -> path
        Just (c, pos) -> followPath grid (nextDirection c dir) $ NE.appendList path [Just (c, pos)]


buildLoop :: Grid -> [GridItem]
buildLoop grid = loop where
    start = findStart grid
    possibleLoops = map ((flip $ followPath grid) (NE.fromList [Just start])) [(minBound :: Direction)..]
    loop = map fromJust $ NE.toList . fromJust $ find (all isJust) possibleLoops


toSimpleGrid :: Grid -> [GridItem] -> Grid
toSimpleGrid grid loop = map (map toSimpleChar) grid where
    toSimpleChar (c,pos)
        | (c, pos) `elem` loop = ('*', pos)
        | otherwise = ('.', pos)


isIntersection :: GridItem -> Maybe GridItem -> Maybe GridItem -> Bool
isIntersection ('*', _) Nothing (Just next) = True
isIntersection ('*', _) (Just prev) Nothing = True
isIntersection ('*', _) (Just (pc, _)) (Just (nc, _)) = any (=='.') [pc, nc]
isIntersection _ _ _ = False


window :: Grid -> GridItem -> (GridItem, Maybe GridItem, Maybe GridItem)
window grid (c, Position x y) = ((c, Position x y), prev, next) where
    prev = findInGrid grid (Position (x-1) y)
    next = findInGrid grid (Position (x+1) y)


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c


intersections :: Grid -> [[GridItem]]
intersections grid = map (filter ((uncurry3 isIntersection) . (window grid))) grid


groupIntersections :: [a] -> [(a, a)]
groupIntersections intsects = group intsects where
     toTuple [x,y] = (x, y)
     group [] = []
     group [a] = []
     group list = (toTuple $ take 2 list):group (drop 1 list)


filterAlternates :: [a] -> [a]
filterAlternates [] = []
filterAlternates [x] = [x]
filterAlternates (x:xs) = x:(drop 1 xs)


inFillRange :: GridItem -> (GridItem,GridItem) -> Bool
inFillRange (_, Position x _) ((_, Position x1 _), (_, Position x2 _))
    | x > x1 && x < x2 = True
    | otherwise = False


fillChar :: [(GridItem, GridItem)] -> GridItem -> GridItem
fillChar intsects item = case item of
    ('*', _) -> item
    (c, pos) -> if any (inFillRange item) intsects then ('I', pos) else item


fillLine :: [GridItem] -> [(GridItem, GridItem)] -> [GridItem]
fillLine line intsects = map (fillChar intsects) line


fillGrid :: Grid -> Grid
fillGrid grid = result where
    intsectList = map (filterAlternates . groupIntersections) $ intersections grid
    result = map (uncurry fillLine) $ zip grid intsectList



printGrid :: Grid -> IO ()
printGrid = mapM_ (print . map fst)


main :: IO ()
main = do
    input <- readFile "data/day10/sample3.txt"
    let grid = parseGrid $ lines input
    --printGrid grid
    let loop = buildLoop grid
    --print loop
    let simpleGrid = toSimpleGrid grid loop
    printGrid simpleGrid
    putStrLn ""
    let filled = fillGrid simpleGrid
    printGrid filled
    putStrLn ""
    print $ sum $ map (length . (filter (=='I')) . map fst) filled
