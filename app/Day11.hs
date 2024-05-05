module Main where


data Direction = Up | Down | Left | Right

data Point = Point
    { val :: Char
    , x :: Int
    , y :: Int
    } deriving (Eq, Show)

data Grid = Grid
    { gridpoints :: [[Point]]
    , xlen :: Int
    , ylen :: Int
    } deriving (Show)

toGrid :: [String] -> Grid
toGrid lines = grid where
    toPoints lineno line = zipWith (\a b -> Point b a lineno) [0..] line
    points = zipWith toPoints [0..] lines
    grid = Grid points (length $ head points) (length points)

printGrid :: Grid -> IO ()
printGrid grid = mapM_ (print . (map val)) $ gridpoints grid

row :: Grid -> Int -> [Point]
row grid yidx
    | 0 > yidx || yidx >= ylen grid = []
    | otherwise = (gridpoints grid) !! yidx

column :: Grid -> Int -> [Point]
column grid xidx
    | 0 > xidx || xidx >= xlen grid = []
    | otherwise = map (!! xidx) $ gridpoints grid

isEmpty :: [Point] -> Bool
isEmpty points = result where
    nonEmpty = filter (\a -> val a == '#') points
    result = (length nonEmpty) == 0

emptyRows :: Grid -> [Int]
emptyRows grid = result where
    rows = zip [0..] $ gridpoints grid
    result = map fst $ filter (isEmpty . snd) rows

emptyCols :: Grid -> [Int]
emptyCols grid = result where
    columns = zip [0..] $ map (column grid) [0..(xlen grid) - 1]
    result = map fst $ filter (isEmpty . snd) columns

expMultiplier :: Int
expMultiplier = 1000000

expandGalaxy :: [Int] -> [Int] -> Point -> Point
expandGalaxy ecols erows (Point c x' y') = Point c (x' + incX) (y' + incY) where
    getElen b emptys = length $ filter (\a -> a < b) emptys
    getNew elen
        | elen == 0 = 0
        | expMultiplier == 1 = elen 
        | otherwise = elen * expMultiplier - elen
    incX = getNew $ getElen x' ecols
    incY = getNew $ getElen y' erows


findGalaxies :: Grid -> [Point]
findGalaxies = concat . (map (filter ((=='#') . val))) . gridpoints

combinations :: [Point] -> [(Point, Point)]
combinations [] = []
combinations (p:ps) = map (\a -> (p, a)) ps ++ combinations ps

minSteps :: Point -> Point -> Int
minSteps (Point _ x1 y1) (Point _ x2 y2) = abs (x1 - x2) + abs (y1 - y2)


main :: IO ()
main = do
    input <- readFile "data/day11/input.txt"
    let ilines = lines input
    let grid = toGrid ilines
    let erows = emptyRows grid
    let ecols = emptyCols grid
    let galaxies = findGalaxies grid
    --print galaxies
    let expGalaxies = map (expandGalaxy ecols erows) galaxies
    --print expGalaxies
    let combos = combinations expGalaxies
    --print combos
    --print $ length combos
    print $ sum $ map (uncurry minSteps) combos

