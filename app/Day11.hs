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

reposition :: Point -> Int -> Int -> Point
reposition (Point v x y) xinc yinc = Point v (x + xinc) (y + yinc)

expandRow :: Int -> [Int] -> [Point] -> [Point]
expandRow yinc emptyCols row' = recompute row' where
    recompute [] = []
    recompute (p:ps)
        | px `elem` emptyCols = newPoint:Point '.' (x newPoint + 1) (y newPoint):recompute ps
        | otherwise = newPoint:recompute ps
        where
            px = x p
            xinc = length $ filter (\a -> a < px) emptyCols
            newPoint = reposition p xinc yinc

expandGrid :: Grid -> Grid
expandGrid grid = Grid (expand $ gridpoints grid) newXlen newYlen where
    erows = emptyRows grid
    ecols = emptyCols grid
    newYlen = ylen grid + length erows
    newXlen = xlen grid + length ecols
    expand [] = []
    expand (r:rs)
        | ry `elem` erows = newRow:map (\a -> Point '.' a (ry + 2)) [0..newXlen - 1]:expand rs 
        | otherwise = newRow:expand rs
        where
            ry = y $ head r
            yinc = length $ filter (\a -> a < ry) erows
            newRow = expandRow yinc ecols r

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
    --print grid
    --printGrid grid
    --print $ row grid 4
    --print $ column grid 2
    --print $ emptyRows grid
    --print $ emptyCols grid
    --putStrLn ""
    let newGrid = expandGrid grid
    --printGrid newGrid
    let galaxies = findGalaxies newGrid
    --print galaxies
    let combos = combinations galaxies
    --print combos
    --print $ length combos
    print $ sum $ map (uncurry minSteps) combos

