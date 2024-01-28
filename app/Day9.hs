module Main where


parseLine :: String -> [Int]
parseLine = map read . words


genDiff :: [Int] -> [Int]
genDiff [] = []
genDiff [a] = []
genDiff (a:as) = head as - a:genDiff as


extrapolate :: [Int] -> [[Int]]
extrapolate a
    | all (==0) a = [a] 
    | otherwise = a:extrapolate (genDiff a)



main :: IO ()
main = do
    input <- readFile "data/day9/input.txt"
    print $ sum $ map (foldr1 (-) . map head . extrapolate . parseLine) $ lines input
