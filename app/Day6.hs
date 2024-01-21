module Main where



parseLine :: String -> [Int]
parseLine line = [(read . concat . tail . words) line]


calculateDistance :: Int -> Int -> Int
calculateDistance totalTime holdTime = distance
    where
        travelTime = totalTime - holdTime
        speed = holdTime
        distance = speed * travelTime


genDistances :: Int -> [Int]
genDistances totalTime = map (calculateDistance totalTime) [0..totalTime]


genRecordBeaters :: Int -> Int -> [Int]
genRecordBeaters totalTime record = filter (>record) $ genDistances totalTime


main :: IO ()
main = do
    input <- readFile "data/day6/input.txt"
    let [times, distances] = map parseLine $ lines input
    let recordBeaters = zipWith genRecordBeaters times distances
    print $ product $ map length recordBeaters
