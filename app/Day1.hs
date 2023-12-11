module Main where
import Text.Read
import Data.List


data Direction = Forward | Backward deriving (Eq)


parseFirstInt :: String -> Maybe Int
parseFirstInt str
    | isPrefixOf "one" str = Just 1
    | isPrefixOf "two" str = Just 2
    | isPrefixOf "three" str = Just 3
    | isPrefixOf "four" str = Just 4
    | isPrefixOf "five" str = Just 5
    | isPrefixOf "six" str = Just 6
    | isPrefixOf "seven" str = Just 7
    | isPrefixOf "eight" str = Just 8
    | isPrefixOf "nine" str = Just 9
    | otherwise = readMaybe [head str] :: Maybe Int


findDigit :: Direction -> Int -> String -> Int
findDigit direction current [] = current
findDigit direction current str =
    let next = parseFirstInt str
    in case next of Nothing -> findDigit direction current $ tail str
                    Just int -> if direction == Forward
                                    then int
                                    else findDigit direction int $ tail str


getDigits :: String -> [Int]
getDigits line = [first, last]
    where first = findDigit Forward 0 line
          last = findDigit Backward 0 line


main :: IO ()
main = do
    contents <- readFile "data/day1/input2.txt"
    let digits = map getDigits $ lines contents
    let numberStrings = map (concatMap show) digits
    print $ sum $ map read numberStrings

