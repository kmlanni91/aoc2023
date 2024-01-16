module Main where
import Data.List


pairRanges :: [Int] -> [(Int, Int)]
pairRanges [] = []
pairRanges (a:b:leftovers) = (a, b):pairRanges leftovers


parseSeeds :: String -> [Int]
parseSeeds x = seeds
    where
        seedsStrs = tail $ words x
        parsedInts = map read seedsStrs
        seedRanges = pairRanges parsedInts
        seeds = concatMap (\(start, len) -> [start..start + len - 1]) seedRanges


parseSections :: [String] -> [[String]]
parseSections list = result
    where
        (newSection, leftovers) = span (/= "") $ dropWhile (=="") list
        result = case leftovers of [] -> [newSection]
                                   [""] -> [newSection]
                                   xs -> newSection:parseSections xs


data MapRange = MapRange { destStart :: Int
                         , srcStart :: Int
                         , rangeLen :: Int
                         } deriving (Show)


toMapRange :: [Int] -> MapRange
toMapRange [dest, src, len] = MapRange {destStart=dest, srcStart=src, rangeLen=len}



sectionMapRanges :: [String] -> [MapRange]
sectionMapRanges =  map (toMapRange . map read . words) . tail


convertValue :: Int -> [MapRange] -> Int
convertValue val mapRanges = result 
    where
        inRange x = srcStart x <= val && val <= srcStart x + rangeLen x - 1
        mConverter = find inRange mapRanges
        result = case mConverter of Nothing -> val
                                    Just converter -> destStart converter + val - srcStart converter


seedToLocation :: Int -> [[MapRange]] -> Int
seedToLocation = foldl' convertValue


main = do
    input <- readFile "data/day5/input.txt"
    let strLines = lines input
    let seeds = parseSeeds $ head strLines
    let converters =  map sectionMapRanges $ (parseSections . tail) strLines
    let locations = map (`seedToLocation` converters) seeds 
    print $ minimum locations
