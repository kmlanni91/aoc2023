module Main where
import Data.List
import Data.Char as C
import Data.Maybe (fromJust)


data Direction = Left | Right deriving (Show)


data Node = Node { val :: String
            , next :: (String, String)
            } deriving (Show)


data NodePointer = NodePointer { counter :: Int
                               , node :: Node
                               } deriving (Show)


toDirection :: Char -> Direction
toDirection 'L' = Main.Left
toDirection 'R' = Main.Right


parseDirections :: String -> [Direction]
parseDirections = map toDirection 


parseNode :: String -> Node
parseNode str = Node val (left, right)
    where
        [val, left, right] = filter (/= "") $ map (filter C.isAlpha) $ words str


findNode :: String -> [Node] -> Node
findNode toFind = fromJust . find (\node -> val node == toFind)


moveNode :: [Node] -> Direction -> Node -> Node
moveNode nodes direction current = findNode nextNode nodes
    where
        (left, right) = next current
        nextNode = case direction of Main.Left -> left
                                     Main.Right -> right


findFinalNode :: String -> [Node] -> [Direction] -> NodePointer -> NodePointer
findFinalNode end nodes directions (NodePointer counter node)
    | val node == end = NodePointer counter node
    | otherwise = findFinalNode end nodes (tail directions) nextPointer
        where
            nextNode = moveNode nodes (head directions) node
            nextPointer = NodePointer (counter + 1) nextNode


main :: IO ()
main = do
    input <- readFile "data/day8/input.txt"
    let inputLines = lines input
    let directions = cycle $ map toDirection $ head inputLines
    let nodes = map parseNode $ filter (/="") $ tail inputLines
    let first = NodePointer 0 (findNode "AAA" nodes)
    print $ counter $ findFinalNode "ZZZ" nodes directions first

