import Data.List.Split (splitWhen)
import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Node = String

type Network = Map Node (Node, Node)

type Path = [Char]

findPath :: Network -> Node -> Path -> [Node]
findPath net node (x : xs) = case x of
  'L' -> node : findPath net nextL xs
  'R' -> node : findPath net nextR xs
  _ -> error "path element not L or R"
  where
    (nextL, nextR) = net Map.! node
findPath _ _ [] = error "unable to reach end"

cycleLength :: Network -> Node -> Path -> Int
cycleLength net node path = length (paths !! 1) + 1
  where
    paths = splitWhen (\n -> last n == 'Z') (findPath net node path)

starts :: Network -> [Node]
starts = filter (\node -> last node == 'A') . Map.keys

allStepsToEnd :: Network -> [Node] -> Path -> Int
allStepsToEnd net nodes path = foldl lcm 1 $ map (\node -> cycleLength net node path) nodes

findAnswer :: Network -> Path -> Int
findAnswer net = allStepsToEnd net (starts net)

parseNetElem :: String -> (Node, (Node, Node))
parseNetElem xs = (take 3 xs, (take 3 $ drop 7 xs, take 3 $ drop 12 xs))

parseNet :: [String] -> Network
parseNet = Map.fromList . map parseNetElem

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let path = head input
  let rawNet = drop 2 input
  print $ findAnswer (parseNet rawNet) (cycle path)
