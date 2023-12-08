import Data.Map (Map)
import qualified Data.Map.Strict as Map

type Node = String

type Network = Map Node (Node, Node)

type Path = [Char]

stepsToEnd :: Network -> Node -> Path -> Int
stepsToEnd _ "ZZZ" _ = 0
stepsToEnd net node (x : xs) = case x of
  'L' -> 1 + stepsToEnd net nextL xs
  'R' -> 1 + stepsToEnd net nextR xs
  _ -> error "path element not L or R"
  where
    (nextL, nextR) = net Map.! node
stepsToEnd _ _ [] = error "unable to reach end"

findAnswer :: Network -> Path -> Int
findAnswer net = stepsToEnd net "AAA"

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
