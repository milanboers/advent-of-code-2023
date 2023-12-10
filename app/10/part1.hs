import Data.Matrix (Matrix, ncols, nrows, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust, isJust)

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Eq, Enum)

moveTo :: Pos -> Dir -> (Int, Int)
moveTo (r, c) U = (r - 1, c)
moveTo (r, c) D = (r + 1, c)
moveTo (r, c) L = (r, c - 1)
moveTo (r, c) R = (r, c + 1)

nextDir :: Dir -> Char -> Maybe Dir
nextDir U '|' = Just U
nextDir U '7' = Just L
nextDir U 'F' = Just R
nextDir D '|' = Just D
nextDir D 'J' = Just L
nextDir D 'L' = Just R
nextDir L '-' = Just L
nextDir L 'L' = Just U
nextDir L 'F' = Just D
nextDir R '-' = Just R
nextDir R '7' = Just D
nextDir R 'J' = Just U
nextDir _ _ = Nothing

path :: Matrix Char -> Pos -> Dir -> [Pos]
path field pos dir
  | field ! nextPos == 'S' = [pos]
  | isJust newDir = pos : path field nextPos (fromJust newDir)
  | otherwise = []
  where
    nextPos = moveTo pos dir
    newDir = nextDir dir (field ! nextPos)

findStart :: Matrix Char -> Pos
findStart field = head [(r, c) | r <- [1 .. nrows field], c <- [1 .. ncols field], field ! (r, c) == 'S']

findAnswer :: Matrix Char -> Int
findAnswer field = length somePath `div` 2
  where
    start = findStart field
    allPaths = filter (not . null) [path field start d | d <- [U ..]]
    somePath = head allPaths

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = Matrix.fromLists input
  print $ findAnswer field
