import Data.Matrix (Matrix, ncols, nrows, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

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
  | Matrix.safeGet nr nc field == Just 'S' = [pos]
  | isJust newDir = pos : path field nextPos (fromJust newDir)
  | otherwise = []
  where
    nextPos@(nr, nc) = moveTo pos dir
    newDir = Matrix.safeGet nr nc field >>= nextDir dir

findStart :: Matrix Char -> Pos
findStart field = head [(r, c) | r <- [1 .. nrows field], c <- [1 .. ncols field], field ! (r, c) == 'S']

inBounds :: (Int, Int) -> Pos -> Bool
inBounds (br, bc) (r, c) = r >= 1 && c >= 1 && r <= br && c <= bc

adjacents :: (Int, Int) -> Pos -> Set Pos
adjacents bounds (r, c) = Set.filter (inBounds bounds) $ Set.fromList [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

bfs :: (Int, Int) -> Set Pos -> Set Pos -> Set Pos
bfs _ seen currs | Set.null currs = seen
bfs bounds seen currs = bfs bounds newSeen newCurrs
  where
    newCurrs = Set.unions (Set.map (adjacents bounds) currs) `Set.difference` seen
    newSeen = Set.union seen currs

extraElems :: Matrix Char -> Pos -> [Pos]
extraElems field (r, c) = case field ! (r, c) of
  'F' -> [(r * 2 + 1, c * 2), (r * 2, c * 2 + 1)]
  'L' -> [(r * 2 - 1, c * 2), (r * 2, c * 2 + 1)]
  '7' -> [(r * 2 + 1, c * 2), (r * 2, c * 2 - 1)]
  'J' -> [(r * 2 - 1, c * 2), (r * 2, c * 2 - 1)]
  '|' -> [(r * 2 - 1, c * 2), (r * 2 + 1, c * 2)]
  '-' -> [(r * 2, c * 2 - 1), (r * 2, c * 2 + 1)]
  'S' -> []
  _ -> error "unknown path element"

scalePath :: Matrix Char -> Set Pos -> Set Pos
scalePath field p = Set.union scaledElems newElems
  where
    scaledElems = Set.map (\(r, c) -> (r * 2, c * 2)) p
    newElems = Set.unions $ Set.map (Set.fromList . extraElems field) p

findAnswer :: Matrix Char -> Int
findAnswer field = nrows field * ncols field - Set.size notInLoop
  where
    start = findStart field
    allPaths = filter (not . null) [path field start d | d <- [U ..]]
    somePath = head allPaths

    scaledPath = scalePath field (Set.fromList somePath)
    scaledBounds = (nrows field * 2, ncols field * 2)
    scaledBorders =
      Set.fromList $
        [(1, c) | c <- [1 .. ncols field * 2]]
          ++ [(nrows field * 2, c) | c <- [1 .. ncols field * 2]]
          ++ [(r, 1) | r <- [1 .. nrows field * 2]]
          ++ [(r, ncols field * 2) | r <- [1 .. nrows field * 2]]

    startPoss = scaledBorders `Set.difference` scaledPath
    scaledOutsideLoop = bfs scaledBounds scaledPath startPoss

    notInLoop = Set.filter (\(r, c) -> even r && even c) (Set.union scaledPath scaledOutsideLoop)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = Matrix.fromLists input
  print $ findAnswer field
