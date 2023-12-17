import Algorithm.Search (dijkstra)
import Data.Char (digitToInt)
import Data.Matrix (Matrix, ncols, nrows, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (fromJust, isJust)

type Field = Matrix Int

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Eq, Enum, Ord)

type State = (Pos, Dir, Int) -- pos, dir, # in dir

move :: Dir -> Pos -> Pos
move U (r, c) = (r - 1, c)
move D (r, c) = (r + 1, c)
move L (r, c) = (r, c - 1)
move R (r, c) = (r, c + 1)

oppositeDir :: Dir -> Dir
oppositeDir U = D
oppositeDir D = U
oppositeDir L = R
oppositeDir R = L

neighbors :: Field -> State -> [State]
neighbors field (pos, dir, nDir)
  | nDir < 3 =
      [ ((r, c), dir, nDir + 1)
        | let (r, c) = move dir pos,
          isJust $ Matrix.safeGet r c field
      ]
neighbors field (pos, dir, nDir) =
  [ ((r, c), newDir, newNDir)
    | newDir <- [U ..],
      oppositeDir newDir /= dir,
      let (r, c) = move newDir pos,
      isJust $ Matrix.safeGet r c field,
      let newNDir = if dir == newDir then nDir + 1 else 0,
      newNDir < 10
  ]

cost :: Field -> State -> State -> Int
cost field _ (pos, _, _) = field ! pos

end :: Field -> State -> Bool
end field ((r, c), _, nDir) = nDir >= 3 && r == nrows field && c == ncols field

findAnswer :: Field -> Int
findAnswer field = fst . fromJust . dijkstra (neighbors field) (cost field) (end field) $ ((1, 1), D, 4)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = Matrix.fromLists $ (map . map) digitToInt input
  print $ findAnswer field
