import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

type Bounds = (Int, Int)

type Field = (Bounds, Set Pos) -- bounds, rocks

inBounds :: Field -> Pos -> Bool
inBounds ((br, bc), _) (r, c) = r >= 1 && c >= 1 && r <= br && c <= bc

neighbors :: Field -> Pos -> Set Pos
neighbors field@(_, rocks) (r, c) =
  Set.fromList
    [ (nr, nc)
      | n@(nr, nc) <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)],
        inBounds field n,
        n `Set.notMember` rocks
    ]

steps :: Field -> Set Pos -> [Set Pos]
steps field currs = currs : steps field newCurrs
  where
    newCurrs = Set.unions $ Set.map (neighbors field) currs

findAnswer :: Field -> Pos -> Int
findAnswer field start = Set.size $ allSteps !! 64
  where
    allSteps = steps field (Set.singleton start)

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = [((r, c), x) | (r, line) <- zip [1 ..] input, (c, x) <- zip [1 ..] line]
  let rocks = Set.fromList [(r, c) | ((r, c), x) <- field, x == '#']
  let start = head [(r, c) | ((r, c), x) <- field, x == 'S']
  let bounds = (length input, length $ head input)
  print $ findAnswer (bounds, rocks) start
