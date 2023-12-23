import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

type Forests = Set Pos

data Field = Field
  { getForests :: Forests,
    getStart :: Pos,
    getEnd :: Pos,
    getMaxR :: Int,
    getMaxC :: Int
  }

type Edge = (Pos, Int) -- pos, weight

type Network = Map Pos [Edge]

findMaxPath :: Network -> Pos -> Pos -> Set Pos -> Maybe Int
findMaxPath _ start end _ | start == end = Just 0
findMaxPath net start end seen = case nbPaths of
  [] -> Nothing
  lengths -> Just $ maximum lengths
  where
    nbs = filter (\(p, _) -> p `Set.notMember` seen) $ net ! start
    nbPaths = mapMaybe (\(nb, w) -> (w +) <$> findMaxPath net nb end (Set.insert nb seen)) nbs

inBounds :: Field -> Pos -> Bool
inBounds f (r, c) = r >= 1 && c >= 1 && r <= getMaxR f && c <= getMaxC f

neighbors :: Field -> Pos -> Set Pos
neighbors f (r, c) =
  Set.fromList
    [ (r', c')
      | p@(r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)],
        p `Set.notMember` getForests f,
        inBounds f p
    ]

isNode :: Field -> Pos -> Bool
isNode f point | point == getStart f = True
isNode f point | point == getEnd f = True
isNode f point = Set.size (neighbors f point) > 2

buildNodes :: Field -> [Pos]
buildNodes f =
  [ (r, c)
    | r <- [1 .. getMaxR f],
      c <- [1 .. getMaxC f],
      (r, c) `Set.notMember` getForests f,
      isNode f (r, c)
  ]

buildEdges :: Field -> Set Pos -> Pos -> [Edge]
buildEdges f path curr | not (Set.null path) && isNode f curr = [(curr, Set.size path)]
buildEdges f path curr = concatMap (buildEdges f newPath) nbs
  where
    newPath = Set.insert curr path
    nbs = neighbors f curr `Set.difference` path

buildNetwork :: Field -> Network
buildNetwork field = Map.fromList [(node, buildEdges field Set.empty node) | node <- buildNodes field]

findAnswer :: Field -> Int
findAnswer f = fromJust maxPath
  where
    net = buildNetwork f
    maxPath = findMaxPath net (getStart f) (getEnd f) Set.empty

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let tiles = [((r, c), x) | (r, line) <- zip [1 ..] input, (c, x) <- zip [1 ..] line]
  let forests = Set.fromList [(r, c) | ((r, c), x) <- tiles, x == '#']
  let start = (1, 2)
  let maxR = maximum [r | ((r, _), _) <- tiles]
  let maxC = maximum [c | ((_, c), _) <- tiles]
  let end = (maxR, maxC - 1)
  print $ findAnswer (Field forests start end maxR maxC)
