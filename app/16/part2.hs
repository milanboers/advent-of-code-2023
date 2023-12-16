import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Pos = (Int, Int)

data Dir = U | D | L | R deriving (Show, Eq, Ord)

type Beam = (Pos, Dir)

type Field = Map Pos Char

type Bounds = (Int, Int)

moveBeam :: Beam -> Beam
moveBeam ((r, c), U) = ((r - 1, c), U)
moveBeam ((r, c), D) = ((r + 1, c), D)
moveBeam ((r, c), L) = ((r, c - 1), L)
moveBeam ((r, c), R) = ((r, c + 1), R)

inBounds :: Bounds -> Beam -> Bool
inBounds (br, bc) ((r, c), _) = r > 0 && c > 0 && r <= br && c <= bc

beamNext :: Field -> Beam -> Set Beam
beamNext field (pos, dir) = case (dir, field !? pos) of
  (U, Just '/') -> Set.singleton (pos, R)
  (D, Just '/') -> Set.singleton (pos, L)
  (L, Just '/') -> Set.singleton (pos, D)
  (R, Just '/') -> Set.singleton (pos, U)
  (U, Just '\\') -> Set.singleton (pos, L)
  (D, Just '\\') -> Set.singleton (pos, R)
  (L, Just '\\') -> Set.singleton (pos, U)
  (R, Just '\\') -> Set.singleton (pos, D)
  (L, Just '|') -> Set.fromList [(pos, U), (pos, D)]
  (R, Just '|') -> Set.fromList [(pos, U), (pos, D)]
  (U, Just '-') -> Set.fromList [(pos, L), (pos, R)]
  (D, Just '-') -> Set.fromList [(pos, L), (pos, R)]
  (_, _) -> Set.singleton (pos, dir)

step :: Bounds -> Field -> Set Beam -> Set Beam
step bounds field beams = Set.unions . Set.map (beamNext field) $ validMovedBeams
  where
    movedBeams = Set.map moveBeam beams
    validMovedBeams = Set.filter (inBounds bounds) movedBeams

steps :: Bounds -> Field -> Set Beam -> Set Beam -> Set Beam
steps _ _ seen currs | Set.null currs = seen
steps bounds field seen currs = steps bounds field newSeen newCurrs
  where
    newSeen = Set.union seen currs
    newCurrs = step bounds field currs `Set.difference` seen

energized :: Set Beam -> Int
energized = Set.size . Set.map fst

findEnergized :: Bounds -> Field -> Beam -> Int
findEnergized bounds field initBeam = (-1 +) . energized . steps bounds field Set.empty $ Set.singleton initBeam

findAnswer :: Bounds -> Field -> Int
findAnswer bounds@(br, bc) field = maximum . map (findEnergized bounds field) $ initBeams
  where
    initBeams =
      [((r, 0), L) | r <- [1 .. br]]
        ++ [((r, bc + 1), R) | r <- [1 .. br]]
        ++ [((0, c), D) | c <- [1 .. bc]]
        ++ [((br + 1, c), U) | c <- [1 .. bc]]

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let bounds = (length input, length $ head input)
  let field = Map.fromList [((r, c), x) | (r, line) <- zip [1 ..] input, (c, x) <- zip [1 ..] line, x /= '.']
  print $ findAnswer bounds field
