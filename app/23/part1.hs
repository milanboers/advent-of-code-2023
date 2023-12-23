import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data SlopeType = U | D | L | R deriving (Show, Eq)

type Pos = (Int, Int)

type Forests = Set Pos

type Slopes = Map Pos SlopeType

type Field = (Forests, Slopes)

validSlope :: Slopes -> Pos -> Pos -> Bool
validSlope slopes _ to | to `Map.notMember` slopes = True
validSlope slopes (fr, fc) to@(tr, tc) | slopes ! to == U && (tr, tc) == (fr - 1, fc) = True
validSlope slopes (fr, fc) to@(tr, tc) | slopes ! to == D && (tr, tc) == (fr + 1, fc) = True
validSlope slopes (fr, fc) to@(tr, tc) | slopes ! to == L && (tr, tc) == (fr, fc - 1) = True
validSlope slopes (fr, fc) to@(tr, tc) | slopes ! to == R && (tr, tc) == (fr, fc + 1) = True
validSlope _ _ _ = False

neighbors :: Field -> Pos -> [Pos]
neighbors (forests, slopes) from@(r, c) =
  [ (r', c')
    | p@(r', c') <- [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)],
      p `Set.notMember` forests,
      validSlope slopes from p
  ]

findMaxPath :: Field -> Pos -> Pos -> Set Pos -> Maybe Int
findMaxPath _ start end _ | start == end = Just 0
findMaxPath field start end seen = case nbPaths of
  [] -> Nothing
  lengths -> Just $ 1 + maximum lengths
  where
    nbs = filter (`Set.notMember` seen) $ neighbors field start
    nbPaths = mapMaybe (\nb -> findMaxPath field nb end (Set.insert nb seen)) nbs

findAnswer :: Field -> Pos -> Pos -> Int
findAnswer field (sr, sc) end = fromJust $ findMaxPath field (sr, sc) end (Set.singleton (sr - 1, sc))

parseSlopeType :: Char -> SlopeType
parseSlopeType '^' = U
parseSlopeType 'v' = D
parseSlopeType '<' = L
parseSlopeType '>' = R
parseSlopeType _ = error "not a valid slope"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let tiles = [((r, c), x) | (r, line) <- zip [1 ..] input, (c, x) <- zip [1 ..] line]
  let forests = Set.fromList [(r, c) | ((r, c), x) <- tiles, x == '#']
  let slopes = Map.fromList [((r, c), parseSlopeType x) | ((r, c), x) <- tiles, x /= '#', x /= '.']
  let start = (1, 2)
  let maxRow = maximum [r | ((r, _), _) <- tiles]
  let maxCol = maximum [c | ((_, c), _) <- tiles]
  let end = (maxRow, maxCol - 1)
  print $ findAnswer (forests, slopes) start end
