import Data.Char (isDigit)
import Data.Matrix (Matrix, getRow, ncols, nrows, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (mapMaybe)
import qualified Data.Vector as Vector

type Coord = (Int, Int)

type Field = Matrix Char

type Number = (Coord, Int) -- start, length

tuplify2 :: [a] -> (a, a)
tuplify2 [x, y] = (x, y)
tuplify2 _ = error "list does not have exactly 2 values"

adjacent :: Number -> Coord -> Bool
adjacent ((i, j), l) (gi, gj) = i - 1 <= gi && gi <= i + 1 && j - 1 <= gj && gj <= j + l

adjacentToTwoNumbers :: [Number] -> Coord -> Maybe (Number, Number)
adjacentToTwoNumbers ns gear = if length adjacentNumbers == 2 then Just (tuplify2 adjacentNumbers) else Nothing
  where
    adjacentNumbers = [n | n <- ns, adjacent n gear]

numberValue :: Field -> Number -> Int
numberValue field ((i, j), l) = read $ Vector.toList $ Vector.slice (j - 1) l (getRow i field)

readNumber :: Field -> Coord -> Number
readNumber field (i, j) = ((i, j), l)
  where
    row = getRow i field
    l = Vector.length $ Vector.takeWhile isDigit (Vector.drop (j - 1) row)

isNumberStart :: Field -> Coord -> Bool
isNumberStart field (i, j) = isDigit current && maybe True (not . isDigit) previous
  where
    current = field ! (i, j)
    previous = Matrix.safeGet i (j - 1) field

numberStarts :: Field -> [Coord]
numberStarts field =
  [ (i, j)
    | i <- [1 .. nrows field],
      j <- [1 .. ncols field],
      isNumberStart field (i, j)
  ]

findNumbers :: Field -> [Number]
findNumbers field = map (readNumber field) (numberStarts field)

findGears :: Field -> [Coord]
findGears field = [(i, j) | i <- [1 .. nrows field], j <- [1 .. ncols field], field ! (i, j) == '*']

findAnswer :: Field -> Int
findAnswer field = sum $ map (\(x, y) -> numberValue field x * numberValue field y) gearsAndAdjacentNumbers
  where
    numbers = findNumbers field
    gears = findGears field
    gearsAndAdjacentNumbers = mapMaybe (adjacentToTwoNumbers numbers) gears

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = Matrix.fromLists input
  print $ findAnswer field
