import Data.Char (isDigit)
import Data.Matrix (Matrix, getRow, ncols, nrows, (!))
import qualified Data.Matrix as Matrix
import Data.Maybe (maybeToList)
import qualified Data.Vector as Vector

type Coord = (Int, Int)

type Field = Matrix Char

type Number = (Coord, Int) -- start, length

isSymbol :: Char -> Bool
isSymbol x = not (isDigit x) && x /= '.'

adjacentToSymbol :: Matrix Char -> Number -> Bool
adjacentToSymbol field ((i, j), l) =
  or
    [ isSymbol c
      | ni <- [i - 1 .. i + 1],
        nj <- [j - 1 .. j + l],
        c <- maybeToList $ Matrix.safeGet ni nj field
    ]

numberValue :: Field -> Number -> Int
numberValue field ((i, j), l) = read . Vector.toList $ Vector.slice (j - 1) l (getRow i field)

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

findAnswer :: Field -> Int
findAnswer field = sum $ map (numberValue field) numbersAdjacentToSymbol
  where
    numbers = findNumbers field
    numbersAdjacentToSymbol = filter (adjacentToSymbol field) numbers

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = Matrix.fromLists input
  print $ findAnswer field
