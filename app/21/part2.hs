{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

steps :: Field -> Set Pos -> [Int]
steps field currs = Set.size currs : steps field newCurrs
  where
    newCurrs = Set.unions $ Set.map (neighbors field) currs

stepsFromStart :: Field -> Pos -> [Int]
stepsFromStart field start = steps field (Set.singleton start)

findAnswer :: Field -> Pos -> Int
findAnswer field@((br, bc), _) start@(sr, sc) = totFull + totFullAlt + totCorners + totSidesSmall + totSidesBig
  where
    s = 26501365
    m = br
    (mlt, left) = s `divMod` m

    [full, fullAlt] = take 2 $ drop m $ stepsFromStart field start
    corners = map (\p -> stepsFromStart field p !! (m - 1)) [(sr, 1), (sr, bc), (1, sc), (br, sc)]
    sidesSmall = map (\p -> stepsFromStart field p !! (left - 1)) [(1, 1), (1, bc), (br, 1), (br, bc)]
    sidesBig = map (\p -> stepsFromStart field p !! (m + left - 1)) [(1, 1), (1, bc), (br, 1), (br, bc)]

    totFull = ((mlt - 1) ^ 2) * full
    totFullAlt = mlt ^ 2 * fullAlt
    totCorners = sum corners
    totSidesSmall = mlt * sum sidesSmall
    totSidesBig = (mlt - 1) * sum sidesBig

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let field = [((r, c), x) | (r, line) <- zip [1 ..] input, (c, x) <- zip [1 ..] line]
  let rocks = Set.fromList [(r, c) | ((r, c), x) <- field, x == '#']
  let start = head [(r, c) | ((r, c), x) <- field, x == 'S']
  let bounds = (length input, length $ head input)
  print $ findAnswer (bounds, rocks) start
