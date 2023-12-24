{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)

type Point = (Integer, Integer)

type PointF = (Rational, Rational)

type Vec = (Point, Point) -- A -> B

lineIntersection :: Vec -> Vec -> Maybe PointF
lineIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = case d of
  0 -> Nothing -- parallel
  _ -> Just (pxn / d, pyn / d)
  where
    pxn = fromIntegral $ (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
    pyn = fromIntegral $ (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
    d = fromIntegral $ (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

inFuture :: Vec -> PointF -> Bool
inFuture ((px1, py1), (px2, py2)) (ix, iy) = xSameDir && ySameDir
  where
    px1f = fromIntegral px1
    py1f = fromIntegral py1
    xSameDir = px2 > px1 && ix > px1f || px2 < px1 && ix < px1f
    ySameDir = py2 > py1 && iy > py1f || py2 < py1 && iy < py1f

inArea :: (Int, Int) -> PointF -> Bool
inArea (min', max') (x, y) = x >= minF && x <= maxF && y >= minF && y <= maxF
  where
    minF = fromIntegral min'
    maxF = fromIntegral max'

intersects :: (Int, Int) -> Vec -> Vec -> Bool
intersects area v1 v2 = case lineIntersection v1 v2 of
  Just p -> inArea area p && inFuture v1 p && inFuture v2 p
  Nothing -> False

combinations :: [a] -> [(a, a)]
combinations [] = []
combinations (x : xs) = map (x,) xs ++ combinations xs

findAnswer :: [Vec] -> Int
findAnswer vecs = length $ filter (uncurry (intersects area)) combos
  where
    -- area = (7, 27)
    area = (200000000000000, 400000000000000)
    combos = combinations vecs

parsePoint :: String -> Point
parsePoint = (\[x, y, _] -> (x, y)) . map read . splitOn ", "

dirToPoint :: Point -> (Integer, Integer) -> Point
dirToPoint (px, py) (dx, dy) = (px + dx, py + dy)

parseVec :: String -> Vec
parseVec = (\[p, d] -> (p, dirToPoint p d)) . map parsePoint . splitOn "@"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let vecs = map parseVec input
  print $ findAnswer vecs
