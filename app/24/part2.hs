{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

type Point = (Integer, Integer, Integer)

type Dir = (Integer, Integer, Integer)

type Point2 = (Integer, Integer)

type PointF = (Rational, Rational, Rational)

type PointF2 = (Rational, Rational)

type Vec = (Point, Point) -- A -> B

type Vec2 = (Point2, Point2) -- A -> B

lineIntersection :: Vec2 -> Vec2 -> Maybe PointF2
lineIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4)) = case d of
  0 -> Nothing -- parallel
  _ -> Just (pxn / d, pyn / d)
  where
    pxn = fromIntegral $ (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)
    pyn = fromIntegral $ (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)
    d = fromIntegral $ (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

lineIntersection3 :: Vec -> Vec -> Maybe PointF
lineIntersection3 ((x1, y1, z1), (x2, y2, z2)) ((x3, y3, z3), (x4, y4, z4)) = case (xyInt, yzInt, xzInt) of
  (Just (x, y), Just (y', z), _) | y == y' -> Just (x, y, z)
  (Just (x, y), _, Just (x', z)) | x == x' -> Just (x, y, z)
  (_, Just (y, z'), Just (x, z)) | z == z' -> Just (x, y, z)
  _ -> Nothing
  where
    xyInt = lineIntersection ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
    yzInt = lineIntersection ((y1, z1), (y2, z2)) ((y3, z3), (y4, z4))
    xzInt = lineIntersection ((x1, z1), (x2, z2)) ((x3, z3), (x4, z4))

getDir :: Vec -> Dir
getDir ((v1x, v1y, v1z), (v2x, v2y, v2z)) = (v2x - v1x, v2y - v1y, v2z - v1z)

normalizeDir :: Vec -> Dir -> Vec
normalizeDir v@(p1@(v1x, v1y, v1z), _) (dx, dy, dz) = (p1, (v1x + ndx, v1y + ndy, v1z + ndz))
  where
    (vdx, vdy, vdz) = getDir v
    (ndx, ndy, ndz) = (vdx - dx, vdy - dy, vdz - dz)

dirs :: [Dir]
dirs = dirs' 0
  where
    dirs' d =
      [(x, y, z) | x <- [-d .. d], y <- [-d, d], z <- [-d, d]]
        ++ [(x, y, d) | x <- [-d .. d], y <- [-d .. d]]
        ++ [(x, -d, z) | x <- [-d .. d], z <- [-d .. d]]
        ++ [(x, d, z) | x <- [-d .. d], z <- [-d .. d]]
        ++ [(-d, y, z) | y <- [-d .. d], z <- [-d .. d]]
        ++ [(d, y, z) | y <- [-d .. d], z <- [-d .. d]]
        ++ dirs' (d + 1)

intersectionPoint :: Vec -> Vec -> Dir -> Maybe PointF
intersectionPoint vec1 vec2 dir = lineIntersection3 normalized1 normalized2
  where
    normalized1 = normalizeDir vec1 dir
    normalized2 = normalizeDir vec2 dir

allIntersectionPoint :: [Vec] -> Dir -> Maybe PointF
allIntersectionPoint vecs dir = case (mi1, mi2) of
  (Just i1, Just i2) | i1 == i2 -> Just i1
  _ -> Nothing
  where
    [v1, v2, v3] = take 3 vecs
    mi1 = intersectionPoint v1 v2 dir
    mi2 = intersectionPoint v2 v3 dir

findAnswer :: [Vec] -> Integer
findAnswer vecs = round $ x + y + z
  where
    (x, y, z) = head $ catMaybes [allIntersectionPoint vecs dir | dir <- dirs]

parsePoint :: String -> Point
parsePoint = (\[x, y, z] -> (x, y, z)) . map read . splitOn ", "

dirToPoint :: Point -> Dir -> Point
dirToPoint (px, py, pz) (dx, dy, dz) = (px + dx, py + dy, pz + dz)

parseVec :: String -> Vec
parseVec = (\[p, d] -> (p, dirToPoint p d)) . map parsePoint . splitOn "@"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let vecs = map parseVec input
  print $ findAnswer vecs
