import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type SubMap = (Int, Int, Int) -- dst, src, range

type Map = [SubMap]

subMapping :: SubMap -> (Int, Int) -> Maybe ((Int, Int), (Int, Int)) -- src, dst
subMapping (dst, src, range) (ss, se)
  | overlapE >= overlapS = Just ((overlapS, overlapE), (overlapS + delta, overlapE + delta))
  | otherwise = Nothing
  where
    overlapS = max src ss
    overlapE = min (src + range) se
    delta = -src + dst

remaining :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
remaining (ss, se) []
  | se > ss = [(ss, se)]
  | otherwise = []
remaining (ss, se) ((rs, re) : xs)
  | rs > ss = (ss, rs) : remaining (re, se) xs
  | otherwise = remaining (re, se) xs

mapping :: Map -> (Int, Int) -> [(Int, Int)]
mapping subMaps seed = matchesDsts ++ remaining seed matchesSrcRanges
  where
    matches = mapMaybe (`subMapping` seed) subMaps
    matchesDsts = map snd matches
    matchesSrcRanges = sort $ map fst matches

mappings :: Map -> [(Int, Int)] -> [(Int, Int)]
mappings subMaps = concatMap (mapping subMaps)

seedLocations :: [Map] -> (Int, Int) -> [(Int, Int)]
seedLocations maps seed = foldl (flip mappings) [seed] maps

findAnswer :: [Map] -> [(Int, Int)] -> Int
findAnswer maps seedss = minimum [x | seed <- seedss, (x, _) <- seedLocations maps seed]

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)
tuplify3 _ = error "list is not of length 3"

parseSeeds' :: [String] -> [(Int, Int)]
parseSeeds' (x : y : ys) = (read x, read x + read y) : parseSeeds' ys
parseSeeds' _ = []

parseSeeds :: String -> [(Int, Int)]
parseSeeds = parseSeeds' . drop 1 . splitOn " "

parseSubMap :: String -> SubMap
parseSubMap = tuplify3 . map read . splitOn " "

parseMap :: [String] -> Map
parseMap (_ : xs) = map parseSubMap xs
parseMap _ = error "map does not have correct format"

parseMaps :: [String] -> [Map]
parseMaps = drop 1 . map parseMap . splitOn [""]

parseInput :: [String] -> ([(Int, Int)], [Map]) -- seeds, maps
parseInput (x : xs) = (parseSeeds x, parseMaps xs)
parseInput _ = error "input does not have correct format"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let (seeds, maps) = parseInput input
  print $ findAnswer maps seeds
