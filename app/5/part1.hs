import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe, mapMaybe)

type SubMap = (Int, Int, Int) -- dst, src, range

type Map = [SubMap]

subMapping :: SubMap -> Int -> Maybe Int
subMapping (dst, src, range) x
  | x >= src && x <= src + range = Just $ x - src + dst
  | otherwise = Nothing

mapping :: Map -> Int -> Int
mapping subMaps x = case listToMaybe $ mapMaybe (`subMapping` x) subMaps of
  Just m -> m
  _ -> x

seedLocation :: [Map] -> Int -> Int
seedLocation maps seed = foldl (flip mapping) seed maps

findAnswer :: [Map] -> [Int] -> Int
findAnswer maps = minimum . map (seedLocation maps)

tuplify3 :: [a] -> (a, a, a)
tuplify3 [x, y, z] = (x, y, z)
tuplify3 _ = error "list is not of length 3"

parseSeeds :: String -> [Int]
parseSeeds = map read . drop 1 . splitOn " "

parseSubMap :: String -> SubMap
parseSubMap = tuplify3 . map read . splitOn " "

parseMap :: [String] -> Map
parseMap (_ : xs) = map parseSubMap xs
parseMap _ = error "map does not have correct format"

parseMaps :: [String] -> [Map]
parseMaps = drop 1 . map parseMap . splitOn [""]

parseInput :: [String] -> ([Int], [Map]) -- seeds, maps
parseInput (x : xs) = (parseSeeds x, parseMaps xs)
parseInput _ = error "input does not have correct format"

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let (seeds, maps) = parseInput input
  print $ findAnswer maps seeds
