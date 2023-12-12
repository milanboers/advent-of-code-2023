import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.MemoUgly (memo)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

arrangements :: String -> [Int] -> Bool -> Int
arrangements = curry3 . memo . uncurry3 $ arrangements'
  where
    arrangements' "" [0] _ = 1
    arrangements' "" [] _ = 1
    arrangements' ('.' : xs) (0 : gs) _ = arrangements xs gs False
    arrangements' ('.' : xs) groups False = arrangements xs groups False
    arrangements' ('#' : _) (0 : _) _ = 0
    arrangements' ('#' : xs) (g : gs) _ = arrangements xs (g - 1 : gs) True
    arrangements' ('?' : xs) groups inGroup = operational + damaged
      where
        operational = arrangements ('.' : xs) groups inGroup
        damaged = arrangements ('#' : xs) groups inGroup
    arrangements' _ _ _ = 0

unfold :: (String, [Int]) -> (String, [Int])
unfold (s, g) = (intercalate "?" $ replicate 5 s, concat $ replicate 5 g)

findAnswer :: [(String, [Int])] -> Int
findAnswer records = sum $ map ((\(s, g) -> arrangements s g False) . unfold) records

parseRecord :: String -> (String, [Int])
parseRecord line = (head splitLine, groups)
  where
    splitLine = words line
    groups = map read . splitOn "," $ last splitLine

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let record = map parseRecord input
  print $ findAnswer record
