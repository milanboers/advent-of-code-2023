import Data.List.Split (splitOn)

arrangements :: String -> [Int] -> Bool -> Int
arrangements "" [0] _ = 1
arrangements "" [] _ = 1
arrangements ('.' : xs) (0 : gs) _ = arrangements xs gs False
arrangements ('.' : xs) groups False = arrangements xs groups False
arrangements ('#' : _) (0 : _) _ = 0
arrangements ('#' : xs) (g : gs) _ = arrangements xs (g - 1 : gs) True
arrangements ('?' : xs) groups inGroup = operational + damaged
  where
    operational = arrangements ('.' : xs) groups inGroup
    damaged = arrangements ('#' : xs) groups inGroup
arrangements _ _ _ = 0

findAnswer :: [(String, [Int])] -> Int
findAnswer records = sum $ map (\(s, g) -> arrangements s g False) records

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
