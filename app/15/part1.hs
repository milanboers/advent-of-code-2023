import Data.Char (ord)
import Data.List.Split (splitOn)

hashC :: Int -> Char -> Int
hashC i c = ((i + ord c) * 17) `mod` 256

hash :: String -> Int
hash = foldl hashC 0

findAnswer :: [String] -> Int
findAnswer = sum . map hash

main :: IO ()
main = do
  contents <- getContents
  let steps = splitOn "," contents
  print $ findAnswer steps
