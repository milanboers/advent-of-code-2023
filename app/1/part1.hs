import Data.Char (isDigit)

lineNumber :: String -> Int
lineNumber line = read [head filteredLine, last filteredLine]
  where
    filteredLine = filter isDigit line

findAnswer :: [String] -> Int
findAnswer = sum . map lineNumber

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  print $ findAnswer input
