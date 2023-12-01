import Data.Char (intToDigit)
import Data.List (find, inits, isPrefixOf, isSuffixOf, tails)
import Data.Maybe (mapMaybe)

digitMap :: [(String, Char)]
digitMap =
  literalDigits
    ++ [ ("one", '1'),
         ("two", '2'),
         ("three", '3'),
         ("four", '4'),
         ("five", '5'),
         ("six", '6'),
         ("seven", '7'),
         ("eight", '8'),
         ("nine", '9')
       ]
  where
    literalDigits = map (\i -> (show i, intToDigit i)) [1 .. 9]

prefixChar :: String -> Maybe Char
prefixChar xs = snd <$> find (\(text, _) -> text `isPrefixOf` xs) digitMap

suffixChar :: String -> Maybe Char
suffixChar xs = snd <$> find (\(text, _) -> text `isSuffixOf` xs) digitMap

firstDigit :: String -> Char
firstDigit = head . mapMaybe prefixChar . tails

lastDigit :: String -> Char
lastDigit = last . mapMaybe suffixChar . inits

lineNumber :: String -> Int
lineNumber line = read [firstDigit line, lastDigit line]

findAnswer :: [String] -> Int
findAnswer = sum . map lineNumber

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  print $ findAnswer input
