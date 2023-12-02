{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Char (isDigit)
import Data.List (isSuffixOf)
import Data.List.Split (splitOn)

type Draw = (Int, Int, Int) -- red, green, blue

type Game = (Int, [Draw]) -- id, draws

drawPossible :: Draw -> Bool
drawPossible (r, g, b) = r <= 12 && g <= 13 && b <= 14

gamePossible :: Game -> Bool
gamePossible (_, draws) = all drawPossible draws

findAnswer :: [Game] -> Int
findAnswer = sum . map fst . filter gamePossible

addSubDraws :: Draw -> Draw -> Draw
addSubDraws (r1, g1, b1) (r2, g2, b2) = (r1 + r2, g1 + g2, b1 + b2)

parseSubDraw :: String -> Draw
parseSubDraw xs
  | "red" `isSuffixOf` xs = (amount, 0, 0)
  | "green" `isSuffixOf` xs = (0, amount, 0)
  | "blue" `isSuffixOf` xs = (0, 0, amount)
  | otherwise = error "Not a valid color"
  where
    amount = read $ filter isDigit xs

parseDraw :: String -> Draw
parseDraw = foldl addSubDraws (0, 0, 0) . map parseSubDraw . splitOn ","

parseDraws :: String -> [Draw]
parseDraws = map parseDraw . splitOn ";"

parseGame :: String -> Game
parseGame line = (gameId, parseDraws drawsPart)
  where
    [gamePart, drawsPart] = splitOn ":" line
    gameId = read $ filter isDigit gamePart

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let games = map parseGame input
  print $ findAnswer games
