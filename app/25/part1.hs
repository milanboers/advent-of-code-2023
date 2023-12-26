{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import System.Random (StdGen, randomR)
import System.Random.Stateful (mkStdGen)

type Network = Map String [String]

getAllWires :: Network -> [(String, String)]
getAllWires = concat . Map.foldlWithKey (\wires from tos -> map (from,) tos : wires) []

contract :: StdGen -> Network -> (StdGen, Network)
contract rng net = (newRng, newNet)
  where
    wires = getAllWires net
    (wirei, newRng) = randomR (0, length wires - 1) rng
    (u, v) = wires !! wirei
    uv = u ++ "," ++ v
    uvEdges = [x | x <- net ! u ++ net ! v, x /= u && x /= v]
    newNet =
      Map.insert uv uvEdges
        . Map.map (map (\value -> if value == u || value == v then uv else value))
        . Map.delete u
        . Map.delete v
        $ net

contractAll :: StdGen -> Network -> (StdGen, Network)
contractAll rng net | Map.size net == 2 = (rng, net)
contractAll rng net = contractAll newRng newNet
  where
    (newRng, newNet) = contract rng net

contractUntil3 :: StdGen -> Network -> Network
contractUntil3 rng net = case cutSize of
  3 -> contracted
  _ -> contractUntil3 newRng net
  where
    (newRng, contracted) = contractAll rng net
    cutSize = length . snd . head $ Map.toList contracted

findAnswer :: Network -> Int
findAnswer net = product groups
  where
    rng = mkStdGen 42
    contracted = contractUntil3 rng net
    groups = map (length . splitOn ",") $ Map.keys contracted

insertEdge :: Maybe [String] -> String -> [String]
insertEdge Nothing e = [e]
insertEdge (Just xs) e = e : xs

addEdge :: Network -> String -> String -> Network
addEdge net from to =
  Map.alter (\xs -> Just $ insertEdge xs from) to $
    Map.alter (\xs -> Just $ insertEdge xs to) from net

addEdges :: Network -> String -> [String] -> Network
addEdges net from = foldl (`addEdge` from) net

parseEdges :: String -> (String, [String])
parseEdges xs = (from, words tos)
  where
    [from, tos] = splitOn ": " xs

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let edges = map parseEdges input
  let net = foldl (\net' (from, tos) -> addEdges net' from tos) Map.empty edges
  print $ findAnswer net
