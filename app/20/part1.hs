{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)

data Type = Flip | Conj deriving (Show, Eq)

type Module = (Type, [String])

type Modules = Map String Module

data PulseAmp = Low | High deriving (Show, Eq, Ord)

type Pulse = (String, String, PulseAmp) -- from, to, amp

data ModuleState = FlipState Bool | ConjState (Map String PulseAmp) deriving (Show, Eq, Ord)

type State = Map String ModuleState

outputPulses :: String -> [String] -> PulseAmp -> [Pulse]
outputPulses from tos amp = [(from, to, amp) | to <- tos]

nextPulse :: ModuleState -> Pulse -> [String] -> (ModuleState, [Pulse])
nextPulse s@(FlipState _) (_, _, High) _ = (s, [])
nextPulse (FlipState False) (_, to, Low) outputs = (FlipState True, outputPulses to outputs High)
nextPulse (FlipState True) (_, to, Low) outputs = (FlipState False, outputPulses to outputs Low)
nextPulse (ConjState inputs) (from, to, Low) outputs = (ConjState newInputs, outputPulses to outputs High)
  where
    newInputs = Map.insert from Low inputs
nextPulse (ConjState inputs) (from, to, High) outputs = (ConjState newInputs, outputPulses to outputs newAmp)
  where
    newInputs = Map.insert from High inputs
    newAmp = if all (== High) newInputs then Low else High

handlePulse :: Modules -> State -> Pulse -> (State, [Pulse])
handlePulse mods state (_, to, _) | to `Map.notMember` mods = (state, [])
handlePulse mods state pulse@(_, to, _) = (newState, newPulses)
  where
    (_, outputs) = mods ! to
    modState = state ! to
    (newModState, newPulses) = nextPulse modState pulse outputs
    newState = Map.insert to newModState state

deliverPulses :: Modules -> State -> [Pulse] -> Int -> Int -> (State, Int, Int)
deliverPulses _ state [] lows highs = (state, lows, highs)
deliverPulses mods state (p@(_, _, amp) : ps) lows highs =
  deliverPulses mods newState (ps ++ newPulses) newLows newHighs
  where
    (newState, newPulses) = handlePulse mods state p
    newLows = if amp == Low then lows + 1 else lows
    newHighs = if amp == High then highs + 1 else highs

deliverPulsess :: Modules -> State -> [Pulse] -> [(Int, Int)]
deliverPulsess mods state pulses = (nextLows, nextHighs) : deliverPulsess mods nextState pulses
  where
    (nextState, nextLows, nextHighs) = deliverPulses mods state pulses 1 0

buildModuleState :: Module -> [String] -> ModuleState
buildModuleState (Flip, _) _ = FlipState False
buildModuleState (Conj, _) inputs = ConjState . Map.fromList $ map (,Low) inputs

findInputs :: Modules -> String -> [String]
findInputs mods x = [label | (label, (_, outputs)) <- Map.toList mods, x `elem` outputs]

buildState :: Modules -> State
buildState mods = Map.mapWithKey (\label m -> buildModuleState m (findInputs mods label)) mods

findAnswer :: Modules -> [String] -> Int
findAnswer mods initial = lows * highs
  where
    initState = buildState mods
    initPulses = [("/", to, Low) | to <- initial]
    allButtonPresses = take 1000 $ deliverPulsess mods initState initPulses
    lows = sum $ map fst allButtonPresses
    highs = sum $ map snd allButtonPresses

parseLabel :: String -> String
parseLabel = head . splitOn " -> "

parseTargets :: String -> [String]
parseTargets = splitOn ", " . last . splitOn " -> "

parseModule :: String -> Maybe (String, Module)
parseModule ('%' : xs) = Just (parseLabel xs, (Flip, parseTargets xs))
parseModule ('&' : xs) = Just (parseLabel xs, (Conj, parseTargets xs))
parseModule _ = Nothing

parseBroadcast :: String -> Maybe [String]
parseBroadcast ('b' : xs) = Just $ parseTargets xs
parseBroadcast _ = Nothing

main :: IO ()
main = do
  contents <- getContents
  let input = lines contents
  let mods = Map.fromList $ mapMaybe parseModule input
  let initial = head $ mapMaybe parseBroadcast input
  print $ findAnswer mods initial
