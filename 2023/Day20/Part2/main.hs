import Data.Map (Map, empty, fromList, (!), insert, assocs, keys,lookup)
import Data.Maybe (isJust, fromJust)
import Prelude hiding (lookup)
import Data.Bifunctor(second)
import Data.Set (Set,insert, member, empty)

data Pulse = Low | High deriving (Eq, Ord, Show)

type Destinations = [Address]

type Address = String

data Module
  = FlipFlop {mem :: Pulse, dests :: Destinations}
  | Conjunction {memMap :: Map Address Pulse, dests :: Destinations}
  | Broadcaster {dests :: Destinations}
  deriving (Eq, Ord, Show)

main = do
  contents <- readFile "input.txt"
  let modules = fromList $ map parseModule $ filter (/= "") $ lines contents
  let populatedModules = populateConjunctions modules $ assocs modules
  let getcycleForAddress = getCycleLengthAndContribution populatedModules 0
  --These are the four input addresses to the Conjunction that outputs to rx
  print $ (getcycleForAddress "th" + 1) * (getcycleForAddress "sv" + 1) * (getcycleForAddress "gh" + 1) * (getcycleForAddress "ch" + 1)
  
getCycleLengthAndContribution :: Map Address Module -> Int -> String -> Int
getCycleLengthAndContribution moduleMap currLength match
    | any (\(_,p,d) -> p == Low && d == match) allPulses = currLength
    | otherwise = getCycleLengthAndContribution nextMap (currLength + 1) match
    where (p,nextMap) = executePulses moduleMap [("",Low,"broadcaster")]
          allPulses = concat p

populateConjunctions :: Map Address Module -> [(Address,Module)] ->  Map Address Module
populateConjunctions moduleMap [] = moduleMap
populateConjunctions moduleMap ((addr,mod):mods) = populateConjunctions (foldl (addSourceToMemMap addr) moduleMap destMods) mods
    where destMods = getAllExistingModules moduleMap $ dests mod

executePulses :: Map Address Module -> [(Address,Pulse,Address)] -> ([[(Address,Pulse,Address)]],Map Address Module)
executePulses moduleMap pulses
    | null outputPulses = ([pulses],outputMap)
    | otherwise = (pulses:nextPulses,finalMap)
    where (outputPulses,outputMap) = executePulseStep moduleMap pulses
          (nextPulses,finalMap) = executePulses outputMap outputPulses

executePulseStep :: Map Address Module -> [(Address,Pulse,Address)] -> ([(Address,Pulse,Address)],Map Address Module)
executePulseStep moduleMap [] = ([],moduleMap)
executePulseStep moduleMap (pulse:pulses) = (outputPulses ++ nextOutputPulses,finalMap)
    where  (outputPulses,updatedMap) = executePulseOnModule moduleMap pulse
           (nextOutputPulses,finalMap) = executePulseStep updatedMap pulses

executePulseOnModule :: Map Address Module -> (Address,Pulse,Address) -> ([(Address,Pulse,Address)],Map Address Module)
executePulseOnModule moduleMap (prevAddr,pulse,nextAddr)
    | Nothing <- targetModule = ([],moduleMap)
    | Just (Broadcaster dests) <- targetModule = (map (\d -> (nextAddr,pulse,d)) dests,moduleMap)
    | Just (FlipFlop mem dests) <- targetModule =
        case pulse of
        High -> ([],moduleMap)
        Low -> (map (\d -> (nextAddr,pulseFlip mem,d)) dests, Data.Map.insert nextAddr (FlipFlop (pulseFlip mem) dests) moduleMap)
    | Just (Conjunction memMap dests) <- targetModule =
        let updatedMemMap = Data.Map.insert prevAddr pulse memMap
        in if all ((== High) . snd) (assocs updatedMemMap)
            then (map (\d -> (nextAddr,Low,d)) dests,Data.Map.insert nextAddr (Conjunction updatedMemMap dests) moduleMap)
            else (map (\d -> (nextAddr,High,d)) dests,Data.Map.insert nextAddr (Conjunction updatedMemMap dests) moduleMap)
    where targetModule = lookup nextAddr moduleMap


getAllExistingModules :: Map Address Module -> Destinations -> [(Address,Module)]
getAllExistingModules moduleMap dests = map (second fromJust) $ filter (\(_,x) -> isJust x) $ map (\addr -> (addr,lookup addr moduleMap)) dests

pulseFlip :: Pulse -> Pulse
pulseFlip High = Low
pulseFlip Low = High


addSourceToMemMap :: Address -> Map Address Module -> (Address,Module) -> Map Address Module
addSourceToMemMap sourceAddr moduleMap (desAddr,Conjunction memMap dests) = Data.Map.insert desAddr (Conjunction (Data.Map.insert sourceAddr Low memMap) dests) moduleMap
addSourceToMemMap _ moduleMap _ = moduleMap

parseModule :: String -> (Address, Module)
parseModule str
  | typeAddr == "broadcaster" = ("broadcaster", Broadcaster dests)
  | ('%' : addr) <- typeAddr = (addr, FlipFlop Low dests)
  | ('&' : addr) <- typeAddr = (addr, Conjunction Data.Map.empty dests)
  where
    (typeAddr : _ : dests) = words $ filter (/= ',') str