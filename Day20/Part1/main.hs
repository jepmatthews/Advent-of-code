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
  let modules = fromList $ map parseModule $ lines contents
  let populatedModules = populateConjunctions modules $ assocs modules
  let (_,(lContrib,hContrib)) = getCycleLengthAndContribution populatedModules Data.Set.empty (0,(0,0))
  print $ lContrib * hContrib



getCycleLengthAndContribution :: Map Address Module -> Set (Map Address Module) -> (Int,(Int,Int)) -> (Int,(Int,Int))
getCycleLengthAndContribution moduleMap modMapSet (currLength,(curentLContrib,currentHContrib))
    | currLength == 1000 = (currLength,(curentLContrib,currentHContrib))
    | nextMap `member` updatedSet = updatedContributions
    | otherwise = getCycleLengthAndContribution nextMap updatedSet updatedContributions
    where (p,nextMap) = executePulses moduleMap [("",Low,"broadcaster")]
          allPulses = concat p
          lContrib = length $ filter (\(_,p,_) -> p == Low) allPulses
          hContrib = length $ filter (\(_,p,_) -> p == High) allPulses
          updatedSet = Data.Set.insert moduleMap modMapSet 
          updatedContributions = (currLength + 1,(curentLContrib + lContrib,currentHContrib + hContrib))

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