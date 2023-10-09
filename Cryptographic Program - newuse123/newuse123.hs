--Imports:
import System.IO (readFile)


import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import Data.Char (isAlphaNum, isAlpha, isSpace, toLower)
import Data.List (maximumBy, minimumBy, sortBy)
import Data.Tuple (swap)
import Data.Map (Map, fromListWith, toList, fromList)

import Data.Function (on)


import qualified Data.Map as Map
import qualified Data.Set as Set



--Types:
type Alphabet = String
type Key = [(Char, Char)]
type FrequencyTable = [(Char, Double)]
type KeyGeneric a = (a, a)
type Dictionary = Set.Set String



--Task 1:
--a)

-- Function to encrypt plaintext using the given key
encode :: Key -> String -> String
encode key plaintext = map (encoder key) plaintext
                    where
                      encoder :: Key -> Char -> Char
                      encoder [] c = c
                      encoder (h:tail) c = let (k,v) = h in
                                             if (c==k) then v else encoder tail c


--b)

decode :: Key -> String -> String
decode key ciphertext = encode (invert key) ciphertext
                      where
                        invert :: Key -> Key
                        invert key = [(v, k) | (k, v) <- key]


--c)
caesar :: Alphabet -> Integer -> Key
caesar [] x = []
caesar list x = aux x list list
  where
    aux :: Integer -> Alphabet -> Alphabet -> Key
    aux x [] _ = []
    aux x (h:t) list =  (generateKey x h list):aux x t ((tail list)++[head list])
      where
        generateKey :: Integer -> Char -> Alphabet -> (Char,Char)
        generateKey 0 c (h:t) = (c,h)
        generateKey x c (h:t) = generateKey (x-1) c t

--Task 2:
--a)
lengthDouble :: String -> Double
lengthDouble [] = 0
lengthDouble (h:t) = 1.0 + lengthDouble t

count :: String -> FrequencyTable
count l =
  let charCounts = fromListWith (+) [(char, 1.0) | char <- l]
      totalChars = lengthDouble l
  in toList (Map.map (/ totalChars) charCounts)


--b)

loadFrequencyTable :: FilePath -> IO FrequencyTable
loadFrequencyTable filePath = do
  fileContents <- readFile filePath
  return $ count fileContents

--c)

initialGuess :: FrequencyTable -> FrequencyTable -> Key
initialGuess observedModel model =
    zip (map fst sortedObserved) (map fst sortedModel)
  where
    sortedObserved = sortBy (flip compare `on` snd) (observedModel)
    sortedModel = sortBy (flip compare `on` snd) (model)


--d)
calculateChiSquared :: (Char, Double) -> (Char, Double) -> Double
calculateChiSquared (_, observedFreq) (_, expectedFreq) =
    ((observedFreq - expectedFreq) ** 2) / expectedFreq

chiSquared :: FrequencyTable -> FrequencyTable -> Double
chiSquared observed model =
    sum $ zipWith calculateChiSquared (observed) (model)




--Task 3:
--a) && b)
updateEntry :: Eq a => (a, a) -> (a, a) -> (a, a) -> (a, a)
updateEntry p1 p2 entry
  | entry == p1 = p2
  | entry == p2 = p1
  | otherwise   = entry

swapEntriesAux :: Eq a => (a, a) -> (a, a) -> [(a, b)] -> [(a, b)]
swapEntriesAux p1 p2 key = map update key
  where
    update entry@(c1, c2)
      | c1 == fst p1 = (fst p2, c2)
      | c1 == fst p2 = (fst p1, c2)
      | otherwise = entry

swapEntries :: Eq a => (a, a) -> (a, a) -> [(a, b)] -> [(a, b)]
swapEntries p1 p2 key = let list = swapEntriesAux p1 p2 key in
                        case list of
                          [] -> []
                          (h:x:t) -> x:h:t
                          list -> list

--c)
neighbourKeys :: Key -> [Key]
neighbourKeys key =
  [swapEntries pair1 pair2 key | pair1 <- key, pair2 <- key, pair1 /= pair2]


--d) 
calculateXSquare :: String -> FrequencyTable -> Double
calculateXSquare decryptedText model =
  sum $ zipWith calculateChiSquared (observedFreq) (model) 
  where
    observedFreq = count decryptedText

calculateXSquareForKey :: String -> Key -> FrequencyTable -> Double
calculateXSquareForKey ciphertext key model =
  calculateXSquare (decode key ciphertext) model

greedy :: FrequencyTable -> String -> Key -> Key
greedy model ciphertext initialKey =
  let
    currentXSquare = calculateXSquareForKey ciphertext initialKey model

    findBestKey :: Key -> [Key] -> Key
    findBestKey currentKey [] = currentKey
    findBestKey currentKey (neighbor:neighbors) =
      let
        neighborXSquare = calculateXSquareForKey ciphertext neighbor model
      in
        if neighborXSquare < currentXSquare
          then findBestKey neighbor neighbors
          else findBestKey currentKey neighbors

    bestKey = findBestKey initialKey (neighbourKeys initialKey)
  in
    bestKey



--Task 4:
--a)
loadDictionary :: FilePath -> IO Dictionary
loadDictionary filePath = do
  contents <- readFile filePath
  let wordsList = words contents
      uniqueWords = Set.fromList wordsList
  return uniqueWords

--b)
countValidWords :: Dictionary -> String -> Integer
countValidWords dictionary text =
  let wordsList = words text
      validWordsCount = length $ filter (`Set.member` dictionary) wordsList
  in fromIntegral validWordsCount


--c)

greedyDict :: Dictionary -> String -> Key -> Key
greedyDict dictionary cipherText key = go 10 key 
  where
    go 0 bestKey = bestKey
    go n currentKey =
      let
        neighborKeys = neighbourKeys currentKey
        bestNeighbor = maximumBy (comparing (\k -> countValidWords dictionary (decode k cipherText))) neighborKeys
      in
        if countValidWords dictionary (decode bestNeighbor cipherText) > countValidWords dictionary (decode currentKey cipherText)
          then go (n - 1) bestNeighbor
          else currentKey

