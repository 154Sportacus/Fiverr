--Imports:
import Data.Maybe

--Data types and instances:
data SemiRepetitive = SemiRepetitive String

instance Show SemiRepetitive where
  show (SemiRepetitive str) =
    show str

--Exercise 0.
--a)
semiRepetitive :: String -> Maybe SemiRepetitive 
semiRepetitive [] = Nothing
semiRepetitive string 
                        | isPair = Just (SemiRepetitive pairSemi)
                        | otherwise = Just (SemiRepetitive oddSemi)
                        where
                          isPair = length string `mod` 2 == 0
                          size = length string `div` 2
                          pairSemi = take size string
                          oddSemi = take (size + 1) string

--b)
toString :: SemiRepetitive -> String
toString (SemiRepetitive string) = string

toStringMaybe :: Maybe SemiRepetitive -> String
toStringMaybe mb_semi = toString $ fromJust mb_semi
