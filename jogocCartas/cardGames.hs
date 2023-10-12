--Imports:
import Data.Char
import Data.List (sort)

--Types:
type Card = String
type Hand = [Card]
type HandPair = (Card, Card)
type Deck = [Card]
  
--A.Baralho)
baralho :: Deck
baralho = ["AS","2S","3S","4S","5S","6S","7S","8S","9S","TS","JS","QS","KS",
    "AD","2D","3D","4D","5D","6D","7D","8D","9D","TD","JD","QD","KD",
    "AC","2C","3C","4C","5C","6C","7C","8C","9C","TC","JC","QC","KC",
    "AH","2H","3H","4H","5H","6H","7H","8H","9H","TH","JH","QH","KH"]

--B.Blackjack)
--Calcula o valor total de uma mão.
handValues :: Hand -> [Int]
handValues [] = [0]
handValues (card:cards) =
                    let valuesForCard = cardValue card
                        valuesForRemainingCards = handValues cards
                    in [val + value | val <- valuesForCard, value <- valuesForRemainingCards]
                    where
                    --Calcula os possiveis valores de uma carta.
                    cardValue :: Card -> [Int]
                    cardValue (h:t) = case h of
                        'A' -> [1,11]
                        'K' -> [10]
                        'Q' -> [10]
                        'J' -> [10]
                        'T' -> [10]
                        h ->  [digitToInt h]

--Determina Combinacoes possiveis de Blackjack para um determinado número de pontos.
combinacoesBlackjack :: Int -> [HandPair]
combinacoesBlackjack points = 
                    --Hands
                    removeSameHands [(sort c1, sort c2) | 
                    c1 <- baralho, c2 <- baralho, c1 /= c2,
                    any (\val -> val == points) (handValues [c1, c2])]
                    --Remove mãos iguais da lista das combinacoes de Blackjack.
                    where
                    removeSameHands :: [HandPair] -> [HandPair]
                    removeSameHands [] = []							 
                    removeSameHands (h:t) = h:tail
                            where tail = removeSameHands (filter (sameHand h) t)
                    --Lógica utilizada para determinar se duas mãos são iguais.
                    sameHand :: HandPair -> HandPair -> Bool
                    sameHand (x1,y1) (x2,y2)
                        | x1==x2 && y1==y2 = False
                        | x1==y2 && y1==x2 = False
                        | otherwise = True

--C. Poker)
-- Gera todas as combinações possiveis de n cartas.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations n (x:xs) = [x : rest | rest <- combinations (n - 1) xs] ++ combinations n xs

-- Gera todas as mãos possíveis com um trio e um par
generateFullHouses :: Deck -> [Hand]
generateFullHouses deck = 
                    --Fullhouses
                    [[a, b, c, d, e] | 
                    [a, b, c] <- combinations 3 deck,
                    [d, e] <- combinations 2 (filter (\x -> x `notElem` [a, b, c]) deck),
                    --Condicões lógicas para selecao de cartas validas a criarem um trio [a, b, c] de um full house
                    head a == head b,
                    head a == head c,
                    secondElement  a /= secondElement  b,
                    secondElement  a /= secondElement  c,
                    secondElement  b /= secondElement  c,
                    --Condicões lógicas para selecao de cartas validas a criarem um duo [d, e] de um full house
                    head d == head e,
                    secondElement  d /= secondElement  e]
                    where
                    --Funcao para retirarmos o segundo elemento de uma lista.
                    secondElement :: Card -> Char           
                    secondElement (_:x:_) = x 
 
-- Gera a lista das mãos de Poker que correspondem a Full Houses
fullHouses :: [Hand]
fullHouses = generateFullHouses baralho