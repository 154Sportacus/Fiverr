-- fc59793,fc59806

module Blackjack (
    Baralho,
    converte,
    tamanho,
    EstadoJogo,
    Estrategia,
    sempreStand,
    sempreHit,
    simulaRonda,
    simulaJogo
) where

import BaralhosExemplo
import Data.Char(digitToInt)
import Data.List (delete)

--A
type Carta = String
data Baralho = Baralho [Carta] deriving (Show)

--A1
converte :: [String] -> Baralho
converte = Baralho . foldr (\x acc -> x : acc) []

--A2
tamanho :: Baralho -> Int
tamanho (Baralho xs) = length xs

--B
type Jogador = Int
type Casa = Int
data Turno = Jogador | Casa deriving(Show)
type Mao = [Carta]
data EstadoJogo = EstadoJogo { baralhoAtual :: Baralho
                             , cartasJogador :: Mao
                             , cartasCasa :: Mao
                             , aposta :: Int
                             , turno :: Turno
                             , valorCreditos :: Int
                             } deriving (Show)

--B1
inicializa :: Baralho -> EstadoJogo
inicializa (Baralho xs) = EstadoJogo { baralhoAtual = Baralho xs
                                      , cartasJogador = []
                                      , cartasCasa = []
                                      , aposta = 0
                                      , turno = Jogador
                                      , valorCreditos = 100
                                      }

ejOrdenado :: EstadoJogo
ejOrdenado = inicializa (converte baralhoOrdenado)

ejInsuficiente :: EstadoJogo
ejInsuficiente = inicializa (converte baralhoInsuficiente)

ejSimples :: EstadoJogo
ejSimples = inicializa (converte baralhoSimples)

--B2
creditos :: EstadoJogo -> Int
creditos = valorCreditos

--B3
baralho :: EstadoJogo -> Baralho
baralho = baralhoAtual

--B4
terminado :: EstadoJogo -> Bool
terminado (EstadoJogo (Baralho baralhoInsuf) _ _ _ _ creditos)
    | creditos == 0 || length baralhoInsuf <= 20 = True
    | otherwise = False

--C
-- vai pegar numa carta e indicar o valor numerico desta
getValueCard :: Carta -> [Int]
getValueCard x
    | head x `elem` ['J' , 'Q' , 'K' , 'T'] = [10,10]
    | head x == 'A' = [1 ,11]
    | otherwise = [digit,digit]
        where
            digit = digitToInt (head x)

-- vai fazer a soma das cartas de um determinado baralho 
getSum :: Mao -> Int
getSum xs = foldl (\acc s -> if acc <= 10 then acc + last s else acc + head s ) 0 (map getValueCard (alteraAParaUltimo xs))
        where
            alteraAParaUltimo :: Mao -> Mao
            alteraAParaUltimo [] = []
            alteraAParaUltimo (x:xs)
                | 'A' `elem` x = alteraAParaUltimo xs ++ [x]
                | otherwise = x : alteraAParaUltimo xs

--C.
type Estrategia = EstadoJogo -> EstadoJogo

valorCartasJogador :: EstadoJogo -> Int
valorCartasJogador estadoJogo = getSum (cartasJogador estadoJogo)

valorCartasCasa :: EstadoJogo -> Int
valorCartasCasa estadoJogo = getSum (cartasCasa estadoJogo)

 --C1. Três estratégias
sempreStand :: Estrategia

sempreHit :: Estrategia

ownStrat :: Estrategia


--C2. Simulação de uma ronda
simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo


--C3. Simulação de um jogo


--C4.
instance Show Estrategia where
    show SempreStand = "Estrategia: apostar sempre 5 créditos, fazer sempre stand"
    show SempreHit = "Estrategia: apostar sempre 5 créditos, fazer sempre hit"
    show ownStrat = "Estrategia: ..."