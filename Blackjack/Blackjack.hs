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
data Baralho = Baralho [Carta] deriving(Show)

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
                             , valorCreditos :: Int
                             }


--B1
inicializa :: Baralho -> EstadoJogo
inicializa (Baralho xs) = EstadoJogo { baralhoAtual = Baralho xs
                                      , cartasJogador = []
                                      , cartasCasa = []
                                      , aposta = 0
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
terminado (EstadoJogo (Baralho baralho) _ _ _  creditos)
    | creditos == 0 || length baralho <= 20 = True
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
getSum xs = foldl (\acc s -> if ((acc+last s == 21)&& length xs ==1)  then acc + last s else acc + head s ) 0 (map getValueCard (alteraAParaUltimo xs))

alteraAParaUltimo :: Mao -> Mao
alteraAParaUltimo [] = []
alteraAParaUltimo (x:xs)
    | 'A' `elem` x = alteraAParaUltimo xs ++ [x]
    | otherwise = x : alteraAParaUltimo xs

--C.
data Estrategia = SempreStand | SempreHit | HitUntil

headBaralho :: Baralho -> Carta
headBaralho (Baralho (x:xs)) = x

retiraCarta :: Baralho -> Baralho
retiraCarta (Baralho xs) = Baralho (tail xs)


--C1.
sempreStand :: Estrategia
sempreStand = SempreStand

sempreHit :: Estrategia
sempreHit = SempreHit

hitUntil :: Estrategia
hitUntil = HitUntil

valorCartasJogador :: EstadoJogo -> Int
valorCartasJogador estadoJogo = getSum (cartasJogador estadoJogo)

valorCartasCasa :: EstadoJogo -> Int
valorCartasCasa estadoJogo = getSum (cartasCasa estadoJogo)

--C2
simulaRonda :: Estrategia -> EstadoJogo -> EstadoJogo
simulaRonda estrategia estadoJogo =
                           let novoEstado = drawInitialCards estadoJogo
                           in
                           case estrategia of
                           SempreStand -> (decisaoCasa (decisaoJogador estrategia novoEstado))
                           SempreHit -> (decisaoCasa (decisaoJogador estrategia novoEstado))
                           HitUntil -> (decisaoCasa (decisaoJogador estrategia novoEstado))


decisaoJogador :: Estrategia -> EstadoJogo -> EstadoJogo
decisaoJogador estrategia estadoJogo =
  case estrategia of
    SempreStand -> estadoJogo
    SempreHit -> if (valorCartasJogador estadoJogo < 21) then decisaoJogador estrategia (hitJogador estadoJogo) else estadoJogo
    HitUntil -> if (valorCartasJogador estadoJogo < 15) then decisaoJogador estrategia (hitJogador estadoJogo) else estadoJogo
 


decisaoCasa :: EstadoJogo -> EstadoJogo
decisaoCasa estadoJogo = if (valorCartasJogador estadoJogo > 21) then derrota estadoJogo
                         else
                            if (valorCartasCasa estadoJogo < 17)
                            then decisaoCasa (hitCasa estadoJogo)
                            else comparaMaos estadoJogo

comparaMaos :: EstadoJogo -> EstadoJogo
comparaMaos estadoJogo
    | (valorCartasCasa estadoJogo > 21) = vence estadoJogo
    | (valorCartasJogador estadoJogo > valorCartasCasa estadoJogo) && (valorCartasJogador estadoJogo <= 21) = vence estadoJogo
    | otherwise = derrota estadoJogo


drawInitialCards :: EstadoJogo -> EstadoJogo
drawInitialCards estadoJogo = 
  case baralhoAtual estadoJogo of
  Baralho (x:y:z:k:t) -> estadoJogo { baralhoAtual = Baralho t, cartasJogador = [x, y], cartasCasa = [z, k], aposta = 5}
  _ -> estadoJogo

                  

hitJogador :: EstadoJogo -> EstadoJogo
hitJogador estadoJogo = estadoJogo {cartasJogador = cartasJogador estadoJogo ++ [headBaralho (baralhoAtual estadoJogo)],
                                    baralhoAtual = retiraCarta (baralhoAtual estadoJogo)
                                    }

hitCasa :: EstadoJogo -> EstadoJogo
hitCasa estadoJogo = estadoJogo {cartasCasa = cartasCasa estadoJogo ++ [headBaralho (baralhoAtual estadoJogo)],
                                baralhoAtual = retiraCarta (baralhoAtual estadoJogo)
                                }


vence :: EstadoJogo -> EstadoJogo 
vence estadoJogo = estadoJogo {valorCreditos = (valorCreditos estadoJogo) + (aposta estadoJogo)}

derrota :: EstadoJogo -> EstadoJogo
derrota estadoJogo = estadoJogo {valorCreditos = max 0 (valorCreditos estadoJogo - aposta estadoJogo)}

empate :: EstadoJogo -> EstadoJogo
empate estadoJogo = estadoJogo



--C3
simulaJogo :: Estrategia -> Baralho -> Int
simulaJogo estrategia baralho = 
      let estadoJogo = simulaJogoAux estrategia (inicializa baralho)
      in
      valorCreditos estadoJogo
      where
          simulaJogoAux :: Estrategia -> EstadoJogo -> EstadoJogo
          simulaJogoAux estrategia estado
              | terminado estado = estado
              | otherwise = simulaJogoAux estrategia (simulaRonda estrategia estado{aposta = 0})


instance Show Estrategia where
    show SempreStand = "Estrategia: apostar sempre 5 créditos, fazer sempre stand"
    show SempreHit = "Estrategia: apostar sempre 5 créditos, fazer sempre hit"
    show HitUntil = "Estrategia: apostar sempre 5 créditos, fazer hit até aos 15"


instance Show EstadoJogo where
    show estado =
        "jogador: " ++ show (cartasJogador estado) ++
        "\ncasa: " ++ show (cartasCasa estado) ++
        "\ncreditos: " ++ show (valorCreditos estado)