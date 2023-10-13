import Data.Char

nums :: [Char]
nums = ['A','2','3','4','5','6','7','8','9','T','J','Q','K']

naipes :: [Char]
naipes = ['S','H','D','C']

--Auxiliares
--Usado na alinea a
baralhar :: [Char] -> [Char] -> [String]
baralhar xs xt = [ x : [y] | y <- xt , x <- xs]

--Usado na alinea b
getValues :: String -> [Int]
getValues st | head st `elem` ['J' , 'Q' , 'K' , 'T'] = [10,10]
             | head st `elem` ['A'] = [1 ,11]
             | otherwise = [digit,digit]
             where
                digit = digitToInt (head st)

--Usado na alinea b
removeIguais :: [(String,String)]->[(String,String)]
removeIguais [] = []
removeIguais ((c1,c2):t) = if ((c2,c1) `elem` t) then removeIguais t else (c1,c2):removeIguais t

--Usado na alinea b
second :: [Int] -> Int
second (h:t) = head t

--Usado na alinea c
second_full :: String -> Char
second_full (h:t) = head t


--a
baralho :: [String]
baralho = baralhar nums naipes


--b
combinacoesBlackJack :: Int -> [(String,String)]
combinacoesBlackJack 0 = []
combinacoesBlackJack n = removeIguais [(x, y) | x<-baralho, y<-baralho, x/=y, 
                         (n == (head (getValues x)) + head (getValues y)) ||
                         (n == (second (getValues x)) + head (getValues y)) ||
                         (n == (head (getValues x)) + second (getValues y)) ||
                         (n == (second (getValues x)) + second (getValues y))]
                        
--c
fullhouses :: [[String]]
fullhouses = [[a, b, c, d, e] | a <- baralho,
              b <- [x | x <- baralho, x /= a],
              c <- [x | x <- baralho, x /= a, x /= b],
              d <- [x | x <- baralho, x /= a, x /= b, x /= c],
              e <- [x | x <- baralho, x /= a, x /= b, x /= c, x /= d],
              (head a == head b) && (head a == head c) &&
              (second_full a) /= second_full b &&
              (second_full a) /= second_full c &&
              (second_full b) /= second_full c &&
              (head d == head e) && (second_full d) /= second_full e &&
              (second_full a) /= second_full d]


