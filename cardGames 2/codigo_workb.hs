digitToInt :: Char -> Int
digitToInt char = case char of
                  '2' -> 2
                  '3' -> 3
                  '4' -> 4
                  '5' -> 5
                  '6' -> 6
                  '7' -> 7
                  '8' -> 8
                  char -> 9

nums :: [Char]
nums = ['A','2','3','4','5','6','7','8','9','T','J','Q','K']

naipes :: [Char]
naipes = ['S','H','D','C']

--Auxiliares
--Usado na alinea a
baralhar :: [Char] -> [Char] -> [String]
baralhar xs xt = [ x : [y] | y <- xt , x <- xs]

--Usada na alinea b
getValues :: String -> [Int]
getValues st | head st `elem` ['J' , 'Q' , 'K' , 'T'] = [10,10]
             | head st `elem` ['A'] = [1 ,11]
             | otherwise = [digit,digit]
             where
                digit = digitToInt (head st)

--Usada na alinea b
second :: [Int] -> Int
second (h:t) = head t

--Usada na alinea c
second_full :: String -> Char
second_full (h:t) = head t

--a
baralho :: [String]
baralho = baralhar nums naipes

--b
combinacoesBlackJack :: Int -> [(String,String)]
combinacoesBlackJack 0 = []
combinacoesBlackJack n = [(x, y) | x<-baralho, y<-baralho, x/=y,
                          x>y,
                         (n == (head (getValues x)) + head (getValues y)) ||
                         (n == (second (getValues x)) + head (getValues y)) ||
                         (n == (head (getValues x)) + second (getValues y)) ||
                         (n == (second (getValues x)) + second (getValues y))]
 --c                       
fullhouses :: [[String]]
fullhouses = [[a, b, c, d, e] | 
              a <- baralho,
              b <- [x | x <- baralho, x > a, second_full a /= second_full x, head a == head x],
              c <- [x | x <- baralho, x > b, second_full a /= second_full x, second_full b /= second_full x, head a == head x],
              d <- [x | x <- baralho, x /= a, x /= b, x/=c, head a /= head x],
              e <- [x | x <- baralho, x /= a, x /= b, x /= c, x > d, second_full d /= second_full x, head d == head x]]