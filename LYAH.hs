module LYAH where

-- triangle :: [(Integer,Integer,Integer)]
triangle = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24 ]

removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z'] ]

factorial :: Integer -> Integer
-- factorial n = product [1..n]
factorial 0 = 1
factorial n = n * factorial (n - 1)

circumference :: Double -> Double 
circumference r = 2 * pi * r

-- produto escalar de vetores do R^n
dot_product :: (Num a) => [a] -> [a] -> a
dot_product u v =  if null u == True || null v == True then 0 else
			(if length u /= length v then error "numero de componentes incompativel"
		   	else head u * head v + dot_product (tail u) (tail v) )
-- teria como generalizar limitando o tamanho da lista ou fazendo tuplas de tamanho n?

--------------------------------------------------------------------------------------------------------------
----------------------------  LISTA  ------------------------  LISTA  ----------------------------------------

head' :: [a] -> a
head' [] = error "Lista vazia, sem cabeça."
head' (x:_) = x -- x:_ seria uma LISTA com o primeiro elemento x seguido de QUALQUER COISA
-- ex: 1:2:3:[] == [1,2,3] (syntatic sugar)

--length' :: [a] -> Int OU
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-------------------------------------------------------------------------------------------------------------
----------------------------  LISTA  ---------FIM------------  LISTA  ----------------------------------------

tell :: (Show a) => [a] -> String
tell [] = "Lista vazia."
tell (x:[]) = "Lista de tamanho '1' cujo primeiro e unico elemento eh '" ++ show x ++ "'."
tell (x:y:[]) = "Lista de tamanho '2' cujos dois primeiros elemento sao '" ++ show x ++ "' e '" ++ show y ++ "'."
tell (x:y:_) = "Lista de tamanho '3' ou maior."

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:y:z) = "The first part of " ++ all ++ " is " ++ [z]
