module Exercicio1 where

{-
1) Defina uma função recursiva insere :: Int -> [Int] -> [Int] 
que insere um inteiro na posição correta em uma lista de inteiros já ordenada. 
Mostre o passo-a-passo da aplicação de insere 3 [1,2,4,5]
-}

insere :: Ord a => a -> [a] -> [a]
insere x []     = [x]
insere x (y:ys) | x<=y  = x : y : ys
                | otherwise = y : insere x ys

-- insere 3 [1,2,4,5]
-- 1 : insere 3 [2,4,5]
-- 1 : (2 : insere 3 [4,5])
-- 1 : (2 : 3 : 4 : [5])
-- [1,2,3,4,5]

{-
2) Usando a função insere, defina a função 
ordenaInsere :: [Int] -> [Int] que ordena uma lista de inteiros
em ordem crescente usando o algoritmo de ordenação por inserção. 
Considere na sua função que uma lista vazia já está em ordem e que
para ordenar basta inserir um elemento na posição correta no restante da
lista que já deve estar ordenado.
-}
ordenaInsere :: Ord a => [a] -> [a]
ordenaInsere [] = []
ordenaInsere (x:xs) = insere x (ordenaInsere xs)

-- ordenaInsere [3,2,1,4]
-- insere 3 (ordenaInsere [2,1,4])
-- ...
-- insere 3 ( insere 2 (insere 1 (insere 4 [] )) )
-- insere 3 ( insere 2 (insere 1 [4]))
-- insere 3 ( insere 2 [1,4])
-- insere 3 [1,2,4]
-- [1,2,3,4]

{-
3) Defina uma função recursiva uneOrdenado :: [Int] -> [Int] -> [Int] 
que une duas listas já ordenadas em ordem crescente a uma terceira lista 
resultante que também deve estar em ordem crescente.
-}

uneOrdenado :: Ord a => [a] -> [a]-> [a]
uneOrdenado [] y = y
uneOrdenado (x:xs) (y:ys) | x <=y = x : (uneOrdenado xs (y:ys))
                          | y <=x = y : (uneOrdenado (x:xs) ys)

-- uneOrdenado [1,2] [1,3,4]
-- 1 : uneOrdenado [2] [1,3,4]
-- 1 : (1 :uneOrdenado [2][3,4])
-- 1 : (1:(2 : [3,4]))
-- [1,2,3,4]

{-
4) Usando a função uneOrdenado, defina uma função 
ordenaUne :: [Int] -> [Int] que particiona sucessivamente uma lista na 
metade até atingir partições de tamanho 1 para então ordenar as partições 
através da  função uneOrdenado até atingir uma lista ordenada. Considere uma 
lista vazia e a uma lista com um elemento como ordenadas na sua definição.
-}

ordenaUne :: [Int] -> [Int]
ordenaUne x  = x
ordenaUne (x:xs) = uneOrdenado (ordenaUne (take ((length xs)`div` 2) xs)) (ordenaUne (drop ((length xs) `div` 2) xs))


{-
[4,2,5,1]
[4,2][5,1]
[4][2][5][1] 

[2,4]
-}