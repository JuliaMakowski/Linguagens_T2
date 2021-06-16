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
