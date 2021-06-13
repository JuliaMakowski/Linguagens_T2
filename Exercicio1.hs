module Exercicio1 where

{-
1) Defina uma função recursiva insere :: Int -> [Int] -> [Int] 
que insere um inteiro na posição correta em uma lista de inteiros já ordenada. 
Mostre o passo-a-passo da aplicação de insere 3 [1,2,4,5]
-}

insere :: Ord a => a -> [a] -> [a]
insere e ls = insertBy (compare) e ls

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys
