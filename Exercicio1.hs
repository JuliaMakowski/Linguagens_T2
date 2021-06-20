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
-- funcao rece um numero inteiro e uma lista de inteiros ordenada, depois aloca o numero na sua posicao correta (em odem crescente)

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

-- funcao recebe uma lista de inteiros desordenada e com auxilio do metodo insere (feito anteriormente) ordena essa lista com um metodo recursivo

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
uneOrdenado [] ys         = ys
uneOrdenado xs []         = xs
uneOrdenado (x:xs) (y:ys) | x <=y = x : (uneOrdenado xs (y:ys))
                          | otherwise = y : (uneOrdenado (x:xs) ys)
-- funcao recebe duas listas de inteiros ordenadas e une as listas em ordem crescente

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
ordenaUne []  = []
ordenaUne [x] = [x]
ordenaUne xs = uneOrdenado (ordenaUne ys) (ordenaUne zs)
            where (ys,zs)     = splitAt ((length xs)`div` 2) xs


-- funcao recebe uma lista de inteiros desordenados, separa as listas e posteriormente vai unindo elas de maneira ordenada
{-
--ordenaUne [3,2,1]
--uneOrdenado (ordenaUne [3]) (ordenaUne[2,1])
--uneOrdenado (uneOrdenado [] [3]) (uneOrdenado ( ordenaUne [2]) (ordenaUne [1]))
--uneOrdenado ( [3] ) (uneOrdenado ([][2])([][1]))
--uneOrdenado ([3])([1,2])
-- [1,2,3]
-}

{-
5) Explique a função padrão zipWith cuja definição é a seguinte:
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (x:xs) (y:ys)=f x y : zipWith f xs ys
zipWith f _      _      = []

ZipWith recebe uma função e duas listas como parâmetros e então junta as duas listas aplicando a função entre os elementos correspondentes.
O primeiro parâmetro é uma função que recebe duas coisas e produz uma terceira. O segundo e o terceiro parâmetro são listas. O resultado é também uma lista.
O primeiro tem que ser uma lista de as, porque a função de junção recebe as como primeiros argumentos.
A segunda tem que ser uma lista de bs, porque o segundo argumento da função é do tipo b. O resultado é uma lista de cs.
Se a declaração do tipo de uma função diz que ela aceita uma função a -> b -> c como parâmetro, ela também aceitará uma função a -> a -> a, mas não o contrário!
O corpo da função no último pattern é também parecido com zip normal, mas ele não faz (x,y), e sim f x y.
Uma única função de alta ordem pode ser usada para uma enorme variedade de tarefas se é suficientemente geral.
-}


{-
7) Dê uma definição para a função

disjuntas :: (Ord a) => [a] -> [a] -> Bool

que recebe duas listas em ordem crescente e determina se as mesmas não possuem nenhum elemento em comum, isto é, se são disjuntas.
-}
disjuntas :: (Ord a) => [a] -> [a] -> Bool
disjuntas [] _ = True
disjuntas (x:xs) ys
  | elem x ys = False
  | otherwise = disjuntas xs ys

-- pega um elemento pro vez da lista 1 e compara com a segunda lista 
-- caso o elemento colida a verificacao = true, return = false
-- caso nao tenha elemento colidindo a verificacao = false, e o codigo continua percorrendo a linha até o fim

-- então fica assim:
-- [3,4,6] [6,7,8]
-- elem 3 [6,7,8] -- false entra no otherwise 
-- disjuntas [4,6] [6,7,8]
-- elem 4 [6,7,8] -- false entra no otherwise 
-- disjuntas [6] [6,7,8]
-- elem 6 [6,7,8] -- true return false, nao é disjunta 