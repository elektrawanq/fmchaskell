{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# HLINT ignore "Use map" #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import Solutions.FMCNat -- lembrar de rodar o ghci na pasta fmchaskell

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)
ar nada além do que tá sendo importado (exceto teu próprio FMCNat, se quiser usar 
-}

head :: [a] -> a
head []       = undefined  -- não possui elementos, logo não tem elemento head
head (x : xs) = x

tail :: [a] -> [a] -- recebe uma lista e remove o primeiro elemento (ver init)
tail []       = undefined  --lista vazia, não há elemento para remover
tail (x : xs) = xs

null :: [a] -> Bool  -- fechei
null [] = True
null _  = False

length :: [a] -> Nat -- modifiquei o tipo
length []       = O
lenght (x : xs) = S (lenght xs)

sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1 --tenho que definir como 1, pois ele vai multiplicar ao fim da desconstrução de uma lista não vazia
product (x : xs) = x * product xs -- o head é mesmo necessário? Fica dando erro

reverse :: [a] -> [a] -- FAZER
reverse []       = []
reverse (x : xs) = undefined --Cons reverse xs x

(++) :: [a] -> [a] -> [a]
--[a] ++ [] = [a] não preciso desse caso
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)  -- não preciso do head?
-- no último caso,  a ideia é que já existe uma lista concatenada como eu queria
-- e eu vou só adicionar o último elemento na frente. Depois eu vou executar a operação recursivamente

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x []       = [x] -- x : []
snoc x (y : ys) = y : snoc x ys  --tira o elemento da frente da lista e 
--coloca o elemento do argumento atrás, até que só reste nil.

(<:) :: [a] -> a -> [a]
(<:) = flip snoc --flip troca a ordem de recebimento dos argumentos de uma função
-- assim o resultado pode ser diferente da função original, 
-- pois não temos a mesma intensão (no sentido de algoritmo)
-- <: [] x  = [x]   troca o x e o [] de lugarn nós que passamos os argumentos invertidos
-- <: (y : ys) x = y : snoc x ys

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum []       = error "Lista vazia não possui mínimo"
minimum [x]      = x
minimum (x : xs) =  min x (minimum xs)


maximum :: Ord a => [a] -> a
maximum []       =  error "Lista vazia não possui máximo"
maximum [x]      = x
maximum (x : xs) = max x (maximum xs)

take :: Nat -> [a] -> [a] -- operação que retira da lista a quantidade de elementos passada no argumento
take O _            = []
take (S n) []       = undefined -- quando o Nat é maior que o tamanho da lista
--take (S n) [] = [] outra forma que pode fazer mais sentido (?)
take (S n) (x : xs) = x : take n xs

drop :: Nat -> [a] -> [a]
drop O [a]          = [a]
--drop (S n) []     = error "O argumento da quantidade de elementos a remover é maior que o tamanho da lista"
drop (S n) []       = []   -- outra forma que tbm faz sentido
drop (S n) (x : xs) = drop n xs

-- curry f a b = f (a,b)
-- f::

takeWhile :: (a -> Bool) -> [a] -> [a] -- (condição -> bool) -> verifica a condição na lista 
--e para quando a cond = False. Retorna a lista até antes de ser False.
takeWhile _ [] = []
takeWhile c (x : xs) 
  | c x       = x : takeWhile c xs -- se a condição é verdade para o primeiro elemento
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile c (x : xs)
  | c x       = dropWhile c xs
  | otherwise = x : xs

tails :: [a] -> [[a]] -- recebe uma lista e retorna uma espécie de conjunto das partes, removendo o primeiro item da lista anterior
tails []       = [[]]  
tails (x : xs) = (x : xs) : tails xs

init :: [a] -> [a] -- recebe uma lista e devolve uma lista sem o último elemento
init []       = [] -- não temos último elemento
init [x]      = []
init (x : xs) = x : init xs

inits :: [a] -> [[a]] --TERMINAR
inits []  = [[]]
inits [x] = [[], [x]]
--inits (x : xs) = inits (init (x : xs)) : (x : xs) -- rascunho anotações

subsequences :: [a] -> [[a]] -- 
subsequences [] = [[]]
-- subsequences [x] = [[], [x]] -- forma da resposta: deve me mostrar das menores listas para as maiores
subsequences (x : xs) = subsequences xs ++ map (x :) (subsequences xs)
-- subsequences xs pega os subconjuntos do conjunto que não contém x
-- map (x :) (subsequences xs) chama a função map tomando como argumento a função que 
--adiciona o elemento x na frente de todas as subsequences xs 

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any c (x : xs)
  | c x       = True
  | otherwise = any c xs

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all c (x : xs)
  | c x       = all c xs
  | otherwise = False

and :: [Bool] -> Bool -- verifica se todos os itens da lista são True
and []       = True
and (x : xs) = x && and xs 

or :: [Bool] -> Bool
or []       = False
or (x : xs) = x || or xs


concat :: [[a]] -> [a] -- arrumar isso
concat [[]]   = []
--concat [[]] = []
concat (x : xs) = x ++ concat xs


-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

(!!) :: [a] -> Nat -> a
(!!) [] _           = error "A lista é vazia"
(!!) (x : xs) O     = x
(!!) (x : xs) (S n) = (!!) xs n

filter :: (a -> Bool) -> [a] -> [a] 
filter f [] = []
filter f (x : xs)
  | f x = x : filter f xs
  | otherwise = filter f xs

map :: (a -> b) -> [a] -> [b] -- retorna uma lista de f a
map f [] = []
map f (x : xs) =  f x : map f xs

cycle :: [a] -> [a]
cycle [] = []
cycle (x : xs) = cycle (x : cycle xs)
-- repeat
-- replicate

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

