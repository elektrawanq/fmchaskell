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
import qualified Data.Type.Bool as TP

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
length (x : xs) = S (length xs)

sum :: Num a => [a] -> a
sum []       = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1 --tenho que definir como 1, pois ele vai multiplicar ao fim da desconstrução de uma lista não vazia
product (x : xs) = x * product xs -- o head é mesmo necessário? Fica dando erro

reverse :: [a] -> [a]
reverse []       = []
reverse (x : xs) = snoc x (reverse xs)

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
--inits [x] = [[], [x]] -- só pra guiar o que inits faz
inits (x:xs) = [] : map (x:) (inits xs)
--inits (x : xs) = inits (init (x : xs)) : (x : xs) -- rascunho e anotações: tentar outra definição

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
and (x : xs) = x (&&) and xs 

or :: [Bool] -> Bool -- verifica se ao menos um item da lista é True
or []       = False
or (x : xs) = x (||) or xs


concat :: [[a]] -> [a] 
concat []        = []
concat (xs : ys) = xs ++ concat ys -- ys é uma lista de listas xs, eu quero ir concatenando de par em par de listas xs


-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x = any (== x) -- em que == é a função ou condição e x é a lista 

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x (y:ys)
    | x == y    = True
    | otherwise = elem' x ys

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
map f []       = []
map f (x : xs) =  f x : map f xs

cycle :: [a] -> [a]
cycle [] = error "A lista é vazia"
cycle xs = xs ++ cycle xs -- corrigi xs' where xs' = xs ++ xs' ??

repeat :: a -> [a] -- recebe um elemento e retorna uma lista com o elemento repetido infinitamente.
repeat a = a : repeat a -- parecidíssima com a cycle, mas a diferença está no input: cycle recebe listas e não elementos!

replicate :: Nat -> a -> [a]
replicate O _     = []
replicate (S n) a = a : replicate n a

isPrefixOf :: Eq a => [a] -> [a] -> Bool -- verifica se a primeira lista aparece no início da segunda
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x : xs) (y : ys)
  | x == y    = isPrefixOf xs ys
  | otherwise = False

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] _              = True  -- nenhum elemento está contido em uma lista qualquer? True
isInfixOf (x : xs) []       = False
isInfixOf (x : xs) (y : ys) = x == y (||) isInfixOf (x : xs) ys
  -- | x == y = isInfixOf xs ys
  -- |otherwise = isInfixOf (x : xs) ys -- seria o mesmo que x == y isInfixOf (x:xs) || ys?

isSuffixOf :: Eq a => [a] -> [a] -> Bool --precisa da reverse? ver outra definição legal
isSuffixOf [] _              = True
isSuffixOf (x : xs) []       = False
isSuffixOf (x : xs) (y : ys) = reverse (x : xs) `isPrefixOf` reverse (y : ys)

zip :: [a] -> [b] -> [(a , b)] -- criação de tuplas, mas é bom lembrar que ela joga fora os elementos restantes de uma lista se a outra acaba (vale para os dois lados/argumentos 1 ou 2)
zip [] _              = []
zip _ []              = []
zip (x : xs) (y : ys) = (x,y) : zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c] -- aplica funções no par (a,b), para cada a e b com o mesmo índice nas listas e retorna a lista de f a b
zipWith f [] _              = []
zipWith f _ []              = []
zipWith f (x : xs) (y : ys) = f x y : zipWith f xs ys

intercalate :: [a] -> [[a]] -> [a]
intercalate _ []  = []
intercalate _ [x] = x
intercalate s (x : xs) = x ++ s ++ intercalate s xs -- em que s é o elemento (ou lista de caracteres) que vai separar o elementos da segunda lista

nub :: Eq a => [a] -> [a]
nub [] = []
nub (x : xs) 
    | x `elem'` xs = nub xs
    | otherwise    = x : nub xs
 
splitAt :: Nat -> [a] -> ([a], [a])
splitAt O xs = ([], xs)
splitAt _ [] = ([], [])
splitAt (S n) (x:xs) = (x:ys, zs)
  where
    (ys, zs) = splitAt n xs

-- what is the problem with the following?: 
-- splitAt n xs  =  (take n xs, drop n xs)
-- Resposta: se n for maior que o tamanho da lista xs, a segunda lista fica vazia e a primeira fica com todos os elementos.
-- Ou seja: o splitAt foi mal usado nesse caso.

-- quebra a lista em partes: primeira parte é a lista até antes do elemento que satisfaz a condição
-- a segunda lista é a lista a partir desse elemento (ver anotações, meio confuso)
break :: (a -> Bool) -> [a] -> ([a], [a]) 
break _ [] = ([], [])
break p (x:xs)
    | p x       = ([], x:xs)
    | otherwise = (x : parte1, parte2)
        where (parte1, parte2) = break p xs

lines :: String -> [String] 
lines "" = [""]
lines str =
    case break (== '\n') str of -- divide no primeiro '\n'
        (linha, resto) ->          -- linha = parte antes do '\n'
            if null resto
            then [linha]           -- retorna só a linha
            else linha : lines (tail resto)  --else adiciona linha e processa o resto

isSpace :: Char -> Bool  
isSpace c = c == ' ' (||) c == '\n'

words :: String -> [String]
words "" = []
words string =
    case dropWhile isSpace string of
        "" -> []
        s -> let (palavra, rest) = break isSpace s
             in palavra : words rest

unlines :: [String] -> String
unlines [] = ""
unlines (linha:linhas) = linha ++ "\n" ++ unlines linhas

unwords :: [String] -> String
unwords = intercalate " "

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose (x : xs)
    | all null (x : xs) = []
    | otherwise = map head (x : xs) : transpose (map tail (x : xs))

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

