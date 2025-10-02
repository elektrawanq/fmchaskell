{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

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
import FMCNat
-- import ExNat (Nat(O))  -- importou automaticamente (???)

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

-}

head :: [a] -> a
head []       = undefined -- não possui elementos, logo não tem elemento head
head (x : xs) = x

tail :: [a] -> [a]
tail []       = undefined  --lista vazia, não há elemento para remover
tail (x : xs) = xs

null :: [a] -> Bool  -- fechei
null []       = True
null (x : xs) = False

length :: Integral i => [a] -> i
length []       = O
lenght (x : xs) = S (lenght xs)

sum :: Num a => [a] -> a
sum [] = O
sum head (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = S O --tenho que definir como 1, pois ele vai multiplicar ao fim da desconstrução de uma lista não vazia
product (x : xs) = x * product xs -- o head é mesmo necessário? Fica dando erro

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = Cons reverse xs x

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
snoc x [] = [x]
snoc x xs = xs ++ snoc x []

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
-- maximum :: Ord a => [a] -> a

-- take
-- drop

-- takeWhile
-- dropWhile

-- tails
-- init
-- inits

-- subsequences

-- any
-- all

-- and
-- or

-- concat

-- elem using the funciton 'any' above

-- elem': same as elem but elementary definition
-- (without using other functions except (==))

-- (!!)

-- filter
-- map

-- cycle
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

