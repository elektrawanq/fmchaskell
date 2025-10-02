{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}

module FMCNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) 
    , String            -- importou automaticamente para o Show
    , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise, 
    )
import Data.Text.Internal.Read (IParser(P))

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat -- SO
  S :: Nat -> Nat -- S(SO)


----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show :: Nat -> String
    show O     = "O"
    show (S n) = "S" ++ show n

instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O     = True
    S n == S m = n == m   -- (==) (S n) (S m) = n == m
    _ == _     = False    -- qualquer outro par de Nats que não entre nem no segundo caso 
    -- ou no primeiro (nem pela recursão do segundo) deve me retornar False
    -- infix 4 == Já é definido infix 4 na Prelude por padrão

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _     = True 
    S n <= O   = False
    S n <= S m = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min _ O         = O
    min O _         = O 
    -- preciso desse caso? Se eu colocar _ _ = O e colocar 
    -- apenas o outro casos dos sucessores, eu tenho redundância 
    -- e não vou retornar o que preciso
    min (S n) (S m) = S (min n m)

    max :: Nat -> Nat -> Nat -- raciocínio análogo a min n m
    max n O         = n
    max O n         = n
    max (S n) (S m) = S (max n m) 

----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven
nine  = S eight
ten   = S nine

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O     = True
isZero (S n) = False
 
-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O     = O
pred (S n) = n

even :: Nat -> Bool
even O     = True
even (S n) = odd n

odd :: Nat -> Bool
odd O     = False
odd (S n) = even n

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O     = n
n <+> (S m) = S (n <+> m)

infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (-*) 

(-*) :: Nat -> Nat -> Nat
n -* O       = n
O -* n       = O
(S n) -* (S m) = n -* m

infix 6 -*

-- multiplication
times :: Nat -> Nat -> Nat
n `times` O     = O
n `times` (S m) = (n `times` m) + n

infixl 7 `times` -- preciso disso?

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow = exp

exp :: Nat -> Nat -> Nat
exp = (<^>)

(<^>) :: Nat -> Nat -> Nat
n <^> O   = S O
n <^> S m = n <^> m * n

infixr 8 <^>

-- quotient --CORRIGIR ESSA AQUI
(</>) :: Nat -> Nat -> Nat
n </> O = error "A divisão por zero é indefinida"
n </> m = 
    case n <= m of
        True -> O
        False -> S((n -* m) </> m) 

infixl 7 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> O   = error "A divisão por zero é indefinida"
n <%> S m = n -* (n </> S m) * S m -- r = D - q*d
-- os parênteses não são necessários pois definimos infix de * maior que o de -*

infixl 7 <%>

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)  -- entradas: (D, d); saídas: (q, r)
eucdiv (_, O) = error "A divisão por zero é indefinida"
eucdiv (n, m) = (n </> m , n <%> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = isZero (n <%> m)

infix 6 <|>

divides = (<|>)

-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist O n = n
dist n O = n
dist (S n) (S m) = dist n m

(|-|) = dist

infix 6 |-|

factorial :: Nat -> Nat
factorial O     = S O
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O     = O
sg (S n) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat -- o primeiro argumento é a base e o segundo é o logaritmando
lo O _     = error "lo indefinido para base zero"
lo _ O     = error "lo indefinido para logaritmando igual a zero"
lo (S O) _ = error "lo indefinido para base one"
lo n m = 
    if n == min n m
        then S (lo n (m </> n)) 
        else O

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
--toNat 0 = O
--toNat n = S(toNat (n - 1))
toNat n
    | n <= 0 = O
    | otherwise = S (toNat (n - 1))

fromNat :: Integral a => Nat -> a
fromNat O     = 0
fromNat (S n) = 1 + fromNat n


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (-*)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = O
      | otherwise = S (fromInteger (x - 1))

