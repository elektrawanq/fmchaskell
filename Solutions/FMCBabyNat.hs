module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
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

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = S O
isZero (S n) = O --FECHEI
-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n --FECHEI

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even (S n) = odd n --FECHEI

odd :: Nat -> Nat
odd O = O
odd (S n) = even n --FECHEI

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
O `monus` n = O       -- outra forma de escrever a operação é us
n `monus` O = n
(S m) `monus` (S n) = m `monus` n -- e o caso em que n > m? Chegaria ao caso  0 - n por recursão
--FECHEI
(-*) :: Nat -> Nat -> Nat
(-*) = monus
infix 6 -* -- verificar se é necessário

-- multiplication
(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = n * m + n
--FECHEI
infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
n ^ O = S O
n ^ (S m) = n ^ m * n

infixr 8 ^

(<=) :: Nat -> Nat -> Nat
O <= _ = S O
S _ <= O = O
(S n) <= (S m) = n <= m

-- quotient
(/) :: Nat -> Nat -> Nat
n / O = undefined
n / m = 
  case m <= n of 
    O -> O
    S O -> S ((n -* m) / m)

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
n % O = undefined
n % m = n -* (m * (n / m))
infixl 7 %

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
n ||| m =  isZero (m % n) -- b%a == O
infixl 7 |||  --precedência sintática!! verificar

(.|.) :: Nat -> Nat -> Nat
n .|. m = n ||| m
infixl 7 .|. --preciso definir o infix?
-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff n m  = 
  case m <= n of
    O -> m - n  -- |n - m| = n - m se m <= n
    S O -> n - m -- |n - m| = m - n, se n <= m

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff   

factorial :: Nat -> Nat
factorial = undefined

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined