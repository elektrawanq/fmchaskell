module FMCBabyNat where

-- Do not alter this import!
import Prelude ( Show(..) , Eq(..) , undefined, error )

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
tt = S O
ff = O

-- addition
(+) :: Nat -> Nat -> Nat
n + O   = n
n + S m = S (n + m)

 -- syntactic associativity: L
 -- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O     = tt
isZero (S n) = ff -- (S n) indica que a forma do Nat é sucessor

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O     = O
pred (S n) = n -- (S n) é usado para depois o n ser retomado como antecessor

-- Output: O means False, S O means True
even :: Nat -> Nat
even O     = tt
even (S n) = odd n -- por recursão, uma função chama a outra até que chegue a even/odd de zero

odd :: Nat -> Nat
odd O     = ff
odd (S n) = even n

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
O `monus` _ = O       -- outra forma de escrever a operação é usar backtips `operação`
n `monus` O = n
(S m) `monus` (S n) = m `monus` n -- e o caso em que n > m? Chegaria ao caso  0 - n por recursão

(-*) :: Nat -> Nat -> Nat
(-*) = monus
infix 6 -* 

-- multiplication
(*) :: Nat -> Nat -> Nat
n * O     = O
n * (S m) = n * m + n

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
n ^ O     = S O
n ^ (S m) = n ^ m * n

infixr 8 ^

(<=) :: Nat -> Nat -> Nat
O <= _     = tt
S _ <= O   = ff
S n <= S m = n <= m

infix <= -- preciso de precedência sintática?

-- quotient
(/) :: Nat -> Nat -> Nat
n / O = error "A divisão por zero é indefinida"
n / m = 
  case m <= n of 
    O -> O
    S O -> S ((n -* m) / m)

infixl 7 /

-- remainder
(%) :: Nat -> Nat -> Nat
n % O = error "A divisão por zero é indefinida"
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
absDiff n m  = (n -* m) + (m -* n)  -- corrigir


(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff 
  
infix 6 |-|

factorial :: Nat -> Nat
factorial O     = S O
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O     = O
sg (S n) = S O

-- lo b a is the floor of the logarithm base b of a 
lo :: Nat -> Nat -> Nat -- primeiro argumento é "a" e segundo é "b"
lo O _ = error "lo não definido para logaritmando igual a zero" -- log de n na base zero
lo _ O = error "lo não definido para base zero" -- log de zero na base n
lo (S O) n = error "lo não definido para base S O" -- log de n na base um, pois daria vários logs
lo n m = 
  case m <= n of
    O -> n / m 
    S O -> n / m -* S O
  --n / m - S O
-- toda vez que eu dividir pela base, quero somar 1