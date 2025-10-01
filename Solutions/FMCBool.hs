module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True  = "True"
    show False = "False"

instance Enum Bool where

--corrigir toEnum
    toEnum 0 = False
    toEnum 1 = True
    toEnum _ = error "O argumento não é um valor booleano"

    fromEnum True  = 1
    fromEnum False = 0
    -- fromEnum _ = error "O argumento não é booleano" -- esse caso dá erro: redundância

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _       = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _         = True

infixr 2 ||

-- NAND (Sheffer stroke) -- negação da AND
(/|\) :: Bool -> Bool -> Bool
x /|\ y = not (x && y)
--True /|\ True   = False
--_ /|\ _         = True

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool  -- negação do OR
x \|/ y = not (x || y)
--False \|/ False = True
--_ \|/ _         = False

infixr 2 \|/

-- XOR (exclusive disjunction) -- negação da bi-implicação
(<=/=>) :: Bool -> Bool -> Bool
x <=/=> y = not (x <=> y)
--True <=/=> True   = False
--False <=/=> False = False
--_ <=/=> _         = True

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True  = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True a _  = a
ifThenElse False _ b = b 

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _        = True

infixr 1 ==>

-- logical "implied by" -- mesma definição da implicação, só muda a direção da seta
(<==) :: Bool -> Bool -> Bool  -- é chamada equivalência lógica
x <== y = y ==> x
--False <== True = False
--_ <== _        = True

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
True <=> True   = True
False <=> False = True
_ <=> _         = False

infixr 1 <=>

