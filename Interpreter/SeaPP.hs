--------------------------------------------------------------------------------
-- |                                   SEA++                                | --
-- | SEA++ is a Haskell built stack language. Please reference the          | --
-- |        README.md for more language details and how to compile!         | --
-- |                                                                        | --
-- | Authors: Brenden Smith (932-546-035), Bryce Hahn (932-819-555),        | --
-- |            Sheldon Roberts (933-021-566), April James (932-724-994)    | --
-- | Last Modified: 2/27/20                                                 | --
-- | Last Change  : n/a                                                     | --
--------------------------------------------------------------------------------
module SeaPP where
import Prelude hiding (Num)

--------------------------------------------------------------------------------
-- |                                Grammar                                 | --
-- |                                                                        | --
-- | number := Integer | Floating Point | Double                            | --
-- | string := Char* | Char                                                 | --
-- | type   := number | bool | string | error                               | --
-- | cmd    := Push            push a type on the stack                     | --
-- |         | Add             add the top two numbers on the stack         | --
-- |         | Sub             subtract the top two numbers on the stack    | --
-- |         | Mul             multiply the top two numbers on the stack    | --
-- |         | Div             divide the top two numbers on the stack      | --
-- |         | Equ             check equality of top two items on stack     | --
-- |         | IfElse          conditional if else statement                | --
-- | prog   := cmd*                                                         | --
-- | stack  := number | bool                                                | --
-- | domain := Stack -> Maybe Stack                                         | --
-- |                                                                        | --
--------------------------------------------------------------------------------
data NumberType = Int Int
                -- | Float Float
                -- | Double Double
                deriving(Eq,Show)

data StringType = String [Char]
                | Char Char
                deriving(Eq,Show,Read)

data Type = Num  NumberType
          | Bool Bool
          | Str  StringType
          | Error
          deriving(Eq,Show)

data Cmd = Push Type
         | Add
         | Sub
         | Mul
         | Div
         | Equ
         | IfElse Prog Prog
         deriving(Eq,Show)

type Prog = [Cmd]
type Stack = [Type]
type Domain = Stack -> Maybe Stack


-- define the command semantics in terms of a domain
cmd :: Cmd -> Domain
cmd (Push v)     = \s -> case v of
                    (Num i)  -> Just (Num i : s)
                    (Bool b) -> Just (Bool b : s)
                    (Str st) -> Just (Str st : s)
                    _ -> Nothing
cmd Add          = \s -> case s of
                    (Num i : Num j : s') -> Just (Num (evalInt i + evalInt j) : s')
                    _ -> Nothing
cmd Sub          = \s -> case s of
                    (Num i : Num j : s') -> Just (Num (evalInt i - evalInt j) : s')
                    _ -> Nothing
cmd Mul          = \s -> case s of
                    (Num x : Num y : s') -> Just (Num (evalInt x * evalInt y) : s')
                    _ -> Nothing
cmd Div          = \s -> case s of
                    (Num x : Num y : s') -> Just (Num (evalInt x / evalInt y) : s')
                    _ -> Nothing
cmd Equ          = \s -> case s of
                    (Num x : Num y : s')   -> Just (Bool (x == y) : s')
                    (Bool i : Bool j : s') -> Just (Bool (i == j) : s')
                    (Str z : Str k : s')   -> Just (Bool (z == k) : s')
                    _  -> Nothing
cmd (IfElse t e)   = \s -> case s of
                     (Bool True)  -> prog t s'  -- if statement was true
                     (Bool False) -> prog e s'  -- if statement was false
                    _ -> Nothing
--
-- --
 evalInt :: Value -> Int
 evalInt v = case v of
               (Num n) -> case n of
                   (Int i) -> i
                   _ -> error "internal error: expected Int, got a decimal!" --ignore floats and doubles
               _ -> error "internal error: expected Int, got something else!"

 -- | Helper function to evaluate an Expr to an Bool.
 evalBool :: Value -> Bool
 evalBool v = case v of
                (Bool b) -> b
                _ -> error "internal error: expected Bool, got something else!"


isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False

-- strEqu :: StringType -> StringType -> Bool -- use if "==" does not work for str comparison
-- strEqu

prog :: Prog -> Domain
prog []    = \s -> Just s
prog (c:p) = \s -> case cmd c s of
                     Just s' -> prog p s'
                     _ -> Nothing


run :: Prog -> Maybe Stack
run p = prog p []


-- exAdd1 :: Prog
-- exAdd1  = [Push (Num 3), Push (Num 2), Add]     --Expect 5
--
-- exMul1 :: Prog
-- exMul1 = [Push (Num 5), Push (Num 10), Mul]     --Expect 50
--
-- exDiv1 :: Prog
-- exDiv1 = [Push (Num 10), Push (Num 2), Div]     --Expect 5


-- exIf1 :: Prog
-- exIf1 = [Push (Bool True), IfElse (Push (Bool True)) (Push (Bool False))] --Expect True



-- this is the end of file :)
