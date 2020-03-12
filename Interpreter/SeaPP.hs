--------------------------------------------------------------------------------
-- |                                   SEA++                                | --
-- | SEA++ is a Haskell built stack language. Please reference the          | --
-- |        README.md for more language details and how to compile!         | --
-- |                                                                        | --
-- | Authors: Brenden Smith (932-546-035), Bryce Hahn (932-819-555),        | --
-- |            Sheldon Roberts (933-021-566), April James (932-724-994)    | --
-- | Last Modified: 3/11/20                                                 | --
--------------------------------------------------------------------------------
module SeaPP where
import Prelude hiding (Num)

--------------------------------------------------------------------------------
-- |                                Grammar                                 | --
-- |                                                                        | --
-- | number := Integer | Floating Point | Double                            | --
-- | string := Char* | Char                                                 | --
-- | type   := number | bool | string | variable | error                    | --
-- | cmd    := Push            push a type on the stack                     | --
-- |         | Add             add the top two numbers on the stack         | --
-- |         | Sub             subtract the top two numbers on the stack    | --
-- |         | Mul             multiply the top two numbers on the stack    | --
-- |         | Div             divide the top two numbers on the stack      | --
-- |         | Equ             check equality of top two items on stack     | --
-- |         | IfElse          conditional if else statement                | --
-- |         | Define          define a function macro                      | --
-- |         | Call            call a defined function macro                | --
-- | exception := typeER | toofewER | toomanyER                             | --
-- | prog   := cmd*                                                         | --
-- | stack  := number | bool                                                | --
-- | domain := Stack -> Maybe Stack                                         | --
-- |                                                                        | --
--------------------------------------------------------------------------------
data NumberType = Int Int
                | Float Float
                | Double Double
                deriving(Eq,Show,Ord)

data StringType = String [Char]
                | Char Char
                deriving(Eq,Show,Read)

-- | Error Exception handling (to give specifics with error reports)
data Exception = TypeError
               | TooFewArguments
               | TooManyArguments
               | Unknown
               deriving(Eq,Show)

data Type = Num  NumberType
          | Bool Bool
          | Str  StringType
          | Var  Variable -- | Define a variable for a function macro
          | Error Exception StringType -- | Return error with a message
          deriving(Eq,Show)

data Cmd = Push Type  -- | Push all primative types to stack
         | Addition -- | Stack work
         | Subtract -- |      "
         | Multiply -- |      "
         | Divide -- |        "
         | Equal -- |         "
         | IfElse Prog Prog -- | Conditional Statement piping
         | Define StringType [Variable] Prog  -- | Create a function
         | Call StringType [Type]             -- | Call function with arguments
         deriving(Eq,Show)



type Prog = [Cmd]
type Variable = StringType
type Stack = [Type]
type Domain = Stack -> Maybe Stack


-- define the command semantics in terms of a domain
cmd :: Cmd -> Domain
cmd (Push v)     = \s -> case v of
                        (Num i)  -> Just (Num i : s)
                        (Bool b) -> Just (Bool b : s)
                        (Str st) -> Just (Str st : s)
                        _ -> Just (Error TypeError (String "Pushing failed -> not a valid stack type") : [])
cmd Addition          = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i + j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a + b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c + d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e + f)) : s')
                        _ -> Just (Error TypeError (String "Addition failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Subtract          = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i - j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a - b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c - d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e - f)) : s')
                        _ -> Just (Error TypeError (String "Subtraction failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Multiply          = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i * j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a * b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c * d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e * f)) : s')
                        _ -> Just (Error TypeError (String "Multiplication failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Divide          = \s -> case s of
                    --    (Num x : Num y : s') -> Just (Num (x / y) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a `div` b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c / d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e / f)) : s')
                        _ -> Just (Error TypeError (String "Division failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Equal          = \s -> case s of
                    --    (Num x : Num y : s')   -> Just (Bool (x == y) : s')
                        (Num i : Num j : s') -> case (i, j) of
                                (Int a, Int b)       -> Just (Bool (a == b) : s')
                                (Double c, Double d) -> Just (Bool (c == d) : s')
                                (Float e, Float f)   -> Just (Bool (e == f) : s')
                        (Bool i : Bool j : s') -> Just (Bool (i == j) : s')
                        (Str z : Str k : s')   -> Just (Bool (z == k) : s')
                        _ -> Just (Error TypeError (String "Comparison failed -> one or more values on the stack were not a valid type") : [])
cmd (IfElse t e) = \s -> case s of
                        (Bool True : s')  -> prog t s'  -- if statement was true
                        (Bool False : s') -> prog e s'  -- if statement was false
                        _ -> Just (Error TypeError (String "Conditional failed -> value on stack was not a Boolean Type") : [])
-- Don't need to include Define and Call in the cmd function-no need to
-- evaluate/ add to domain
-- cmd (Define f t p) = \s -> case s of
--                         (Num n : s') -> prog p s'
--                         _ -> Nothing
-- cmd (Call f t)   = \s -> case s of
--                         ()


-- These bad boys below are easier ways to implement the core commands
-- This will allow all arithmetic to be performed with the formula∷
--     <operation-type> <value> <value>
add :: Type -> Type -> Prog
add x y = [ Push x, Push y, Addition ]

sub :: Type -> Type -> Prog
sub x y = [ Push x, Push y, Subtract ]

mul :: Type -> Type -> Prog
mul x y = [ Push x, Push y, Multiply ]

divi :: Type -> Type -> Prog
divi x y = [ Push x, Push y, Divide ]

equ :: Type -> Type -> Prog
equ x y = [ Push x, Push y, Equal ]




quadruple :: NumberType -> Prog
quadruple x = double x ++ [Push (Num x), Addition]

double :: NumberType -> Prog
double x = add (Num x) (Num x)

negation :: NumberType -> Prog
negation x = mul (Num (Int (-1))) (Num x)

while :: Cmd
while = undefined
--while = Define (String "while") [Char 'b'] [Push (Var (Char 'b')),
    --            IfElse [Call (String "while" [Bool True])] [Call (String "while" [Bool False])]]




-- typeOf :: Type -> Either Type (Either NumberType StringType)
-- typeOf (Num x)  = case x of
--                     (Int _)    -> Right (Left Int)
--                     (Double _) -> Right (Left Double)
--                     (Float _)  -> Right (Left Float)
-- typeOf (Bool _) = Left Bool
-- typeOf (Str s)  = case s of
--                     (String _)  -> Right (Right String)
--                     (Char _)    -> Right (Right Char)
-- typeOf (Var _)  = Left Var
-- typeOf _        = Left (Error (String "Invalid Type"))
--



lessThan :: NumberType -> NumberType -> Bool
lessThan l r = (l < r)

lessThanEqual :: NumberType -> NumberType -> Bool
lessThanEqual l r = (l <= r)

greaterThan :: NumberType -> NumberType -> Bool
greaterThan l r = (l > r)

greaterThanEqual :: NumberType -> NumberType -> Bool
greaterThanEqual l r = (l >= r)

evalInt :: NumberType -> Int
evalInt t = case t of -- | Sort by type
                (Int i) -> i -- | Only look for integers
                _ -> error "Expected an Int, got a decimal value!" -- | Ignore floats and doubles


 -- | Helper function to evaluate an Expr to an Bool.
-- evalBool :: Type -> Bool
-- evalBool t = case t of -- | Sort by type
--                 (Bool b) -> b -- | single out a boolean value
--                 _ -> error "internal error: expected Bool, got something else!"


isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False

-- strEqu :: StringType -> StringType -> Bool -- use if "==" does not work for str comparison
-- strEqu

-- | Takes a Prog (list of Cmd),
prog :: Prog -> Domain
prog []    = \s -> Just s
prog (c:p) = \s -> case cmd c s of
                     Just s' -> prog p s'
                     _ -> Nothing


run :: Prog -> Maybe Stack
run p = prog p []

-- reverse :: StringType -> StringType
-- reverse s = case s of
--                 (String (c:cs)) -> (String )
--                 (Char c) -> Char c
-- reverse [] = []    --string is fully pushed
-- reverse (c:cs) = \s -> case Char c s of
--                        _ -> reverse cs

-- | Hello there.
    -- | General Kenobi


-- testAdd :: Prog -- | Expected 7
-- testAdd = [Push (Num (Int 5)), Push (Num (Int 2)), Add]
--
-- testBigProg :: Prog -- | Expected 73
-- testBigProg = testAdd ++ [Push (Num (Int 10)), Mul, Push (Num (Int 3)), Add]-- , Push (Num (Int 4)), Div]
--
-- testBool :: Prog
-- testBool = [Push (Bool (lessThan (Int 20) (Int 15))), IfElse testAdd testBigProg]

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
