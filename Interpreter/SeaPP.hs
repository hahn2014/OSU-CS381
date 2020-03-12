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
                deriving(Eq,Show)

data StringType = String [Char]
                | Char Char
                deriving(Eq,Show,Read)

-- | Error Exception handling (to give specifics with error reports)
data Exception = TypeError
               | TooFewArguments
               | TooManyArguments
               deriving(Eq,Show)

data Type = Num  NumberType
          | Bool Bool
          | Str  StringType
          | Var  Variable -- | Define a variable for a function macro
          | Error Exception StringType -- | Return error with a message
          deriving(Eq,Show)

data Operator = AddO | SubO | MulO | DivO | EquO

data Cmd = Push Type  -- | Push all primative types to stack
         | Add -- | Stack work
         | Sub -- |      "
         | Mul -- |      "
         | Div -- |      "
         | Equ -- |      "
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
cmd Add          = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i + j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a + b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c + d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e + f)) : s')
                        _ -> Just (Error TypeError (String "Addition failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Sub          = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i - j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a - b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c - d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e - f)) : s')
                        _ -> Just (Error TypeError (String "Subtraction failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Mul          = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i * j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a * b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c * d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e * f)) : s')
                        _ -> Just (Error TypeError (String "Multiplication failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Div          = \s -> case s of
                    --    (Num x : Num y : s') -> Just (Num (x / y) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a `div` b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c / d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e / f)) : s')
                        _ -> Just (Error TypeError (String "Division failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Equ          = \s -> case s of
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

-- numericalEvaluation :: NumberType -> NumberType -> Operator -> Stack
-- numericalEvaluation l r AddO = case (l, r) of
--                                 (Int a, Int b)       -> (Num (Int (a + b))) : []
--                                 (Double c, Double d) -> (Num (Double (c + d))) : []
--                                 (Float e, Float f)   -> (Num (Float (e + f))) : []
-- numericalEvaluation (Int a, Int b, AddO) =
-- numericalEvaluation (Int a, Int b, SubO) = (Num (Int (a - b))) : []
-- numericalEvaluation (Int a, Int b, MulO) = (Num (Int (a * b))) : []
-- numericalEvaluation (Int a, Int b, DivO) = (Num (Int (a `div` b))) : []
-- numericalEvaluation (Int a, Int b, EquO) =: (Bool (a == b)) : []





quadruple :: NumberType -> Cmd
quadruple (Int i)    = Call (String "double") [Num (Int i)]
quadruple (Double d) = Call (String "double") [Num (Double d)]
quadruple (Float f)  = Call (String "double") [Num (Float f)]

double :: Cmd
double = Define (String "double") [Char 'x'] [Push (Var (Char 'x')), Add]

negate :: Cmd
negate = Define (String "negate") [Char 'x'] [Push (Num (Int 0)), Push (Num (Int 1)), Sub, Push (Var (Char 'x')), Mul]

while :: Cmd
while = undefined
--while = Define (String "while") [Char 'b'] [Push (Var (Char 'b')),
    --            IfElse [Call (String "while" [Bool True])] [Call (String "while" [Bool False])]]


typeOf :: Type -> Either Type (Either NumberType StringType)
typeOf = undefined
-- typeOf (Num x) = case x of
--                 (Int _)    -> Right (Left Int)
--                 (Double _) -> Right (Left Double)
--                 (Float _)  -> Right (Left Float)
-- typeOf (Bool _) = Left Bool
-- typeOf (Str s)  = case s of
--                 (String _)  -> Right (Right String)
--                 (Char _)    -> Right (Right Char)
-- typeOf _        = Left (Error (String "Invalid Type"))
-- --


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



test1 :: Prog
test1 = [Push (Num (Int 5)), Push (Num (Double 2.5)), Add]



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
