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
               | ExistingDefinition
               | Unknown
               deriving(Eq,Show)

data Type = Num  NumberType
          | Bool Bool
          | Str  StringType
          | Var  Variable -- | Define a variable for a function macro
          | Error Exception StringType -- | Return error with a message
          deriving(Eq,Show)

data Cmd = Push Type -- | Push all primative types to stack
         | Addition -- | Stack work
         | Subtract -- |      "
         | Multiply -- |      "
         | Divide -- |        "
         | Equal -- |         "
         | IfElse Prog Prog -- | Conditional Statement piping
         | Define StringType [Variable] Prog -- | Create a function
         | Ref Variable -- | Variable reference
         | Call StringType [Type] -- | Call function with arguments
         deriving(Eq,Show)



type Prog = [Cmd]
type Variable = StringType
type Stack = [Type]
type Domain = Stack -> Maybe Stack
type Environment a = [(Variable,a)]

-- define the command semantics in terms of a domain
cmd :: Cmd -> Environment Type -> Domain
cmd (Push v)       e = \s -> case v of
                        (Num i)  -> Just (Num i : s)
                        (Bool b) -> Just (Bool b : s)
                        (Str st) -> Just (Str st : s)
                        _ -> Just (Error TypeError (String "Pushing failed -> not a valid stack type") : [])
cmd Addition       e = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i + j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a + b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c + d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e + f)) : s')
                        _ -> Just (Error TypeError (String "Addition failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Subtract       e = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i - j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a - b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c - d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e - f)) : s')
                        _ -> Just (Error TypeError (String "Subtraction failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Multiply       e = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i * j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a * b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c * d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e * f)) : s')
                        _ -> Just (Error TypeError (String "Multiplication failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Divide         e = \s -> case s of
                    --    (Num x : Num y : s') -> Just (Num (x / y) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a `div` b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c / d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e / f)) : s')
                        _ -> Just (Error TypeError (String "Division failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Equal          e = \s -> case s of
                    --    (Num x : Num y : s')   -> Just (Bool (x == y) : s')
                        (Num i : Num j : s') -> case (i, j) of
                                (Int a, Int b)       -> Just (Bool (a == b) : s')
                                (Double c, Double d) -> Just (Bool (c == d) : s')
                                (Float e, Float f)   -> Just (Bool (e == f) : s')
                        (Bool i : Bool j : s') -> Just (Bool (i == j) : s')
                        (Str z : Str k : s')   -> Just (Bool (z == k) : s')
                        _ -> Just (Error TypeError (String "Comparison failed -> one or more values on the stack were not a valid type") : [])
cmd (IfElse t o)   e = \s -> case s of
                        (Bool True : s')  -> prog t e s'  -- if statement was true
                        (Bool False : s') -> prog o e s'  -- otherwise statement was false
                        _ -> Just (Error TypeError (String "Conditional failed -> value on stack was not a Boolean Type") : [])

-- cmd (Define s v p) e = if (inLookup (lookup s e)) then -- | Search stack for existing function name
--                             e ++
--                             Just (Error ExistingDefinition (String "The variable name has already been assigned") : [])
--                         else Just (Str (String "Variable assigned"))
-- cmd (Ref v)        e = lookup v e

-- cmd (Call f t)   = \s -> case s of
--                         ()


inLookup :: Maybe a -> Bool
inLookup (Just a) = True
inLookup Nothing  = False


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


-- empty :: Environment a
-- empty e = []

-- get :: Variable -> Environment a -> a
-- get v e = case (lookup (v e)) of
--             (Just a) -> a

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



--
-- typeOf :: Type -> a
-- typeOf a = case a of
--             (Num n)     -> case n of
--                             (Int _)     -> Int
--                             (Double _)  -> Double
--                             (Float _)   -> Float
--             (Bool _)    -> Bool
--             (Str s)     -> case s of
--                             (String _) -> String
--                             (Char _)   -> Char
--             (Var _)     -> Var
--             (Error _ _) -> Error


-- | Boolean manipulation testing
lessThan :: NumberType -> NumberType -> Bool
lessThan l r = (l < r)

lessThanEqual :: NumberType -> NumberType -> Bool
lessThanEqual l r = (l <= r)

greaterThan :: NumberType -> NumberType -> Bool
greaterThan l r = (l > r)

greaterThanEqual :: NumberType -> NumberType -> Bool
greaterThanEqual l r = (l >= r)

toString :: Type -> String
toString t = case t of
            (Num (Int i))    -> show i ++ []
            (Num (Float f))  -> show f ++ []
            (Num (Double d)) -> show d ++ []
            (Str (String s)) -> s
            (Str (Char c))   -> show c ++ []
            (Bool True)      -> "True"
            (Bool False)     -> "False"
            (Var v)          -> case v of
                                    (String s) -> s
                                    (Char c)   -> show c ++ []
            (Error e m)      -> prettyError t

pop :: Stack -> String
pop [] = []
pop s = [\s] ++ pop s

reverse :: String -> String
reverse [] = pop s
reverse (c:cs) = (reverse cs) . (Push c)

concatenate :: [String] -> String
concatenate [] = []
concatenate (t:ts) = t ++ (concatenate ts)





evalInt :: Type -> Int
evalInt t = case t of -- | Sort by type
            (Num (Int i)) -> i
            _ -> error "Expected an Int, recieved other type!" -- | Ignore all other types

evalDouble :: Type -> Double
evalDouble t = case t of -- | Sort by type
            (Num (Double d)) -> d
            _ -> error "Expected Double, recieved other type!" -- | Ignore all other types

evalFloat :: Type -> Float
evalFloat t = case t of -- | Sort by type
            (Num (Float x)) -> x
            _ -> error "Expected Float, recieved other type!"  -- | Ignore all other types

evalBool :: Type -> Bool
evalBool t = case t of -- | Sort by type
            (Bool b) -> b
            _ -> error "Expected Bool, recieved other type!"   -- | Ignore all other types

evalString :: Type -> String
evalString t = case t of -- | Sort by type
              (Str (String s)) -> s
              _ -> error "Expected String, recieved other type!"   -- | Ignore all other types

evalChar :: Type -> Char
evalChar t = case t of -- | Sort by type
              (Str (Char c)) -> c
              _ -> error "Expected Character, received other type!" -- | Ignore all other types

evalVar :: Type -> Variable
evalVar t = case t of -- | Sort by type
            (Var v) -> v
            _ -> error "Expected Variable, recieved other type!"   -- | Ignore all other types

prettyError :: Type -> String
prettyError t = case t of -- | Sort by type
            (Error e (String m)) -> prettyException e ++ " Exception: " ++ m
            _ -> error "Expected Error, received other type!" -- | META

prettyException :: Exception -> String
prettyException e = case e of
                    TypeError          -> "Invalid Type" ++ []
                    TooFewArguments    -> "Too Few Arguments" ++ []
                    TooManyArguments   -> "Too Many Arguments" ++ []
                    ExistingDefinition -> "Existing Definition" ++ []
                    Unknown            -> "Unknown" ++ []

-- Make everything a bit nicer to look at
prettyPrint :: Stack -> Maybe Stack
prettyPrint [] = Nothing
prettyPrint (c:cs) = Just (c:cs) show (toString c) ++ prettyPrint cs



isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False

-- strEqu :: StringType -> StringType -> Bool -- use if "==" does not work for str comparison
-- strEqu

-- | Takes a Prog (list of Cmd),
prog :: Prog -> Environment Type -> Domain
prog []    _ = \s -> prettyPrint s
prog (c:p) e = \s -> case (cmd c e s) of
                     Just s' -> prog p e s'
                     _ -> Nothing


run :: Prog -> Maybe Stack
run p = prog p [] []


ex1 :: Prog
ex1 = negation (sub (quadruple 72) (Num 30))

-- good ex
testConcatenateG :: String
testConcatenate = (concatenate "123" "456")

-- bad ex
testConcatenateB :: String
testConcatenateB = (concatenate 123 456)

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
-- testAdd = add (Int 5) (Int 2)

-- testBigProg :: Prog -- | Expected 73
-- testBigProg = testAdd ++ [Push (Num (Int 10)), Mul, Push (Num (Int 3)), Add]-- , Push (Num (Int 4)), Div]

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
