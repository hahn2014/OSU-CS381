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

-- define the command semantics in terms of a domain
cmd :: Cmd -> Domain
cmd (Push v)        = \s -> case v of
                        (Num i)  -> Just (Num i : s)
                        (Bool b) -> Just (Bool b : s)
                        (Str st) -> Just (Str st : s)
                        _ -> Just (Error TypeError (String "Pushing failed -> not a valid stack type") : [])
cmd Addition        = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i + j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a + b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c + d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e + f)) : s')
                        _ -> Just (Error TypeError (String "Addition failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Subtract        = \s -> case s of
                    --    (Num i : Num j : s') -> Just (Num (i - j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                            (Int a, Int b)       -> Just (Num (Int (a - b)) : s')
                            (Double c, Double d) -> Just (Num (Double (c - d)) : s')
                            (Float e, Float f)   -> Just (Num (Float (e - f)) : s')
                        _ -> Just (Error TypeError (String "Subtraction failed -> one or more values on the stack were not of the Numerical Type") : [])
cmd Multiply        = \s -> case s of
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
cmd Equal           = \s -> case s of
                    --    (Num x : Num y : s')   -> Just (Bool (x == y) : s')
                        (Num i : Num j : s') -> case (i, j) of
                                (Int a, Int b)       -> Just (Bool (a == b) : s')
                                (Double c, Double d) -> Just (Bool (c == d) : s')
                                (Float e, Float f)   -> Just (Bool (e == f) : s')
                        (Bool i : Bool j : s') -> Just (Bool (i == j) : s')
                        (Str z : Str k : s')   -> Just (Bool (z == k) : s')
                        _ -> Just (Error TypeError (String "Comparison failed -> one or more values on the stack were not a valid type") : [])
cmd (IfElse t o)    = \s -> case s of
                        (Bool True : s')  -> prog t s'  -- if statement was true
                        (Bool False : s') -> prog o s'  -- otherwise statement was false
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

ifelse :: Type -> Prog -> Prog -> Prog
ifelse (Bool b) t e = [ Push (Bool b), IfElse t e]
ifelse _ _ _ = [ Push (Error TypeError (String "Conditional argument was not a Boolean value"))]



quadruple :: NumberType -> Prog
quadruple x = double x ++ [Push (Num x), Addition]

double :: NumberType -> Prog
double x = add (Num x) (Num x)

negation :: NumberType -> Prog
negation x = mul (Num (Int (-1))) (Num x)

while :: Stack -> Prog
while = \st -> ifelse (Bool st)
                    (while st)          -- | Then
                    [Push (Bool False)] -- | Else



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
pop [] = ""
pop (s:ss) = toString s

flipString :: String -> String
flipString [] = ""
flipString (c:cs) = flipString cs ++ toString (Str (Char c))

strcat :: [String] -> String
strcat [] = []
strcat (t:ts) = t ++ (strcat ts)





evalInt :: Type -> Int
evalInt t = case t of -- | Sort by type
            (Num (Int i)) -> i
            _ -> error "Expected an Int, recieved other type!" -- | Ignore all other types

evalIntP :: Prog -> Int
evalIntP p = case (run p) of
                    (Just [(Num (Int i))]) -> i

evalDouble :: Type -> Double
evalDouble t = case t of -- | Sort by type
            (Num (Double d)) -> d
            _ -> error "Expected Double, recieved other type!" -- | Ignore all other types

evalDoubleP :: Prog -> Double
evalDoubleP p = case (run p) of
                    (Just [(Num (Double d))]) -> d

evalFloat :: Type -> Float
evalFloat t = case t of -- | Sort by type
            (Num (Float x)) -> x
            _ -> error "Expected Float, recieved other type!"  -- | Ignore all other types

evalFloatP :: Prog -> Float
evalFloatP p = case (run p) of
                    (Just [(Num (Float f))]) -> f

evalBool :: Type -> Bool
evalBool t = case t of -- | Sort by type
            (Bool b) -> b
            _ -> error "Expected Bool, recieved other type!"   -- | Ignore all other types

evalBoolP :: Prog -> Bool
evalBoolP p = case (run p) of
                    (Just [(Bool b)]) -> b

evalString :: Type -> String
evalString t = case t of -- | Sort by type
              (Str (String s)) -> s
              _ -> error "Expected String, recieved other type!"   -- | Ignore all other types

evalStringP :: Prog -> String
evalStringP p = case (run p) of
                  (Just [(Str (String s))]) -> s

evalChar :: Type -> Char
evalChar t = case t of -- | Sort by type
              (Str (Char c)) -> c
              _ -> error "Expected Character, received other type!" -- | Ignore all other types

evalCharP :: Prog -> Char
evalCharP p = case (run p) of
                  (Just [(Str (Char c))]) -> c

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
prettyPrint (c:cs) = Just (c:cs) --show (toString c) ++ prettyPrint cs



isEmpty :: Stack -> Bool
isEmpty [] = True
isEmpty _  = False


-- | Takes a Prog (list of Cmd),
prog :: Prog -> Domain
prog []    = \s -> Just s
prog (c:p) = \s -> case cmd c s of
                     Just s' -> prog p s'
                     _ -> Nothing

-- prog :: Prog -> Environment Type -> Type
-- prog []    _ = \s -> Error Unknown (String "End of program error")
-- prog (c:p) e = \s -> case (cmd c e s) of
--
--                     Just (Num n) : s'     -> Num n
--                     Just (Str s) : s'     -> Str s
--                     Just (Bool b) : s'    -> Bool b
--                     Just (Var v) : s'     -> Var v
--                     Just (Error e m) : s' -> Error e m
--                     _                -> Error Unknown (String "Unknown command return.")

run :: Prog -> Maybe Stack
run p = prog p []


-- | Hello there.
    -- | General Kenobi






-- | Type Semantics

-- type := Num  NumberType
--       | Bool Bool
--       | Str  StringType
--       | Var  Variable
--       | Error Exception StringType




-- | GOOD EXAMPLES
goodAdd :: Prog -- | Expected 7
goodAdd = add (Num (Int 5)) (Num (Int 2))

goodSub :: Prog -- | Expected 7
goodSub = sub (Num (Int 20)) (Num (Int 7))

goodMul :: Prog -- | Expected 7
goodMul = mul (Num (Float 20.4)) (Num (Float 6.2))

goodDivi :: Prog
goodDivi = divi (Num (Double 2.0)) (Num (Double 6.1))

goodEqu :: Prog
goodEqu = equ (Str (String "Hello")) (Str (String "World"))

goodIf :: Prog
goodIf = ifelse (Bool (lessThanEqual (Int 5) (Int 10))) goodDivi goodSub

-- testIsEmptyG :: Bool            -- should return True
-- testIsEmptyG = (isEmpty (lessThanEqual (Int 50) (Int 100)))

testReverseG :: String
testReverseG = (strcat ["it's a small world", " after all"])


-- | BAD EXAMPLES
badAdd :: Prog -- | Expect Error, Wrong type
badAdd = add (Num (Int 29)) (Num (Float 7.244))

badSub :: Prog -- | Error mismatched types
badSub = sub (Num (Int 5)) (Num(Double 2.0))

badIf :: Prog -- | Error bad syntax; No bool
badIf = ifelse (Num (Int 5)) (add (Num (Int 5)) (Num (Int 5))) (add (Num (Int 10)) (Num (Int 29)))




-- testIsEmptyB :: Bool
-- testIsEmptyB = (isEmpty (Str (String "string")))

-- testReverseB :: String
-- testReverseB = (strcat (Bool True) (123))













-- this is the end of file :)
