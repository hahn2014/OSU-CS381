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
-- | type   := number | bool | string | error                               | --
-- | cmd    := Push            push a type on the stack                     | --
-- |         | Add             add the top two numbers on the stack         | --
-- |         | Sub             subtract the top two numbers on the stack    | --
-- |         | Mul             multiply the top two numbers on the stack    | --
-- |         | Div             divide the top two numbers on the stack      | --
-- |         | Equ             check equality of top two items on stack     | --
-- |         | IfElse          conditional if else statement                | --
-- |         | Define          define a function macro                      | --
-- |         | Call            call a defined function macro                | --
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

data Type = Num  NumberType
          | Bool Bool
          | Str  StringType
          | Error StringType -- | Return error with a message
          deriving(Eq,Show)

data Cmd = Push Type  -- | Push all primative types to stack
         | Add -- | Stack work
         | Sub -- |      "
         | Mul -- |      "
         | Div -- |      "
         | Equ -- |      "
         | IfElse Prog Prog -- | Conditional Statement piping
         | Define StringType [Type] Prog  -- | Create a function
         | Call StringType [Type]           -- | Call function with arguments
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
                    --    (Num i : Num j : s') -> Just (Num (i + j) : s')
                        (Num i : Num j : s') -> case (i, j) of
                                        (Int i', Int j') -> Just Num (i' + j')
                                        (Double a, Double b) -> Just Num (a + b)
                                        (Float c, Float d) -> Just Num (c + d)
                        _ -> Nothing
cmd Sub          = \s -> case s of
                        (Num i : Num j : s') -> Just (Num (i - j) : s')
                                        ()

                        _ -> Nothing
cmd Mul          = \s -> case s of
                        (Num x : Num y : s') -> case of (x y) of
                                                (Int x : Int y) -> Just Num (x * y)

                        _ -> Nothing
cmd Div          = \s -> case s of
                        (Num x : Num y : s') -> Just (Num (x / y) : s')
                        _ -> Nothing
cmd Equ          = \s -> case s of
                        (Num x : Num y : s')   -> Just (Bool (x == y) : s')
                        (Bool i : Bool j : s') -> Just (Bool (i == j) : s')
                        (Str z : Str k : s')   -> Just (Bool (z == k) : s')
                        _  -> Nothing
cmd (IfElse t e) = (\s -> case s of
                        (Bool True : s')  -> prog t s'  -- if statement was true
                        (Bool False : s') -> prog e s'  -- if statement was false
                        _ -> Nothing)
-- Don't need to include Define and Call in the cmd function-no need to
-- evaluate/ add to domain
-- cmd (Define f t p) = \s -> case s of
--                         (Num n : s') -> prog p s'
--                         _ -> Nothing
-- cmd (Call f t)   = \s -> case s of
--                         ()



addNum :: NumberType -> NumberType -> NumberType
addNum l r = case (typeOf l, typeOf r) of
                (Right (Left x), Right (Left r))    ->
                (Right (Left Double)) ->
                (Right (Left Float))  ->
                _ ->

quadruple :: NumberType -> Cmd
quadruple (Num x) = Call double [Num x]

double :: Cmd
double = Define "double" [x] [Push NumberType x, Add]

negate :: Cmd
negate = Define "negate" [x] [Push (NumberType -1), Push NumberType x, Mul]

while :: Cmd
while = Define "while" [b] [ case b of
                                (Bool True)  -> while

                            ]

typeOf :: Type -> Either Type (Either NumberType StringType)
typeOf (Num x) = case x of
                (Int _)    -> Right (Left Int)
                (Double _) -> Right (Left Double)
                (Float _)  -> Right (Left Float)
typeOf (Bool _) = Left Bool
typeOf (Str s)  = case s of
                (String)  -> Right (Right String)
                (Char)    -> Right (Right Char)
typeOf _        = Left Error "Invalid Type"
-- --
evalInt :: NumberType -> Int
evalInt t = case t of -- | Sort by type
                (Int i) -> i -- | Only look for integers
                _ -> error "internal error: expected Int, got a decimal!" -- | Ignore floats and doubles


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
