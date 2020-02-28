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
-- | cmd    := PushN           push a number on the stack                   | --
-- |         | PushB           push a boolean on the stack                  | --
-- |         | PushStr         push a string on the stack                   | --
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
                deriving(Eq,Show)

data Cmd      = PushN NumberType
              | PushB Bool
              | PushStr StringType
              | Add
              | Sub
              | Mul
              | Div
              | Equ
              | IfElse Prog Prog
              deriving(Eq,Show)

type Prog = [Cmd]
type Stack = [Either StringType (Either NumberType Bool)]
type Domain = Stack -> Maybe Stack



cmd :: Cmd -> Domain
cmd PushN n      = \s -> Just (NumberType n : s')
cmd PushB b      = \s -> Just (Bool b : s')
cmd PushStr str  = Just (StringType str : s')

cmd Add          = \s -> case s of
                    (NumberType i : NumberType j : s') -> Just (NumberType (i+j) : s')
                    _ -> Nothing
cmd Sub          = \s -> case s of
                    (NumberType i : NumberType j : s') -> Just (NumberType (i-j) : s')
                    _ -> Nothing
cmd Mul          = \s -> case s of
                    (NumberType x : NumberType y : s') -> Just (NumberType (x*y) : s')
                    _ -> Nothing
cmd Div          = \s -> case s of
                    (NumberType x : NumberType y : s') -> Just (NumberType (x/y) : s')
                    _ -> Nothing

cmd Equ          = \s -> case s of
                    (NumberType x : NumberType y : s')  -> Just (Bool (x == y) : s')
                    (Bool i : Bool j : s')              -> Just (Bool (i == j) : s')
                    (StringType z : StringType k : s')  -> Just (Bool (z == k) : s')
                    _  -> Nothing
cmd (IfElse t e) = \s -> case s of
                         (Bool True  : s') -> prog t s'
                         (Bool False : s') -> prog e s'
                         _ -> Nothing


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


exAdd1 :: Prog
exAdd1  = [PushN 3, PushNum 2, Add]   --Expect 5

exMul1 :: Prog
exMul1 = [PushN 5, PushN 10, Mul]     --Expect 50

exDiv1 :: Prog
exDiv1 = [PushN 10, PushN 2, Div]     --Expect 5

exIf1 :: Prog
exIf1 = [PushB True, IfElse PushB True PushB False] --Expect True



-- this is the end of file :)
