module ExamOne where

type State = Int
type Val = State -> State

type I = Int
data S = Increment I
       | Reset

type P = [S]

valS :: S -> Val
valS (Increment i) = \s -> (s + i)
valS Reset         = \s -> 0

-- [Inc 5, Reset, Inc 2]
-- (Inc 5):Reset:(Inc 2):[]
--
-- p -> 5
--
-- s = Inc 5
-- p -> 0
--
-- s = Reset
-- p -> 2
--
-- s = Inc 2
-- p -> 0
--
-- 0
valP :: [S] -> Int
valP [] = 0
valP (s:ss) = valS s (valP ss)



-- Program 2
data Cmd   = Gas | Brake | Turn
type Prog  = [Cmd]

type Pos   = Int
type Speed = Int
data Dir   = Forward | Backward
