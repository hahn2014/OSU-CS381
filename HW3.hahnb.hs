-- Teams: Bryce Hahn hahnb@oregonstate.edu
   --     Brenden Smith smitbre2@oregonstate.edu
   --     Sheldon Roberts robeshel@oregonstate.edu
module MiniLogo where
import Data.List
import Data.Stack
import Data.Typeable

-- num	::=	(any natural number)             --Integers
-- var	::=	(any variable name)              --String
-- macro	::=	(any macro name)             --String
type Macro = String
type Var = String

--prog	::=	ε   |   cmd ; prog
type Prog = [Cmd]                   --sequence of commands

--mode	::=	down   |   up
data Mode = Down | Up                --pen status
    deriving (Show, Eq)

-- expr	::=	var
-- |	num
-- |	expr + expr
data Expr = Variable Var            --variable reference
          | Number Int              --literal number
          | Add Expr Expr           --addition expression
    deriving (Show, Eq)

-- cmd	::=	pen mode
-- |	move ( expr , expr )
-- |	define macro ( var* ) { prog }
-- |	call macro ( expr* )
data Cmd = Pen Mode                 --change pen mode
         | Move Expr Expr           --move pen to a new position
         | Def Macro [Var] Prog     --define a macro
         | Call Macro [Expr]        --invoke a macro
    deriving (Show, Eq)

--
-- define line(x1, y1, x2, y2) {
--      pen up;
--      move x1, y1;
--      pen down;
--      move x2, y2;
--      pen up;
-- };
line :: Cmd
line = Def "line" ["x1", "y1", "x2", "y2"]
    [Pen Up, Move (Variable "x1") (Variable "y1"), Pen Down, Move (Variable "x2") (Variable "y2"), Pen Up]

-- define nix(x, y, w, h) {
--      Call line [ x, y, (x + w), (y + h) ];
--      Call line [ (x + w), y, (x - w), (y + h) ];
-- };
nix :: Cmd
nix = Def "nix" ["x", "y", "w", "h"]
    [ Call "line" [(Variable "x"), (Variable "y"), (Variable "x+w"), (Variable "y+h")],
      Call "line" [(Variable "x+w"), (Variable "y"), (Variable "x-w"), (Variable "y+h")]  ]

-- define steps(n) {
--      Call line [0, 0, 0, 1]; (0, 0) -> (0, 1)
--      Call line [0, 1, 1, 1]; (0, 1) -> (1, 1)
--      steps_helper (n - 1) [1, 1]
-- }
steps :: Int -> Prog
steps n = [Call "line" [Number 0, Number 0, Number 0, Number 1],
           Call "line" [Number 0, Number 1, Number 1, Number 1] ] ++ steps_helper (n - 1) [1, 1]

-- define steps_helper(n, [x, y]) {
--      Call line [x, y, x, (y + 1)];
--      Call line [x, (y + 1), (x + 1), (y + 1)];
--      steps_helper (n - 1) [(x + 1), (y + 1)];
-- }
steps_helper :: Int -> [Int] -> Prog
steps_helper 0 _      = []
steps_helper n [x, y] = [Call "line" [Variable "x", Variable "y", Variable "x", Add (Variable "y") (Number 1)],
    Call "line" [Variable "x", Add (Variable "y") (Number 1), Add (Variable "x") (Number 1), Add (Variable "y") (Number 1)] ] ++ steps_helper (n - 1) [x + 1, y + 1]


-- define macros(Prog, [Macros]) {
--
-- };
macros :: Prog -> [Macro]
macros []     = []
macros (x:xs) = case x of
    Def m _ _ -> m : macros xs
    otherwise -> macros xs


pretty :: Prog -> String
pretty []     = ""
pretty (x:xs) = case x of
    Pen m               -> show ("Pen " ++ prettyPen m) ++ "\n" ++ pretty xs
    Move e1 e2          -> show ("Move ") ++ prettyExpression [e1, e2] ++ "\n" ++ pretty xs
    Def m v p           -> show ("Def " ++ m ++ " ") ++ prettyVars v ++ pretty p ++ "\n" ++ pretty xs
    Call m e            -> show ("Call " ++ m ++ " ") ++ prettyExpression e ++ "\n" ++ pretty xs


prettyPen :: Mode -> String
prettyPen Up   = "Up"
prettyPen Down = "Down"


prettyExpression :: [Expr] -> String
prettyExpression []     = ""
prettyExpression (e:es) = case e of
    Number i -> show i ++ ", " ++ prettyExpression es
    Variable v -> v ++ ", " ++ prettyExpression es
    Add e1 e2 -> prettyExpression [e1] ++ " + " ++ prettyExpression [e2] ++ ", " ++ prettyExpression es


prettyVars :: [Var] -> String
prettyVars []     = ""
prettyVars (v:vs) = show v ++ prettyVars vs



optE :: Expr -> Expr
optE = undefined --Add e1 e2 =
--     if (typeOf (e1) == Number && typeOf (e2) == Number)
--         then stackPop (stackPush (e1))
--     else




optP :: Prog -> Prog
optP = undefined;




























__
