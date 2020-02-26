data Expr = Get
          | Lit Int
          | Add Expr Expr
         deriving (Eq,Show)
--          | LTE Expr Expr

-- removing <= from expr, we remove the chance of a type error
data Test = LTE Expr Expr
         deriving (Eq,Show)

data Stmt = Set Expr
          | While Test Stmt
          | Begin [Stmt]
         deriving (Eq,Show)

-- p :: Stmt
p = Begin
      [ Set (Lit 1)
      , While (LTE Get (Lit 100))
          (Set (Add Get Get)
      ]

--
type Reg = Int

-- expr: Reg -> Int
-- test: Reg -> Bool
-- stmt: Reg -> Reg

-- | Valuation function for expressions
expr :: Expr -> Reg -> Int
expr Get       s = s
expr (Lit i)   s = i
expr (Add l r) s = expr l s + expr r s

-- | Valuation function for tests
test :: Test -> Reg -> Bool
test (LTE l r) s = expr l s <= expr r s

-- | Valuation function for statements
stmt :: Stmt -> Reg -> Reg
stmt (Set e)     s = expr e s
stmt (While c b) s = if test c s then stmt (While c b) (stmt b s) else s
stmt (Begin ss)  s = foldl (flip stmt) s ss
--    stmts ss s where stmts [] r = r
--              stmts (s:ss) r = stmts ss (stmt s r)
