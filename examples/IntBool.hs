-- | A simple expression language with two types.
module IntBool where

import Prelude hiding (not,and,or)


--  Syntax of the "core" IntBool language:
--
--  int  ::=  (any integer)
--  bool ::=  true  |  false
--
--  exp  ::=  int                integer literal
--        |   exp + exp          integer addition
--        |   exp * exp          integer multiplication
--        |   exp = exp          check whether two values are equal
--        |   exp ? exp : exp    conditional expressions


-- 1. Define the abstract syntax as a Haskell data type.
type Prog = [Expr]

data Expr = Lit Int
          | Add Expr Expr
          | Mul Expr Expr
          | Equ Expr Expr
          | If Expr Expr Expr
          deriving (Eq, Show)

-- Here are some example expressions:
--  * encode the abstract syntax tree as a Haskell value
--  * draw the abstract syntax trees (exercise)
--  * what should the result be?

-- | 2 * (3 + 4)
ex1 :: Expr
ex1 = Mul (Lit 2) (Add (Lit 3) (Lit 4))

-- | 2 * (3 + 4) == 10
ex2 :: Expr
ex2 = Equ ex1 (Lit 10)

-- | 2 * (3 + 4) ? 5 : 6
ex3 :: Expr
ex3 = If ex1 (Lit 5) (Lit 6)

-- | 2 * (3 + 4) == 10 ? 5 : 6
ex4 :: Expr
ex4 = If ex2 (Lit 5) (Lit 6)


-- 2. Identify/define the semantic domain for this language
--   * what types of values can we have?
--   * how can we express this in Haskell?
-- Types of values we can have
--  * Int
--  * Bool
--  * Error

data Value
    = I Int
    | B Bool
    | Error
   deriving (Eq, Show)

-- Alternative semantics domain using Maybe and Either:
--
--    data Maybe a = Nothing | Just a
--    data Either a b = Left a | Right b
--
--   type Value Maybe (Either Int Bool)
-- Example semantic values in both representations:
--
--    I 6    <=> Just (Left 6)
--    B True <=> Just (Right True)
--    Error  <=> Nothing


-- 3. Define the valuation function
sem :: Expr -> Value
sem (Lit i)    = I i
sem (Add l r)  = case (sem l, sem r) of
                    (I i, I j) -> I (i + j)
                    _ -> Error
sem (Mul l r)  = case (sem l, sem r) of
                    (I i, I j) -> I (i * j)
                    _ -> Error
sem (Equ l r)  = case (sem l, sem r) of
                    (B a, B b) -> B (a == b)
                    (I i, I j) -> B (i == j)
                    _ -> Error
sem (If c t e) = case sem c of
                    B True  -> sem t
                    B False -> sem e
                    _ -> Error


-- 4. Syntactic sugar.
--
-- Goal: extend the syntax of our language with the following operations:
--
--      * boolean literals
--      * integer negation
--      * boolean negation (not)
--      * conjunction (and)
--      * disjunction (or)
--
-- How do we do this? Can we do it without changing the semantics?
true :: Expr
true = Equ (Lit 1) (Lit 0)

false :: Expr
false = Equ (Lit 0) (Lit 1)

neg :: Expr -> Expr
neg e = Mul (Lit (-1)) e

not :: Expr -> Expr
not e = If e false true

and :: Expr -> Expr -> Expr
and e1 e2 = If e1 e2 false

or :: Expr -> Expr -> Expr
or e1 e2 = If e1 true e2

-- | Example program that uses our syntactic sugar.
--     not true || 3 == -3 && (true || false)
ex5 = or (not true) (and (Equ (Lit 3) (neg (Lit 3))) (or true false))


--
-- * Statically typed variant (now!)
--

-- 1. Define the syntax of types
data Type = TBool | TInt | TError
  deriving (Eq,Show)


-- 2. Define the typing relation.
typeOf :: Expr -> Type
typeOf (Lit i)    = TInt
typeOf (Add l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TInt
                      _ -> TError
typeOf (Mul l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TInt
                      _ -> TError
typeOf (Equ l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt)   -> TBool
                      (TBool, TBool) -> TBool
                      _ -> TError
typeOf (If c t e) = case (typeOf c, typeOf t, typeOf e) of
                      (TBool, tt, te) -> if tt == te then tt else TError
                      _ -> TError


-- 3. Define the semantics of type-correct programs.
sem' :: Expr -> Either Int Bool
sem' (Lit i)    = Left i
sem' (Add l r)  = Left (evalInt l + evalInt r)
sem' (Mul l r)  = Left (evalInt l * evalInt r)
sem' (Equ l r)  = Right (sem' l == sem' r)
sem' (If c t e) = if evalBool c then sem' t else sem' e

-- | Helper function to evaluate an Expr to an Int.
evalInt :: Expr -> Int
evalInt e = case sem' e of
              Left i -> i
              _ -> error "internal error: expected Int, got something else!"

-- | Helper function to evaluate an Expr to an Bool.
evalBool :: Expr -> Bool
evalBool e = case sem' e of
               Right b -> b
               _ -> error "internal error: expected Bool, got something else!"

-- 4. Define our interpreter.
eval :: Exp -> Value
eval e = case typeOf e of
          TInt   -> I (evalInt e)
          TBool  -> B (evalBool e)
          TError -> Error
