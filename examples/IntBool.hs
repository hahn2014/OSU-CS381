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
type Int = int
data bool = True | False

type Prog = [Expr]

data Expr = Int Int
          | Add Expr Expr
          | Mul Expr Expr
          | Equ Expr Expr
          | IfElse Expr Expr Expr


-- Here are some example expressions:
--  * encode the abstract syntax tree as a Haskell value
--  * draw the abstract syntax trees (exercise)
--  * what should the result be?

-- | 2 * (3 + 4)
ex1 = undefined

-- | 2 * (3 + 4) == 10
ex2 = undefined

-- | 2 * (3 + 4) ? 5 : 6
ex3 = undefined

-- | 2 * (3 + 4) == 10 ? 5 : 6
ex4 = undefined


-- 2. Identify/define the semantic domain for this language
--   * what types of values can we have?
--   * how can we express this in Haskell?



-- Alternative semantics domain using Maybe and Either:
--
--
-- Example semantic values in both representations:
--
--


-- 3. Define the valuation function
sem = undefined


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


-- | Example program that uses our syntactic sugar.
--     not true || 3 == -3 && (true || false)
ex5 = undefined


--
-- * Statically typed variant (later)
--

-- 1. Define the syntax of types


-- 2. Define the typing relation.

typeOf = undefined


-- 3. Define the semantics of type-correct programs.

sem' = undefined


-- 4. Define our interpreter.
eval = undefined
