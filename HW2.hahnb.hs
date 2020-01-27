-- Teams: Bryce Hahn hahnb@oregonstate.edu
   --     Brenden Smith smitbre2@oregonstate.edu
   --     Sheldon Roberts robeshel@oregonstate.edu

module HW2 where
import Control.Applicative

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

-- Allow a list of arbitrary data types for the tree implementation
data List a = Nil
    | Cons a (List a)

-- | A path is a sequence of steps. Each node in a binary tree can be
--   identified by a path, indicating how to move down the tree starting
--   from the root.
type Path = [Step]

-- | Create a leaf node.
leaf :: a -> Tree a
leaf x = Node x End End

-- | An example tree.
ex :: Tree Int
ex = Node 4 (Node 3 (leaf 2) End)
            (Node 7 (Node 5 End (leaf 6))
                    (leaf 8))

--
-----------------------
--    Encode List    --
-----------------------
--
--  Encode List funtion will take in an array of arbitrary type a, and pattern match
--      on the list to walk through each index and create a tree of only right nodes.
--  Globals: List of arbitrary type a
--  Return: A Tree of type a
--
encodeList :: [a] -> Tree a
encodeList []     = End
encodeList (x:xs) = Node x End (encodeList xs)

--
--------------------
--    Map Tree    --
--------------------
--
--  Map Tree function will use pattern matching to recursively travel through the
--      tree and apply the provided function to each node of the tree.
--  Globals: An arbitrary function of type a to b, and a Tree of type a
--  Return: A Tree of type b
--
mapTree :: Eq a => (a -> b) -> Tree a -> Tree b
mapTree _ End          = End -- end of the tree recursion, end node
mapTree f (Node x l r) = Node (f x) (mapTree f l) (mapTree f r)

--
--------------------
--    Value At    --
--------------------
--
--  Value At funtion will use pattern matching and [Step] test cases to walk through
--      the tree and find the provided values, or Nothing if not in the tree
--  Globals: List of Steps, Tree of type a
--  Return: Just x if value is in Tree, else Nothing
--
valueAt :: [Step] -> Tree a -> Maybe a
valueAt _ End               = Nothing
valueAt (p:ps) (Node x l r) =
    if p == L then valueAt ps l -- next index in step is left, recur left
    else if p == R then valueAt ps r -- next index in step is right, recur right
    else Just x
valueAt _ (Node x _ _)      = Just x


--
-------------------
--    Path To    --
-------------------
--
--  Path To funtion will use pattern matching to then test if the number is at
--      current nodes value, then finish the list. Else, in order to access the
--      information in Maybe Path returned from the recursive calls, we use fmap
--      to concatenate the step with the return Path (an array of steps) from the
--      Maybe. Then to simplify, using <|> after testing if p == x, we can simply
--      write the left and right recursive calls on their respective sides.
--      (instead of more else if statements testing if x < p > x?)
--  Globals: An arbitrary type a, a Tree of type a
--  Return: A path to a if path exists, else Nothing
--
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo _ End          = Nothing
pathTo p (Node x l r) =
    if p == x then Just [] -- fmap and <|> operator concepts suggested to try by Kai Gay
    else (fmap (L:) (pathTo p l) <|> fmap (R:) (pathTo p r))
