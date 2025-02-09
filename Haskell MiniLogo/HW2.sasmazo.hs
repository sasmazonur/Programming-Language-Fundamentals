--Onur Sasmaz
module HW2 where

-- | Binary trees with nodes labeled by values of an arbitrary type.
data Tree a
   = Node a (Tree a) (Tree a)
   | End
  deriving (Eq,Show)

-- | One step in a path, indicating whether to follow the left subtree (L)
--   or the right subtree (R).
data Step = L | R
  deriving (Eq,Show)

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


-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--
--   >>> mapTree odd End
--   End
--
--   >>> mapTree even (Node 5 (leaf 2) End)
--   Node False (Node True End End) End
--
--   >>> (mapTree not . mapTree even) (Node 5 End (leaf 2))
--   Node True End (Node False End End)
--
--   >>> mapTree (+10) ex
--   Node 14 (Node 13 (Node 12 End End) End) (Node 17 (Node 15 End (Node 16 End End)) (Node 18 End End))
--
--   >>> ex == (mapTree (subtract 27) . mapTree (+27)) ex
--   True
--
--map (a -> b) -> [a] -> [b]
--map _ [] = []
--map f (x:xs) = f x : map f xs

-- | Map a function over a tree. Applies the given function to every label
--   in the tree, preserving the tree's structure.
--Sources:http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures
mapTree :: (a->b) -> Tree a -> Tree b
mapTree x End = End
mapTree x (Node a left right) = Node (x a) (mapTree x left) (mapTree x right)


-- | Get the value at the node specified by a path. Returns 'Nothing' if
--   the given path is invalid.
--
--   >>> valueAt [] ex
--   Just 4
--
--   >>> valueAt [L,L] ex
--   Just 2
--
--   >>> valueAt [L,R] ex
--   Nothing
--
--   >>> valueAt [R,L,R] ex
--   Just 6
--
--   >>> valueAt [L,L,L] ex
--   Nothing
--
-- From getting the list we use "Int -> [a] -> a"
-- So, <Whats Here> -> Tree a -> Maybe a

-- Get the value at the node specified by a path. Returns 'Nothing' if
-- the given path is invalid.
valueAt :: Path -> Tree a -> Maybe a
valueAt _ End     = Nothing
valueAt [] (Node a _ _) = Just a
valueAt (L:path) (Node _ left _)  = valueAt path left
valueAt (R:path) (Node _ _ right) = valueAt path right


-- | Find a path to a node that contains the given value.
--
--   >>> pathTo 3 (leaf 5)
--   Nothing
--
--   >>> pathTo 5 ex
--   Just [R,L]
--
--   >>> pathTo 6 ex
--   Just [R,L,R]
--
--   >>> pathTo 4 ex
--   Just []
--
--   >>> pathTo 10 ex
--   Nothing
--
--EXAMPLE
--case expression of pattern -> result
--describeList :: [a] -> String
--describeList xs = "The list is " ++ case xs of [] -> "empty."
--                         [x] -> "a singleton list."
--                         xs -> "a longer list."

-- Find a path to a node that contains the given value.
pathTo :: Eq a => a -> Tree a -> Maybe Path
pathTo _ End = Nothing
pathTo x (Node a left right)
   | x == a = Just []
   | otherwise = case (pathTo x left, pathTo x right) of
           (Just path, _)-> Just (L:path)
           (_, Just path)-> Just (R:path)
           _             -> Nothing
