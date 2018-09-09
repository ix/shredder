{-# LANGUAGE ApplicativeDo #-}
module Data.BTree where

import Diagrams.Prelude hiding (Empty)
import Diagrams.Backend.SVG

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Read, Show, Eq, Ord)

node :: Tree String -> Diagram B
node Empty = mempty
node (Node n _ _) = square 1 # pad 3 # lw 0

tree :: Integer -> Tree String -> Diagram B
tree _ Empty = mempty
tree n t@(Node x Empty Empty) = node t # named n
tree n t@(Node x l Empty) = connect' plain n (n + 1 ) $ nl === nx
  where nx = node t # named n
        nl = tree (n + 1) l
tree n t@(Node x Empty r) = connect' plain n (n + 1) $ nr === nx
  where nx = node t # named n
        nr = tree (n + 1) r 
tree n t@(Node x l r) = connect' plain n (n + 1) $ connect' plain n (n + 2) $ (nl ||| nr) === nx
  where nx = node t # named n
        nl = tree (n + 1) l 
        nr = tree (n + 2) r

diagram :: Diagram B
diagram = tree 1 $ "A" <: (single "B", "C" <: ("D" <: (single "E", single "F"), Empty))

single :: a -> Tree a
single n = Node n Empty Empty

(<:) :: a -> (Tree a, Tree a) -> Tree a
a <: c = Node a (fst c) (snd c)


-- a headless & tailless arrow
plain :: ArrowOpts Double
plain = with & arrowHead .~ noHead & arrowTail .~ noTail
