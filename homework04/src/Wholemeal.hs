----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 04
--
----------------------------------------------------------------------

module Wholemeal where

import Data.List

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3*n + 1)

-- |
--
-- >>> fun1 [1,3,5,7] == fun1' [1,3,5,7]
-- True
-- >>> fun1 [1,2,3] /= fun1' [1,2,3]
-- False
-- >>> fun2 10 == fun2' 10
-- True
-- >>> fun2 15 /= fun2' 15
-- False

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2). filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

data Tree a =
    Leaf
  | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr buildTree Leaf

buildTree :: a -> Tree a -> Tree a
buildTree a Leaf = Node 0 Leaf a Leaf
buildTree a (Node _ l m r)
   | height l <= height r = Node (height (buildTree a l) + 1) (buildTree a l) m r
   | otherwise = Node (height (buildTree a r) + 1) l m (buildTree a r)
  where
    height Leaf = -1
    height (Node h _ _ _) = h

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> xor [False, True, False]
-- True
-- >>> xor [False, True, False, False, True]
-- False

xor :: [Bool] -> Bool
xor = odd . foldr accumTrue 0

accumTrue :: Bool -> Integer -> Integer
accumTrue True acc = acc + 1
accumTrue False acc = acc

-- |
--
-- >>> map' (+1) [1,2,3]
-- [2,3,4]

map' :: (a -> b) -> [a] -> [b]
map' f = foldr myFun []
  where
    myFun value acc = f value : acc

-- Could also be written like this but hlint cries :(
-- map' f = foldr (\value acc -> f value : acc) []

-- Optional

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f acc list = foldr (flip f) acc (reverse list)

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [1..n] \\ sieve n

sieve :: Integer -> [Integer]
sieve n = map calculate $ filter (\(i,j) -> calculate (i,j) <= n) (cartProd [1..n] [1..n])

calculate :: (Integer, Integer) -> Integer
calculate (i,j) = i + j + 2 * i * j

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
