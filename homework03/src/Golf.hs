----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 03
--
----------------------------------------------------------------------

module Golf where

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> skips "ABCD"
-- ["ABCD","BD","C","D"]
-- >>> skips "hello!"
-- ["hello!","el!","l!","l","o","!"]
-- >>> skips [1]
-- [[1]]
-- >>> skips [True, False]
-- [[True,False],[False]]
-- >>> skips []
-- []

skips :: [a] -> [[a]]
skips list = [nthElements list n | n <- [1..(length list)]]

-- Gets the elements in the positions dictated by a list comprehension
-- that goes from the nth element and skips n positions until the end
-- of the list.

nthElements :: [a] -> Int -> [a]
nthElements list n = [ list !! x | x <- [n - 1, n - 1 + n..length list - 1]]

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>> localMaxima [2,9,5,6,1]
-- [9,6]
-- >>> localMaxima [2,3,4,1,5]
-- [4]
-- >>> localMaxima [1,2,3,4,5]
-- []

localMaxima :: [Integer] -> [Integer]
localMaxima (a:b:c:rest)
  | b > a && b > c = b : localMaxima (b:c:rest)
  | otherwise      = localMaxima (b:c:rest)
localMaxima _ = []

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>> histogram [1,1,1,5]
-- " *        \n *        \n *   *    \n==========\n0123456789\n"

histogram :: [Integer] -> String
histogram list = printRows (countNums list) ++ "==========\n0123456789\n"

countNums :: [Integer] -> [Integer]
countNums list = [toInteger $ length $ filter (==n) list | n <- [0..9]];

printRows :: [Integer] -> String
printRows list
  | isValid = printRows (map (subtract 1) list) ++  printRow list
  | otherwise = ""
  where isValid = length (filter (<1) list) < 10

printRow :: [Integer] -> String
printRow list = map (\n -> if n < 1 then ' ' else '*') list ++ "\n"
