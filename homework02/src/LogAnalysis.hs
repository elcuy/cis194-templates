{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage message = let list = words message in
   case list of
     ("I":time:msg)       -> LogMessage Info (read time) (unwords msg)
     ("W":time:msg)       -> LogMessage Warning (read time) (unwords msg)
     ("E":level:time:msg) -> LogMessage (Error (read level)) (read time) (unwords msg)
     _                    -> Unknown "This is not in the right format"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>>
--

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node leftTree msg2@(LogMessage _ ts2 _) rightTree)
  | ts1 < ts2 = Node (insert msg1 leftTree) msg2 rightTree
  | otherwise = Node leftTree msg2 (insert msg1 rightTree)
insert _ tree = tree

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>>
--

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  map getMessage . inOrder . build . filter isSevere
  where isSevere (LogMessage (Error s) _ _) = s > 50
        isSevere _                          = False
        getMessage (LogMessage _ _ message) = message
        getMessage _                        = ""

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------
