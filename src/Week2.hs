module Week2 where

import Data.List (foldl')

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

parseMessage :: String -> LogMessage
parseMessage str = if not $ any (== take 2 str) ["I ", "W ", "E "]
  then Unknown str
  else go $ words str
  where
  go :: [String] -> LogMessage
  go ("I" : ts : msg) = LogMessage Info (read ts) $ unwords msg
  go ("W" : ts : msg) = LogMessage Warning (read ts) $ unwords msg
  go ("E" : severity : ts : msg) =
    LogMessage (Error $ read severity) (read ts) $ unwords msg
  go _            = error
    "parseMessage: go got a line that doesn't start with a valid letter"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) Leaf            = Leaf
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg2@(LogMessage _ ts2 _) (Node lTree msg1@(LogMessage _ ts1 _) rTree) =
  if ts1 <= ts2
  then Node lTree msg1 (insert msg2 rTree)
  else Node (insert msg2 lTree) msg1 rTree
insert _ (Node _ (Unknown _) _)    = error "insert: tree with Unknown in it"
insert (Unknown _) tree@(Node _ _ _) = tree

build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lTree msg rTree) = inOrder lTree ++ [msg] ++ inOrder rTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map getMsgStr $ filter relevant (inOrder $ build msgs)
  where
  relevant (LogMessage (Error n) _ _) = n >= 50
  relevant _                          = False
  getMsgStr (LogMessage _ _ str) = str
  getMsgStr _                    = ""
