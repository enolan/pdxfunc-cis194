module Week2 where

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
parseMessage _ = Unknown "unimplemented" -- YOUR CODE GOES HERE

parse :: String -> [LogMessage]
parse _ = [] -- YOUR CODE GOES HERE

insert :: LogMessage -> MessageTree -> MessageTree
insert _ _ = Leaf -- YOUR CODE GOES HERE

build :: [LogMessage] -> MessageTree
build _ = Leaf -- YOUR CODE GOES HERE

inOrder :: MessageTree -> [LogMessage]
inOrder _ = [] -- YOUR CODE GOES HERE

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong _ = [] -- YOUR CODE GOES HERE
