{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1

parseStr :: [String] -> String
parseStr s = unwords s

parseTimeStamp :: String -> TimeStamp
parseTimeStamp s = read s :: Int

parseError :: String -> MessageType
parseError s = (Error (read s :: Int))

parseMessage :: String -> LogMessage
parseMessage str = case words str of
                    ("I":t:s) -> LogMessage Info (parseTimeStamp(t)) (parseStr(s))
                    ("W":t:s) -> LogMessage Warning (parseTimeStamp(t)) (parseStr(s))
                    ("E":i:t:s) -> LogMessage (parseError(i)) (parseTimeStamp(t)) (parseStr(s))
                    _ -> Unknown str

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert p@(LogMessage _ t1 _) (Node leftMessageTree q@(LogMessage _ t2 _) rightMessageTree)
  | t1 < t2 = Node (insert p leftMessageTree) q rightMessageTree
  | otherwise = Node leftMessageTree q (insert p rightMessageTree)

-- Exercise 3

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (h:t) = insert h (build t)

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftMessageTree logMessage rightMessageTree) = (inOrder leftMessageTree) ++ (logMessage : (inOrder rightMessageTree))

-- Exercise 5

isRelevantError :: LogMessage -> Bool
isRelevantError (LogMessage (Error t) _ _)
  | t >= 50 = True
  | otherwise = False
isRelevantError _ = False

toString :: LogMessage -> String
toString (LogMessage _ _ s) = s
toString _ = ""

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logMessages = map toString (filter isRelevantError (inOrder (build logMessages)))
