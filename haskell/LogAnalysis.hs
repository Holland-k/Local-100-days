{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage (x) = case x of
    ('I':xs) -> LogMessage Info (read y) (drop (length y + 2) xs)
        where y = head(words xs)
    ('W':xs) -> LogMessage Warning (read y) (drop (length y + 2) xs)
        where y = head(words xs)
    ('E':xs) -> LogMessage (Error (read y)) (read z) (drop (length y + length z + 3) xs)
        where (y,z) = (head(words xs), head(tail(words xs)))
    _ -> Unknown x

parseLines :: [String] -> [LogMessage]
parseLines [] = []
parseLines (x:xs) = parseMessage x : parseLines xs

parse :: String -> [LogMessage]
parse x = parseLines (lines x)

getTime :: LogMessage -> TimeStamp
getTime (LogMessage _ ts _) = ts
getTime (Unknown _) = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert mess Leaf = Node Leaf mess Leaf
insert mess (Node l n r) = if getTime mess < getTime n 
    then (Node (insert mess l) n r) 
    else (Node l n (insert mess r))

buildTree :: [LogMessage] -> MessageTree -> MessageTree
buildTree [] a = a 
buildTree (x:xs) tree = buildTree xs (insert x tree)

build :: [LogMessage] -> MessageTree
build x = buildTree x Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l n r) = inOrder (l) ++ [n] ++ inOrder (r)

filterForErrors :: LogMessage -> String
filterForErrors (LogMessage Info _ _) = ""
filterForErrors (LogMessage Warning _ _) = ""
filterForErrors (LogMessage (Error x) _ s) = if x > 50 then s else []
filterForErrors (Unknown _) = "" 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong a = filter (/= "") (map filterForErrors b)
    where b = inOrder (build a)