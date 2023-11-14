-- (custom) depth first graph traversal by Levin Rainey 
-- levin.rainey.lrr@gmail.com
-- made public 14/11/23
-- v1
-- This program doesn't use a stack, but instead tracks every visited node.

import Data.List (elemIndex)

trav :: [(Char, String)] -> Char -> Char -> String -> String
trav [] _ _ _ = []
trav graph node endNode visited
    | not (elem node visited) = trav graph node endNode (visited ++ [node])  -- repeat but after adding this node to visited list.
    | node == endNode = visited
    | not (null unvisitedAttachedNodes) = trav graph (head unvisitedAttachedNodes) endNode visited
    | not (null visited) = if not (null attachedNodesWithUnvisited) then (trav graph (head attachedNodesWithUnvisited) endNode (visited ++ [(head attachedNodesWithUnvisited)])) else (trav graph (head (tail (reverse visited))) endNode (visited ++ [head (tail (reverse visited))]))
    | otherwise = visited
    where
        unvisitedAttachedNodes = [x | x <- getAttachedVertexes graph node, not (elem x visited)]
        prevIndex = [x | x <- (reverse visited), x < (head (reverse visited))]
        unvisitedPrevNodes = [x | x <- prevIndex, elem x [y | y <- (getAttachedVertexes graph x), not (elem y visited)]]
        nodeToGoBackTo = visited !! (getIndex (head (reverse unvisitedPrevNodes)) visited)
        attachedNodesWithUnvisited = [nb | nb <- (getAttachedVertexes graph node), (hasUnvisited graph nb visited)]

getAttachedVertexes :: [(Char, String)] -> Char -> [Char]
getAttachedVertexes graph node = case lookup node graph of
    Just value -> value
    Nothing -> ""

hasUnvisited :: [(Char, String)] -> Char -> String -> Bool
hasUnvisited graph node visited = not (null unvisited) where
    unvisited = [x | x <- (getAttachedVertexes graph node), not (elem x visited)]

count :: Eq a => a -> [a] -> Int
count item list = length (filter (== item) list)

getIndex :: Eq a => a -> [a] -> Int
getIndex item list = case elemIndex item list of
    Just index -> index
    Nothing -> error "help"
