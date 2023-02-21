module Trie where 

import Test.HUnit
import Data.Maybe
import Data.List

{-
Trie Node stores children, the string, its priority (higher the number, 
higher the priority), and whether it is a key (end of word).
The priority of a Node is increased when the same word is inserted multiple times.
 -}
data TrieNode
  = Node {children :: [TrieNode], 
          str :: String, 
          prio :: Int, 
          isKey :: Bool}
  deriving (Eq, Show)

root :: TrieNode 
root = Node [] "" 0 False

para :: (a -> [a] -> b -> b) -> b -> [a] -> b
para _ b [] = b
para f b (x:xs) = f x xs (para f b xs)

{-
insertString n s
Inserts String s into TrieNode n and returns n after insertion.
The function should traverse the Trie while shortening the string by the ones it
visited until it reaches two options:
  1. The string has been exhausted: the isKey flag of the current Node is set to True
     and prio is incremented
  2. The string has not been exhausted and the current Node does not have any 
     more children: continue with a chain of Nodes of the remaining letters in 
     the string
 -}
insertString :: TrieNode -> String -> TrieNode
insertString (Node [] str prio isKey) (s:ss)
  = Node [insertString n ss] str prio isKey where
    n = Node [] [s] 0 False
insertString (Node c str prio isKey) "" 
  = if str /= "" then 
      Node {children = c, str = str, prio = prio + 1, isKey = True}
    else 
      Node c str prio isKey
insertString (Node c str' prio isKey) (s:ss)
  = Node (para f [insertString (Node [] [s] 0 False) ss] c) str' prio isKey where 
    f n ns acc = if str n == [s] then insertString n ss : ns else n : acc

testInsert = 
  TestList
  [ insertString root "" ~?= Node [] "" 0 False,
    insertString root "H" ~?= Node [Node [] "H" 1 True] "" 0 False,
    insertString (insertString root "H") "H" ~?= Node [Node [] "H" 2 True] "" 0 False,
    insertString (insertString root "H") "Hi" ~?= Node [Node [Node [] "i" 1 True] "H" 1 True] "" 0 False,
    insertString root "Hi" ~?= Node [Node [Node [] "i" 1 True] "H" 0 False] "" 0 False,
    insertString (insertString root "Hi") "Hi" ~?= Node [Node [Node [] "i" 2 True] "H" 0 False] "" 0 False,
    insertString (insertString root "Hi") "Ha" ~?= Node [Node [Node [] "i" 1 True, Node [] "a" 1 True] "H" 0 False] "" 0 False
  ]

{-
search n s
Searches String s in Trie with root Node n and returns True if the String is in the Trie, False otherwise.
 -}
search :: TrieNode -> String -> Bool
search (Node _ _ _ isKey) "" = isKey
search (Node [] _ _ _) _ = False
search (Node c str _ _) (s:ss) = 
  case searchList c [s] of
    Just n -> search n ss
    Nothing -> False

testSearch = 
  TestList
  [ search root "a" ~?= False, 
    search (insertString root "Hi") "Hi" ~?= True,
    search (insertString root "Hi") "i" ~?= False,
    search (insertString root "Hi") "Hiy" ~?= False,
    search (insertString (insertString root "H") "Hi") "H" ~?= True,
    search (insertString (insertString root "H") "Hi") "Hiy" ~?= False
  ]

{-
searchList ns s
Searches for a node with str s in ns. Returns Just TrieNode if node is found, Nothing otherwise.
 -}
searchList :: [TrieNode] -> String -> Maybe TrieNode
searchList [] _ = Nothing
searchList (n:ns) s = if str n == s then Just n else searchList ns s

{-
getHighestPrio n s
Returns the string with the highest priority in the Trie with root n given the prefix s.
If a string does not exist with the prefix s, return Nothing.
-}
getHighestPrio :: TrieNode -> String -> Maybe String
getHighestPrio n = aux n "" where
  aux (Node [] _ _ isKey) b _ = if isKey then Just b else Nothing
  aux (Node c str prio isKey) b (s:ss) = 
    case searchList c [s] of
    Just n -> aux n (b ++ [s]) ss
    Nothing -> Nothing
  aux (Node c str prio isKey) b "" = 
    case searchList c "" of
    Just n -> aux n (b ++ str n) ""
    Nothing -> Nothing



main :: IO ()
main = do
  _ <- runTestTT testInsert
  return ()
