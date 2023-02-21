module Trie where 

import Test.HUnit
import Data.Maybe

{-
Trie Node stores children, the string, its priority (higher the number, 
higher the priority), and whether it is a key (end of word)
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
insert n s
Inserts String s into TrieNode n and returns n after insertion.
The function should traverse the Trie while shortening the string by the ones it
visited until it reaches two options:
  1. The string has been exhausted: the isKey flag of the current Node is set to True
     and prio is incremented
  2. The string has not been exhausted and the current Node does not have any 
     more children: continue with a chain of Nodes of the remaining letters in 
     the string
 -}
insert :: TrieNode -> String -> TrieNode
insert (Node [] str prio isKey) (s:ss)
  = Node [insert n ss] str prio isKey where
    n = Node [] [s] 0 False
insert (Node c str prio isKey) "" 
  = if str /= "" then 
      Node {children = c, str = str, prio = prio + 1, isKey = True}
    else 
      Node c str prio isKey
insert (Node c str' prio isKey) (s:ss)
  = Node (para f [insert (Node [] [s] 0 False) ss] c) str' prio isKey where 
    f n ns acc = if str n == [s] then insert n ss : ns else n : acc

testInsert = 
  TestList
  [ insert root "" ~?= Node [] "" 0 False,
    insert root "H" ~?= Node [Node [] "H" 1 True] "" 0 False,
    insert (insert root "H") "H" ~?= Node [Node [] "H" 2 True] "" 0 False,
    insert (insert root "H") "Hi" ~?= Node [Node [Node [] "i" 1 True] "H" 1 True] "" 0 False,
    insert root "Hi" ~?= Node [Node [Node [] "i" 1 True] "H" 0 False] "" 0 False,
    insert (insert root "Hi") "Hi" ~?= Node [Node [Node [] "i" 2 True] "H" 0 False] "" 0 False,
    insert (insert root "Hi") "Ha" ~?= Node [Node [Node [] "i" 1 True, Node [] "a" 1 True] "H" 0 False] "" 0 False
  ]

{-
search n s
Searches String s in Trie with root Node n and returns True if the String is in the Trie, False otherwise.
 -}
search :: TrieNode -> String -> Bool
search (Node _ _ _ isKey) "" = isKey
search (Node [] _ _ _) _ = False
search (Node c str prio isKey) (s:ss) = 
  case searchList c [s] of
    Just n -> search n ss
    Nothing -> False

testSearch = 
  TestList
  [ search root "a" ~?= False, 
    search (insert root "Hi") "Hi" ~?= True,
    search (insert root "Hi") "i" ~?= False,
    search (insert root "Hi") "Hiy" ~?= False,
    search (insert (insert root "H") "Hi") "H" ~?= True,
    search (insert (insert root "H") "Hi") "Hiy" ~?= False
  ]

{-
searchList ns s
Searches for a node with str s in ns. Returns Just TrieNode if node is found, Nothing otherwise.
 -}
searchList :: [TrieNode] -> String -> Maybe TrieNode
searchList [] _ = Nothing
searchList (n:ns) s = if str n == s then Just n else searchList ns s


main :: IO ()
main = do
  _ <- runTestTT testInsert
  return ()
