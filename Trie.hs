module Trie where 

import Test.HUnit

{-
Trie Node stores children, the string, its priority (higher the number, 
higher the priority), and whether it is a key (end of word)
 -}
data TrieNode
  = Node {children :: [TrieNode], 
          str :: String, 
          prio :: Int, 
          isKey :: Bool}

root :: TrieNode 
root = Node [] "" 0 False

{-
insert n s
inserts String s into TrieNode n and returns n after insertion
 -}
insert :: TrieNode -> String -> TrieNode
insert (Node [] str prio isKey) (s : ss) 
  = Node [insert n ss] str prio isKey where
    n = Node [] [s] 0 False
insert (Node c str prio isKey) "" -- Reached end of the String
  = Node {children = c, str = str, prio = prio + 1, isKey = True}
insert (Node c str' prio isKey) (s : ss)
  = Node (map f c) str' prio isKey where 
    f n = if str n == [s] then insert n ss else n

testInsert = 
  TestList
  [

  ]


main :: IO ()
main = do
  _ <- runTestTT testInsert
  return ()
