module Tests where

import Trie
import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT testTrie
  return ()

testTrie = "testTrie" ~:
  TestList
  [ testInsert,
    testSearch,
    testGetHighestPrio]


testInsert = "testList" ~:
  TestList
  [ insertString root "" ~?= Node [] "" 0 False,
    insertString root "H" ~?= Node [Node [] "h" 1 True] "" 0 False,
    insertString (insertString root "H") "H" ~?= Node [Node [] "h" 2 True] "" 0 False,
    insertString (insertString root "H") "Hi" ~?= Node [Node [Node [] "i" 1 True] "h" 2 True] "" 0 False,
    insertString root "Hi" ~?= Node [Node [Node [] "i" 1 True] "h" 1 False] "" 0 False,
    insertString (insertString root "Hi") "Hi" ~?= Node [Node [Node [] "i" 2 True] "h" 2 False] "" 0 False,
    insertString (insertString root "Hi") "Ha" ~?= Node [Node [Node [] "i" 1 True, Node [] "a" 1 True] "h" 2 False] "" 0 False
  ]

testSearch = "testSearch" ~:
  TestList
  [ search root "a" ~?= False, 
    search (insertString root "Hi") "Hi" ~?= True,
    search (insertString root "Hi") "i" ~?= False,
    search (insertString root "Hi") "Hiy" ~?= False,
    search (insertString (insertString root "H") "Hi") "H" ~?= True,
    search (insertString (insertString root "H") "Hi") "Hiy" ~?= False
  ]

testGetHighestPrio = "testGetHighestPrio" ~:
  TestList 
  [ getHighestPrio (insertString root "Hello") "" ~?= Just "hello",
    getHighestPrio (insertString root "Hello") "He" ~?= Just "hello",
    getHighestPrio (insertString (insertString (insertString root "Hello") "Hello") "Her") "He" ~?= Just "hello",
    getHighestPrio (insertString (insertString (insertString root "Hello") "Hello") "He") "He" ~?= Just "he",
    getHighestPrio (insertString (insertString (insertString root "Hello") "Her") "Her") "He" ~?= Just "her",
    getHighestPrio (insertString (insertString (insertString root "Her") "Hello") "Her") "He" ~?= Just "her",
    getHighestPrio (insertString (insertString (insertString root "Her") "Her") "Hello") "He" ~?= Just "her",
    getHighestPrio (insertString root "Hello") "J" ~?= Nothing
  ]
