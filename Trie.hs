module Trie where 

import Data.Maybe
import Data.List
import Data.Char (toLower, isLetter)
import Prelude

{-
This particular Trie only stores words consisting of only letters and stores each 
letter in lowercase. 

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

findNode :: String -> [TrieNode] -> Maybe TrieNode
findNode s = find (\x -> str x == s)

maxPrio :: [TrieNode] -> Maybe TrieNode
maxPrio [] = Nothing
maxPrio (n:ns) = case maxPrio ns of
  Just x -> if prio n > prio x then Just n else Just x
  Nothing -> Just n

isLetters :: String -> Bool
isLetters = foldr (\c acc -> isLetter c && acc) True 

{-
insertString n s
Inserts String s into TrieNode n and returns n after insertion.
The function only takes in strings consisting of only letters. The TrieNode n will be returned
without any changes if it does not. Otherwise, the word in all lowercase will be inserted.
The function should traverse the Trie while shortening the string by the ones it
visited until it reaches two options:
  1. The string has been exhausted: the isKey flag of the current Node is set to True
     and prio is incremented
  2. The string has not been exhausted and the current Node does not have any 
     more children: continue with a chain of Nodes of the remaining letters in 
     the string
 -}
insertString :: TrieNode -> String -> TrieNode
insertString (Node c str prio isKey) "" =
  if str /= "" 
    then Node {children = c, str = str, prio = prio + 1, isKey = True}
    else Node c str prio isKey
insertString (Node c str' prio isKey) (s:ss) =
  Node (para f [insertString node ss] c) str' p isKey 
    where 
      f n ns acc = if str n == [toLower s] 
        then insertString n ss : ns 
        else n : acc
      node = Node [] [toLower s] 0 False
      p = if str' == "" 
        then prio 
        else prio + 1

{-
search n s
Searches String s in Trie with root Node n and returns True if the String is in the Trie, False otherwise.
Search is case-insensitive.
 -}
search :: TrieNode -> String -> Bool
search (Node _ _ _ isKey) "" = isKey
search (Node [] _ _ _) _ = False
search (Node ns str' _ _) (s:ss) = 
  case findNode [toLower s] ns of
    Just n -> search n ss
    Nothing -> False

{-
getHighestPrio n s
Returns the string with the highest priority in the Trie with root n given the prefix s.
If a string does not exist with the prefix s, return Nothing.
-}
getHighestPrio :: TrieNode -> String -> Maybe String
getHighestPrio n s = aux n "" (toLower <$> s) 
  where
    aux (Node [] _ _ isKey) b _ = if isKey then Just b else Nothing
    aux (Node ns _ _ _) b (s:ss) = 
      case findNode [s] ns of
        Just n -> aux n (b ++ [s]) ss
        Nothing -> Nothing
    aux (Node ns _ _ isKey) b "" = 
      if isKey 
        then Just b 
        else case maxPrio ns of
          Just n -> aux n (b ++ str n) ""
          Nothing -> Nothing
