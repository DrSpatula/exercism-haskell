module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

data LinkedList a = Element a (LinkedList a) | Nil deriving (Eq, Show)

datum :: LinkedList a -> a
datum (Element x _) = x

fromList :: [a] -> LinkedList a
fromList [] = Nil
fromList (x:xs) = Element x (fromList xs)

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _   = False

new :: a -> LinkedList a -> LinkedList a
new x xs = Element x xs

next :: LinkedList a -> LinkedList a
next (Element _ nxt) = nxt

nil :: LinkedList a
nil = Nil

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList Nil = []
toList (Element x nxt) = x : toList nxt
