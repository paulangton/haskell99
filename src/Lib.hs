module Lib (
      myLast,
      myButLast,
      elementAt,
      myLength,
      myReverse,
      isPalindrome
    ) where

-- Problem 1
-- Find the last element of a list
myLast :: [a] -> a
myLast [] = error "No end for empty lists!"
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
-- Find the second to last element of a list
myButLast :: [a] -> a
myButLast [] = error "No but last of empty list"
myButLast [a] = error "No but last of a one-element list"
myButLast (x:xs) = if length xs == 1
  then x
  else myButLast xs

-- Problem 3
-- Find the K'th element in a list
elementAt :: Int -> [a] -> a
elementAt _ [] = error "no k'th element"
elementAt 1 (x:_) = x
elementAt k (y:ys) = elementAt (k-1) ys

-- Problem 4
-- Find the number of elements in a list
myLength :: [a] -> Int
myLength list
  | null list = 0
  | otherwise = 1 + myLength (tail list)

-- Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome l = isPalindrome (tail (init l))
  && (head l == last l)

-- Problem 7
-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []

-- Problem 8
-- Remove consecutive duplicates of list elements
compress :: Eq a => [a] -> [a]
compress a = compressHelp (tail a) (head a)
  where
    compressHelp :: Eq a => [a] -> a -> [a]
    compressHelp a e
      | null a = [e]
      | e == head a = compressHelp (tail a) (head a)
      | otherwise = e : compressHelp (tail a) (head a)
