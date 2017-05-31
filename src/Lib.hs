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
compress [] = []
compress (x:xs) = compressHelp xs x
  where
    compressHelp :: Eq a => [a] -> a -> [a]
    compressHelp a e
      | null a = [e]
      | e == head a = compressHelp (tail a) (head a)
      | otherwise = e : compressHelp (tail a) (head a)

-- Problem 9
-- pack consecutive duplicates of list elements into sublists
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = packHelp xs [x]
  where
    packHelp :: Eq a => [a] -> [a] -> [[a]]
    packHelp list run
      | null list = [run]
      | head list == head run = packHelp (tail list) (head list:run)
      | otherwise = run:packHelp (tail list) [head list]

-- Problem 10
-- record run length of consecutive elements in a list
encode :: Eq a => [a] -> [(Int, a)]
encode x = let p = pack x in zip (map length p) (map head p)

-- Problem 11
-- encode but do not pair elements without consecutive duplicates with
-- their corresponding '1'
-- e.g. encode "aaessd" = [(2, a), e, (2,s), d]
data Encoding a = Multiple Int a | Single a
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = encodeModifiedHelp . encode
  where
    encodeModifiedHelp :: Eq a => [(Int, a)] -> [Encoding a]
    encodeModifiedHelp [] = []
    encodeModifiedHelp (x:xs) =
        if fst x > 1
          then uncurry Multiple x:encodeModifiedHelp xs
          else Single (snd x):encodeModifiedHelp xs

-- Problem 12
-- Decode a run-length encoded list
decodeOnce :: Encoding a -> [a]
decodeOnce (Single x) = [x]
decodeOnce (Multiple 1 x) = [x]
decodeOnce (Multiple n x) = x:decodeOnce (Multiple (n-1) x)

decodeModified :: [Encoding a] -> [a]
decodeModified x = foldr (++) [] (map decodeOnce x)

-- Problem 13
-- Run length encoding of a list directly,
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect [x] = encodeDirectHelp [] x

encodeDirectHelp :: Eq a => [Encoding a] -> a -> [Encoding a]
encodeDirectHelp [] c = [(Single c)]
encodeDirectHelp [(Single c1)] c2 =
  if c1 == c2
  then [(Multiple 2 c1)]
  else encodeDirectHelp [] c2
encodeDirectHelp [(Multiple n c1)] c2 =
  if c1 == c2
  then [(Multiple (n + 1) c2)]
  else encodeDirectHelp [] c2
encodeDirectHelp (x:xs) c2 = x:encodeDirectHelp xs c2
