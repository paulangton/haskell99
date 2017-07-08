module Lib (
      myLast,
      myButLast,
      elementAt,
      myLength,
      myReverse,
      isPalindrome,
      randomN
    ) where

import System.Random
import Data.List

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
encodeDirect x = encodeDirectAcc x []

encodeDirectAcc :: Eq a => [a] -> [Encoding a] -> [Encoding a]
encodeDirectAcc [] acc = acc
encodeDirectAcc (x:xs) acc = encodeDirectAcc xs (init acc) ++ encodeDirectHelp (last acc) x

encodeDirectHelp :: Eq a => Encoding a -> a -> [Encoding a]
encodeDirectHelp (Single c) n =
  if c == n
  then [Multiple 2 c]
  else [Single c, Single n]
encodeDirectHelp (Multiple n c1) c2 =
  if c1 == c2
  then [Multiple (n + 1) c1]
  else [Multiple n c1,Single c2]

-- Problem 14
-- Duplicate the elements of a list
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = [x,x] ++ duplicate xs

-- Problem 15
-- Replicate elements in a list a given number of times
replicateN :: Eq a => [a] -> Int -> [a]
replicateN (x:xs) num = replicateNOnce x num ++ replicateN xs num
  where
    replicateNOnce :: a -> Int -> [a]
    replicateNOnce x 0 = []
    replicateNOnce x num = x:replicateNOnce x (num - 1)

-- Problem 16
-- Drop every Nth element of a list
dropEveryN :: [a] -> Int -> [a]
dropEveryN x num = dropEveryNHelp x num num
  where
    dropEveryNHelp :: [a] -> Int -> Int -> [a]
    dropEveryNHelp [] _ _ = []
    dropEveryNHelp (x:xs) num 1 = dropEveryNHelp xs num num
    dropEveryNHelp (x:xs) num cur = x:dropEveryNHelp xs num (cur - 1)

-- Problem 17
-- Split a list at position n
splitAtN :: [a] -> Int -> [[a]]
splitAtN l pos = splitAtNHelp l pos []
  where
    splitAtNHelp :: [a] -> Int -> [a] -> [[a]]
    splitAtNHelp l pos acc
      | null l = [acc]
      | pos == 0 = acc:splitAtNHelp l (-1) []
      | otherwise = splitAtNHelp (tail l) (pos - 1) (acc ++ [head l])

-- Problem 18
-- Extract a slice from a list starting at index i and ending at index k (1-indexed)
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice l 0 e = head $ splitAtN l e
slice (x:xs) b e = slice xs (b-1) (e-1)

-- Problem 19
-- Rotate a list n places to the left
rotateLeft :: [a] -> Int -> [a]
rotateLeft l num = let
  half1 = slice l 0 num
  half2 = slice l num (length l)
  in half2 ++ half1

-- Problem 20
-- Remove the K'th element in a list
remove :: [a] -> Int -> (a, [a])
remove l num = let
  e = elementAt num l
  half1 = slice l 0 (num-1)
  half2 = slice l num (length l)
  in (e, half1 ++ half2)

-- Problem 21
-- Insert an element at a given position into a list
insertAt :: [a] -> Int -> a -> [a]
insertAt l pos e = let
  half1 = slice l 0 pos
  half2 = slice l pos (length l)
  in half1 ++ e:half2

-- Problem 22
-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range lo hi
  | lo == hi = [lo]
  | lo > hi = error "first number must be less than second"
  | otherwise = let newLo = lo + 1 in lo:range newLo hi

-- Problem 23
-- Extract a given number of randomly selected elements from the list (repeats allowed)
randomN :: [a] -> Int -> IO [a]
randomN l num = do
  x <- randomRIO (0, length l - 1)
  rest <- randomN l (num - 1)
  return ((l !! x):rest)

-- Problem 24
-- Draw n different random numbers from the set 1..m
randNUnique :: Int -> Int -> IO [Int]
randNUnique n m = randNUniqueHelp n [1..m] where
  randNUniqueHelp :: Int -> [Int] -> IO [Int]
  randNUniqueHelp l num = do
    x <- randomRIO (0, length l - 1)
    rest <- randNUniqueHelp (remove x l) (num - 1)
    return ((l !! x):rest)

-- Problem 25
-- Generate a random permutation of the elements of a list
rPermute :: [a] -> IO [a]
rPermute l = do
  x <- randomRIO (0, length l - 1)
  rest <- rPermute (remove x l)
  return (l !! x):rest

-- Problem 26
-- Generate the combinations of K distinct objects chosen from the N elements of a list
combinations :: Int -> [a] -> [[a]]
combinations _ [] = []
combinations k (x:xs) = (containsXCombs k x:xs []) ++ combinations k xs
  where containsXCombs :: Int -> [a] -> [[a]]
    containsXCombs 1 l = map [] l
    containsXCombs n (y:ys) = map (y:) containsXCombs n-1 ys

-- Problem 27 (counts for 2)
-- a) Write a function that enumerates all possible ways to split a group of 9 people into 3 disjoint subsets of 2, 3, and 4 people
-- function that takes a 3 deep array, a number, and a list, and adds
genSets :: Int -> [a] -> [[a]] -- Generates all possible sets of n (order doesnt matter) from a list
genSets n [] = []
genSets 1 a = map (:[]) a
genSets n (x:xs) =  (map (x:) (genSets (n-1) xs)) ++  genSets n xs

group3Help :: Int -> [[a]] -> [[[a]]]
group3Help _ [] = []
group3Help n (x:xs) = let toAdd = genSets n (x \\ l) in (map (x++) toAdd) ++ group3Help n xs

group3 :: [a] -> [[[a]]]
group3 l = map group3Help 4 (group3Help 3 (genSets 2 l))


-- b) generalize the above function to take a list of group sizes
group :: [a] -> [Int] -> [[[a]]]
group l (x:xs) = (group3Help (head xs) (genSets x l))

groupHelp :: [a] -> Int -> [[[a]]] -> [[[a]]]
groupHelp _ [] acc = acc
groupHelp l [x] acc = (group3Help x (genSets x l))
groupHelp l (x:xs) acc = groupHelp l xs (map group3Help x acc)

-- Problem 29 (counts for 2)
-- Sort a list of lists based on their length
-- a) sort shorter lists -> longer lists
lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = linsert x $ lsort xs
  where
   linsert :: [a] -> [[a]] -> [[a]]
   linsert e [] = [e]
   linsert e (y:ys) = if length e > length y then y:linsert e ys else e:y:ys

-- b) Sort the elements of this list of lists according to their length
-- frequency; i.e., in the default, where sorting is done ascendingly, lists with rare
-- lengths are placed first, others with a more frequent length come later.
lfsort :: [[a]] -> [[a]]
lfsort x = lsort (groupBy lenTest x)
  where
    lenTest :: [a] -> [a] -> Bool
    lenTest x y = length x == length y

-- Problem 31
-- Determine whether a given integer number is prime
isPrime :: Int -> Bool
isPrime x = length (filter (\a -> if x == 2 then True else (mod x a) == 0) [2..ceiling $ sqrt $ fromIntegral x]) == 0

-- Problem 32
-- Determine the GCD between two positive integers
myGcd :: Int -> Int -> Int
myGcd x y | x == y = x
          | ((isPrime x) && (isPrime y)) = 1
          | x > y = myGcd (x-y) y
          | x < y = myGcd x (y-x)

-- Problem 33
-- Determine whether two positive integer numbers are coprime
coprime :: Int -> Int -> Bool
coprime x y = (myGcd x y) == 1

-- Problem 34
-- Calculate Euler's totient function, defined as the number of positive
-- integers r (1 <= r < m) that are coprime to m.
totient :: Int -> Int
totient m = length (filter (coprime m) [1..m])

-- Problem 35
-- Determine the prime factors of a given positive integer. Construct a flat
-- list containing the prime factors in ascending order.
lowDivisor :: Int -> Int
lowDivisoer

primeFactors :: Int -> [Int]
primeFactors m
| isPrime(m) = m
| otherwise = let divisor = lowDivisor m in divisor:primeFactors(m / divisor)
