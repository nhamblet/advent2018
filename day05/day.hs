module Day5 where

import Data.Char (toUpper, isUpper)
import Control.Parallel.Strategies
import Control.Parallel

-- Part 1

-- in this problem, we collapse a string of [a-zA-Z] characters by
-- removing any characters that are next to each other in the string,
-- the same letter, and different case.


-- remove two elements from a list, given an index
removeNeighbors :: Int -> [a] -> [a]
removeNeighbors i as = left ++ right
  where
    (left, right') = splitAt i as
    right          = drop 2 right'


-- how to determine if two characters anihilate each other
areInverses :: Char -> Char -> Bool
areInverses c c' = toUpper c == toUpper c' && (isUpper c && not (isUpper c') || isUpper c' && not (isUpper c))


-- find the index of the first pair of characters that should be removed,
-- where the index is greater than the given index
--
-- this is a helper function for firstInverse, below
firstInverse' :: String -> Int -> Maybe Int
firstInverse' [] _ = Nothing
firstInverse' [c] _ = Nothing
firstInverse' (c:c':cs) i = if areInverses c c' then Just i else firstInverse' (c':cs) (i+1)

-- find the index of the first pair of characters to remove
firstInverse :: String -> Maybe Int
firstInverse s = firstInverse' s 0

-- get rid of all of the neighboring characters that anihilate each other
trimNegatives :: String -> String
trimNegatives s =
  case firstInverse s of
    Just i -> trimNegatives $ removeNeighbors i s
    Nothing -> s


-- the final answer is given by the length of the reduced string
part1 :: String -> Int
part1 = length . trimNegatives



-- Part 2

-- now we first remove all of a characters, anihilate neighbors from that starting point,
-- and want to find the length of the shortest anihilated string, across all the letters
-- we remove in the first step


-- remove all of one character from a string
dropAll :: Char -> String -> String
dropAll c = filter (not . ((==(toUpper c)) . toUpper))


-- find the minimum length after anihilating neighbors in strings with the individual characters removed
shortestLength :: String -> Int
shortestLength s = minimum [length $ trimNegatives $ dropAll c s | c <- "abcdefghijklmnopqrstuvwxyz"]

-- while i waited for the above to run, i tried a line that was supposed to allow for some
-- parallelization, but i'm not convinced it worked
psl :: String -> Int
psl s = minimum $ parMap rseq (\c -> length $ trimNegatives $ dropAll c s) "abcdefghijklmnopqrstuvwxyz"


-- while i was waiting for the above to run (anticipating ~2 minutes per letter, 26 letters... 1 hour)
-- i realized my trimNegatives was starting over from the beginning of the string a lot more
-- than it needed to be. i also remembered the notion of a 'zipper', like a list with a hole
-- punched in it, which can be represented by two lists - the elements on the left of the
-- hole (with the head of the list being the element directly to the left) and the elements
-- to the right of the hole. since we anihilate characters on either side of such a hole,
-- we can do everything with the heads of lists, which should be faster


-- a "String Zipper" is a pair of strings
data SZ = SZ String String deriving (Eq, Show)

-- you start a zipper from an input string by putting the hole at the front of the string
mksz :: String -> SZ
mksz s = SZ "" s

-- you also want to be able to get rid of the hole when you're done
fromSZ :: SZ -> String
fromSZ (SZ l r) = (reverse l) ++ r

-- the following 'mv' and 'has' functions are utilities for iterating through the zipper.
-- probably if i actually knew anything this is all in some library

mvRight :: SZ -> SZ
mvRight (SZ l []) = SZ l []
mvRight (SZ l (r:rs)) = SZ (r:l) rs

mvLeft :: SZ -> SZ
mvLeft (SZ [] r) = SZ [] r
mvLeft (SZ (l:ls) r) = SZ ls (l:r)

hasLeft :: SZ -> Bool
hasLeft (SZ [] _) = False
hasLeft (SZ _  _) = True

hasRight :: SZ -> Bool
hasRight (SZ _ []) = False
hasRight (SZ _ _ ) = True


-- now to trim a string represented by a zipper, we either eliminate the heads on the two
-- sides if they cancel each other, or we move the hole to the next spot in the list.
-- both of these are easy operations with cons lists.
-- indeed, this runs in ~0.15 seconds for my input string.
trimNegativesSZ :: SZ -> SZ
trimNegativesSZ sz@(SZ [] []) = sz
trimNegativesSZ sz@(SZ _  []) = sz
trimNegativesSZ sz@(SZ [] r) = trimNegativesSZ $ mvRight sz
trimNegativesSZ sz@(SZ (l:ls) (r:rs))
  | areInverses l r = trimNegativesSZ $ mvLeft $ SZ ls rs
  | otherwise       = trimNegativesSZ $ mvRight sz

-- for part 2:
shortestLengthSZ :: String -> Int
shortestLengthSZ s = minimum [length $ fromSZ $ trimNegativesSZ $ mksz $ dropAll c s | c <- "abcdefghijklmnopqrstuvwxyz"]

