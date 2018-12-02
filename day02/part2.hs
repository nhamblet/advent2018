module Day2Part2 where

import System.Environment (getArgs)


-- get all the pairs of things in a list
-- note that this assumes in the input list doesn't contain duplicates
pairs :: Eq a => [a] -> [(a,a)]
pairs xs = [(a,b) | a <- xs , b <- xs , a /= b]


-- given two strings (assumed to be of the same length),
-- how many characters are they different by?
charactersDifferent :: String -> String -> Int
charactersDifferent [] [] = 0
charactersDifferent (a:as) (b:bs)
  | a == b    = charactersDifferent as bs
  | otherwise = 1 + (charactersDifferent as bs)


-- given a list of strings, find the pair that are only off by one character
offByOnePair :: [String] -> (String, String)
offByOnePair xs =
  head $ filter (\p -> 1 == (charactersDifferent (fst p) (snd p))) $ pairs xs


-- given two strings, find the common characters
commonChars :: String -> String -> String
commonChars [] [] = []
commonChars (a:as) (b:bs)
  | a == b    = a : (commonChars as bs)
  | otherwise = commonChars as bs


-- given a list of strings, find the common characters between the two
-- strings that only differ by one character
getFinalAnswer :: [String] -> String
getFinalAnswer xs = commonChars (fst ans) (snd ans)
  where ans = offByOnePair xs


-- and be able to feed in the file
main = do
  args <- getArgs
  content <- readFile $ args !! 0
  putStrLn $ getFinalAnswer $ lines content

