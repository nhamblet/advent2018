module Day2Part2 where

import System.Environment (getArgs)


-- get all the pairs of things in a list
-- note that this assumes in the input list doesn't contain duplicates
pairs :: Eq a => [a] -> [(a,a)]
pairs xs = [(a,b) | a <- xs , b <- xs , a /= b]


-- given a list of strings, find the pair that are only off by one character
offByOnePair :: [String] -> (String, String)
offByOnePair xs =
  head $ filter ffunc $ pairs xs
  where ffunc :: (String, String) -> Bool
        ffunc (a,b) = (length a) - 1 == length (commonChars a b)


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

