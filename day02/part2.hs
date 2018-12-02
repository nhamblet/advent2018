module Day2Part2 where

import System.Environment (getArgs)

pairs :: Eq a => [a] -> [(a,a)]
pairs xs = [(a,b) | a <- xs , b <- xs , a /= b]

charactersDifferent :: String -> String -> Int
charactersDifferent [] [] = 0
charactersDifferent (a:as) (b:bs)
  | a == b    = charactersDifferent as bs
  | otherwise = 1 + (charactersDifferent as bs)


-- find off-by-one error among set of strings
offByOnePair :: [String] -> (String, String)
offByOnePair xs =
  head $ filter (\p -> 1 == (charactersDifferent (fst p) (snd p))) $ pairs xs


commonChars :: String -> String -> String
commonChars [] [] = []
commonChars (a:as) (b:bs)
  | a == b    = a : (commonChars as bs)
  | otherwise = commonChars as bs


getFinalAnswer :: [String] -> String
getFinalAnswer xs = commonChars (fst ans) (snd ans)
  where ans = offByOnePair xs


-- and be able to feed in the file
main = do
  args <- getArgs
  content <- readFile $ args !! 0
  putStrLn $ getFinalAnswer $ lines content

