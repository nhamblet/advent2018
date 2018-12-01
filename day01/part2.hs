module ChronalCalibrationPart2 where

import System.Environment
import Data.Set (Set, member, insert, empty)

-- Given a list of integers, we're going to infinitely cycle through it
-- and take the cumulative sums. At some point, the cumulative sum will
-- reach a value it has hit before, and we want to know that value.


-- Given a list of differences, create the list of cumulative sums,
-- as an infinite list, repeating the input list as necessary.
cumulativeSums :: [Int] -> [Int]
cumulativeSums xs = 0 : (scanl1 (+) $ concat $ repeat xs) -- we stick a 0 on the front of the list to kick things off

-- Given a list of cumulative sums, step through the list until you find
-- the first duplicated value, and return that value
firstDupe :: [Int] -> Maybe Int
firstDupe xs = go xs empty
  where go :: [Int] -> Set Int -> Maybe Int
        go (x:xs) soFar
           | member x soFar = Just x
           | otherwise      = go xs (insert x soFar)
        go [] _ = Nothing -- we shouldn't hit this, if we pass in infinite lists with a solution, but just in case


-- the given input files represent positive values with an explicit "+" in front
-- which haskell's `read` doesn't seem to like much. So just pull that character out
-- if it's there
parseDiff :: String -> Int -- allows for runtime exception
parseDiff str
  | str !! 0 /= '+' = read str
  | otherwise       = read (drop 1 str)


-- given a file of diffs, parse it into the list of ints
diffsFromFile :: String -> IO [Int]
diffsFromFile fname = do
  content <- readFile fname
  return $ fmap parseDiff $ lines content


-- we take the first command line argument as the path to the file of diffs,
-- read that file, and find the first duplicate value in the cumulative sums.
main = do
  args <- getArgs
  diffs <- diffsFromFile $ args !! 0
  putStrLn $ show $ firstDupe $ cumulativeSums diffs

