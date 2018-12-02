module Day2Part1 where

import System.Environment (getArgs)
import Data.List.Unique (count) -- from Unique

-- given a bunch of strings, we want to find those strings
-- which contains 2 of any letter. we also want to find those
-- strings which have 3 of any letter. we then multiply
-- the number of strings which have 2 by the numbers of strings
-- which have 3, as our checksum.


-- given a string, does it count toward the 'duplicate' or 'triples'?
countsToward :: String -> (Bool, Bool)
countsToward xs =
  let xc        = count xs -- count, from Unique, has type [a] -> [(a, Int)]
      hasDupe   = any ((==2) . snd) xc
      hasTriple = any ((==3) . snd) xc
  in
    (hasDupe, hasTriple)



-- now given a list of strings, get the number of hasDupe and hasTriple
-- and multiply those numbers together
checksum :: [String] -> Int
checksum xs = numDupe * numTriple
  where
    xcs       = fmap countsToward xs
    numDupe   = length $ filter fst xcs
    numTriple = length $ filter snd xcs



-- and be able to feed in the file
main = do
  args <- getArgs
  content <- readFile $ args !! 0
  putStrLn $ show $ checksum $ lines content

