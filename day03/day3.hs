module Day3 where

import System.Environment
import Data.Matrix -- my final solution didn't use this, but some of the wrong ones below do
import Text.Regex -- someday I'll learn other methods
import Data.Maybe (catMaybes)
import Debug.Trace
import Data.List.Unique (count)
import qualified Data.Map as M (Map, fromList, lookup, findWithDefault)


-- this sets up our data types. it ends up a little clunky, but not too bad
data Offset = Offset Int Int deriving (Eq, Show)
data Size = Size Int Int deriving (Eq, Show)
data Claim = Claim String Offset Size deriving (Eq, Show)


parseClaim :: String -> Maybe Claim
parseClaim s = case matchRegex (mkRegex "#([^\\ ]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)") s of
  Just [i, l, t, w, h] -> Just $ Claim i (Offset (read l) (read t)) (Size (read w) (read h))
  Nothing                             -> Nothing


--- Attempt 1, process died


-- Given a starting matrix, whose values are the counts of overlapping
-- claims at the cells, update the matrix by adding a new claim
-- note that if the claim ends up extending the bounds of the matrix,
-- the resulting matrix will have a larger size than the input
updateClaims :: Matrix Int -> Claim -> Matrix Int
updateClaims m claim@(Claim cid (Offset left top) (Size width height)) =
  matrix outRows outCols outFiller
  where
    outRows = max (nrows m) (top + height + 1)
    outCols = max (ncols m) (left + width + 1)
    outFiller :: (Int, Int) -> Int
    outFiller (row, col) = (safeElem m row col) + (if countsToward row col claim then 1 else 0)


-- 0-pad a matrix
safeElem :: Matrix Int -> Int -> Int -> Int
safeElem m r c
  | r > 0 && r <= nrows m && c > 0 && c <= ncols m = m ! (r, c)
  | otherwise                                      = 0


-- a helper method to make sure we 1-index consistently
countsToward :: Int -> Int -> Claim -> Bool
countsToward row col (Claim cid (Offset left top) (Size width height)) =
  row >= (top + 1) &&
    row <= (top + height) &&
    col >= (left + 1) &&
    col <= (left + width)


-- hard-coded values from the given sample input
exampleBase  = zero 5 5
exampleOne   = updateClaims exampleBase (Claim "1" (Offset 1 3) (Size 4 4))
exampleTwo   = updateClaims exampleOne  (Claim "2" (Offset 3 1) (Size 4 4))
exampleThree = updateClaims exampleTwo  (Claim "3" (Offset 5 5) (Size 2 2))

exampleClaims = [
  (Claim "1" (Offset 1 3) (Size 4 4)),
  (Claim "2" (Offset 3 1) (Size 4 4)),
  (Claim "3" (Offset 5 5) (Size 2 2))
  ]


-- after creating the whole matrix, find the count of the cells with overlap
numConflicts :: Matrix Int -> Int
numConflicts m = length $ filter (>1) (toList m)


-- aggregate up all the claims into a final matrix of overlap counts
foldClaims :: [Claim] -> Matrix Int
foldClaims cs = foldl updateClaims (zero 5 5) cs


-- this gets killed, on my laptop, after 10 minutes of churning and 6G memory
mainBad = do
  args <- getArgs
  content <- readFile $ args !! 0
  putStrLn $ show $ numConflicts $ foldClaims $ catMaybes $ fmap parseClaim (lines content)


-- Attempt 2, ended up giving the wrong answer


-- there's ~1000 claims in my input, so just loop through them once to determine
-- the matrix size up front, instead of growing it as needed in the previous setup
maxDims :: [Claim] -> (Int, Int)
maxDims = foldl incr (0,0)
  where incr (fh,fw) (Claim _ (Offset l t) (Size w h)) = (max fh (t+h+1), max fw (l+w+1))


-- given a target number of claims, and the list of claims, and a coordinate,
-- do the claims hit that coordinate at least that many times
overlapsAtLeast :: Int -> [Claim] -> (Int, Int) -> Bool
overlapsAtLeast _ [] _ = False
overlapsAtLeast 0 _  (row,col) = True
overlapsAtLeast n (claim@(Claim _ (Offset l t) (Size w h)):cs) (row,col)
  | countsToward row col claim = overlapsAtLeast (n-1) cs (row, col)
  | otherwise                  = overlapsAtLeast n cs (row, col)


-- loop through all coordinates in containing matrix of the claims to find
-- all those coordinates with at least 2 overlapping claims
countOverlaps :: [Claim] -> Int
countOverlaps cs = length [() | r <- [1..nr], c <- [1..nc], overlapsAtLeast 2 cs (r,c)]
  where (nr, nc) = maxDims cs


finalAnswer2 :: [String] -> Int
finalAnswer2 ls = countOverlaps $ toClaims ls


toClaims :: [String] -> [Claim]
toClaims ss = catMaybes $ fmap parseClaim ss


-- answer is low, but took 20 minutes to compute anyway
-- answer was 104621
mainWrongSlow = do
  args <- getArgs
  content <- readFile $ args !! 0
  putStrLn $ show $ finalAnswer2 (lines content)


-- Attempt 3, slow, but at least it is correct

main3 = do
  args <- getArgs
  content <- readFile $ args !! 0
  putStrLn $ show $ finalAnswer3 (lines content)

finalAnswer3 :: [String] -> Int
finalAnswer3 ls = rollup $ toClaims ls


-- the idea here is to build up the matrix of touched coordinates while
-- we also build up the count of coordinates touched more than once.
-- by the time we're done looping through the claims, we have our final
-- count of conflicting cells.
rollup :: [Claim] -> Int
rollup cs = snd $ foldl go (zero nr nc, 0) cs
  where
    (nr, nc) = maxDims cs
    go :: (Matrix Int, Int) -> Claim -> (Matrix Int, Int)
    go (mIn, ansIn) claim@(Claim _ (Offset l t) (Size w h)) = (traceShow $ (show claim) ++ "\n" ++ (show ansIn)) $ foldl paint (mIn, ansIn) (indices claim)
      where
        paint :: (Matrix Int, Int) -> (Int, Int) -> (Matrix Int, Int)
        paint (m,a) (r,c) = (if e < 2 then setElem (1+e) (r,c) m else m, a + if e == 1 then 1 else 0)
          where e = getElem r c m


indices :: Claim -> [(Int, Int)]
indices (Claim _ (Offset l t) (Size w h)) = [(r,c) | r <- [(t+1)..(t+h)], c <- [(l+1)..(l+w)]]


-- Attempt 4, finally works and isn't terribly slow


-- given a bunch of claims, we just write down the coordinates they hit,
-- and then group by coordinate and count overlaps
tabulateIndices :: [Claim] -> [((Int, Int), Int)]
tabulateIndices claims = count $ concat (fmap indices claims)

-- the final answer is then the number of times the count is great than 1
finalAnswer4 :: [String] -> Int
finalAnswer4 = length . filter (>1) . fmap snd . tabulateIndices . toClaims



-- Part 2

-- find the claim that doesn't intersect any others


-- gather all the hit indices into a map
countMap :: [Claim] -> M.Map (Int, Int) Int
countMap = M.fromList . tabulateIndices

-- we can tell if a claim is isolated, relative to the map, by noting
-- if all of the counts of the cells it his is 1 (itself)
isIsolated :: M.Map (Int, Int) Int -> Claim -> Bool
isIsolated m c = all (==1) $ fmap (\k -> M.findWithDefault 0 k m) (indices c)

findIsolated :: [Claim] -> Claim
findIsolated cs = head $ filter (isIsolated $ countMap cs) cs


main = do
  args <- getArgs
  content <- readFile $ args !! 0
  putStrLn $ show $ finalAnswer4 (lines content)
