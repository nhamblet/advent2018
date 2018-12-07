module Day where

import Text.Regex (matchRegex, mkRegex)
import qualified Data.Map as M (Map, empty, insert, findWithDefault, toList)
import Data.List.Unique (count)
import Data.List (sortBy, minimumBy, maximumBy)
import qualified Data.Set as S (Set, empty, fromList, member)
import Data.Maybe (catMaybes)
import GHC.Exts (sortWith)

-- in this problem we're dealing with manhattan distances from grid cells
-- to a fixed set of target points. my given input only had 50 target points,
-- all with coordinates not more than about 350, so i decided i might get
-- away with brute force, which seemed to work ok.


-- part 1
-- find the voronoi regions based on the target points, and find the size of
-- the largest region. note that points an equal distance to two targets isn't
-- considered to be in a region, and we eliminate infinite regions


-- convert the input file
lineRegex = mkRegex "([0-9]+), ([0-9]+)"
getCoords :: String -> (Int, Int)
getCoords s = case matchRegex lineRegex s of
  Just [col, row] -> (read col, read row)


-- set up the sample and test inputs
sampleInput = [(1,1),(1,6),(8,3),(3,4),(5,5),(8,9)] :: [(Int, Int)]
readSampleInput = (fmap.fmap) getCoords $ fmap lines (readFile "input1")
allInput = (fmap.fmap) getCoords $ fmap lines (readFile "/tmp/6.1")


-- find the bounding box of the targets
maxCoords :: [(Int, Int)] -> (Int, Int)
maxCoords ps = (mc, mr)
  where
    mc = maximum $ fmap fst ps
    mr = maximum $ fmap snd ps

-- distance function
manhattanDist :: (Int, Int) -> (Int, Int) -> Int
manhattanDist (c,r) (c',r') = abs(c'-c) + abs(r'-r)

-- given the targets, and a coordinate, what target is the closest,
-- if there is one that is closest (i.e., no ties)
closestPoint :: [(Int, Int)] -> (Int, Int) -> Maybe (Int, Int)
closestPoint ps p = winner $ sortWith snd [(p', manhattanDist p p') | p' <- ps]
  where
    winner :: [((Int, Int), Int)] -> Maybe (Int, Int)
    -- given all the distances, there's only a winner if the best distance
    -- only occurs once
    winner pds = case snd h == snd h' of
      True  -> Nothing
      False -> Just $ fst h
      where [h,h'] = take 2 pds

-- tally up how often each target is the closest point for a point in the grid
countedAssignments :: [(Int, Int)] -> [((Int, Int), Int)]
countedAssignments = count . allLabelAssignments
  where
    -- given a list of targets, compute the target for all cells in the containing
    -- region, and just return the list of targets
    allLabelAssignments :: [(Int, Int)] -> [(Int,Int)]
    allLabelAssignments ps = catMaybes [closestPoint ps (c,r) | c <- [0..mc], r <- [0..mr]]
      where (mc, mr) = maxCoords ps


-- build the set of all targets who have a point on the boundary that is closest
-- to that target. these become the infinite regions, and we want to ignore them
markInfinite :: [(Int, Int)] -> S.Set (Int,Int)
markInfinite ps = S.fromList $ catMaybes $ [closestPoint ps (c,r) | c <- [0,mc], r <- [0..mr]] ++ [closestPoint ps (c,r) | c <- [0..mc], r <- [0,mr]]
  where (mc, mr) = maxCoords ps

-- filter the counted assignments to those that aren't infinite
finiteBlobs :: S.Set (Int, Int) -> [((Int, Int), Int)] -> [((Int, Int), Int)]
finiteBlobs bad = filter (not . (((flip S.member) bad) . fst))

-- and then pick out the biggest blob
largestFiniteBlob :: [((Int, Int), Int)] -> ((Int, Int), Int)
largestFiniteBlob = head . reverse . sortWith snd

-- now string together all of the above!
part1 :: [(Int,Int)] -> Int
part1 coords = snd $ largestFiniteBlob $ finiteBlobs infs $ countedAssignments coords
  where
    infs = markInfinite coords


-- part 2
-- now we want to find all the cells whose total distance to all the targets is
-- less than a given threshold

-- a cell isn't safe enough if the sum of its distances to the targets is past the threshold
neverTooFar :: Int -> [(Int, Int)] -> (Int, Int) -> Bool
neverTooFar threshold ps p = threshold > (sum $ fmap (manhattanDist p) ps)

-- find the number of cells that are safe enough
part2 :: Int -> [(Int, Int)] -> Int
part2 t ps = length [() | c <- [0..mc], r <- [0..mr], neverTooFar t ps (c,r)]
  where (mc, mr) = maxCoords ps

