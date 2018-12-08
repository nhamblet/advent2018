module Day where

import Data.Maybe
import Debug.Trace

-- no real parsing needed today, which is fine with me
readInput :: String -> [Int]
readInput = (fmap read) . words

-- the default type, in ghci anyway, seems to make this [Integer],
-- which upset some of my usages in later methods
sampleInput = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2] :: [Int]
realInput = do
  content <- readFile "/tmp/8.1"
  return $ readInput content


-- given the metadata identified so far, and the remaining codes from
-- the serialized tree, find the metadata for the current tree, and any
-- remaining codes after that tree is pulled off the list
metadata :: [Int] -> [Int] -> ([Int], [Int])
metadata s [] = (s, []) -- if we're out of input, we must have all the metadata
metadata s (c:m:t) = (ans, left)
  where
    (ans, left) = popChildren [] c t -- we have to pull c children off first
    popChildren s 0 t = (s ++ take m t, drop m t) -- when there's no more children to pull off,
                                                  -- we're at the metadata section of a node
    popChildren s n t = popChildren s' (n-1) ct
      where
        (cs,ct) = metadata s t
        s' = s ++ cs


-- the final answer in part 1 is the sum of all the metadata values of all the nodes
part1 :: [Int] -> Int
part1 xs = sum $ fst $ metadata [] xs


-- in part 2 it seems like it'll be handy to have the tree structure as a
-- tree, so we first re-parse into that structure, after which evaluating a node
-- is relatively easy

type Metadata = [Int]
data TreeNode = TreeNode [TreeNode] Metadata deriving Show

-- this logic is basically the same as `metadata` above
readTree :: [Int] -> (TreeNode, [Int])
readTree codes = (TreeNode children meta, remNodes'')
  where
    numChildren = head codes -- see how many children the node has
    numMeta     = head (tail codes) -- and how many metadata
    remCodes    = drop 2 codes -- and get ride of those inputs
    (children, remNodes') = go [] numChildren remCodes -- then recursively get all the children
    meta        = take numMeta remNodes' -- and pull off the metadata for the node
    remNodes''  = drop numMeta remNodes' -- and the finally remaining inputs

    -- go takes a list of already identified children, a number of remaining children
    -- to find, and the list of input codes, and return the list of `TreeNode`s for
    -- the children, as well as any remaining input codes
    go kids 0 codes = (reverse kids, codes)
    go kids numKidsLeft codes = go (newKid:kids) (numKidsLeft-1) leftoverCodes
      where
        (newKid, leftoverCodes) = readTree codes


-- the value of a tree is the sum of its metadata, if it has no children.
-- if it has children, the metadata values are taken as 1-indexed references to the children,
-- and the value of the node is the sum of the referenced values, for any metadata that
-- refers to a valid child index
evaluateTree :: TreeNode -> Int
evaluateTree (TreeNode [] meta) = sum meta
evaluateTree (TreeNode children meta) = sum $ catMaybes $ fmap childValue meta
  where
    childValue :: Int -> Maybe Int
    childValue i = case ((i > 0) && (i <= length children)) of
                     True  -> Just $ evaluateTree $ children !! (i-1)
                     False -> Nothing

