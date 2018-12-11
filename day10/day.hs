module Day where

import Text.Regex (mkRegex, matchRegex)


-- relatively simple line to match
numberRegex = "(\\ *\\-?[0-9]+)"
lineRegex = mkRegex $ concat ["position=<", numberRegex, ",", numberRegex, "> velocity=<", numberRegex, ",", numberRegex, ">"]
lineReader :: String -> ((Int, Int), (Int, Int))
lineReader s = case matchRegex lineRegex s of
                 Just [x,y,x',y'] -> ((read x, read y),(read x',read y'))


-- i've found it convenient to have a function that takes a filename
-- and produces the structured data of the problem, wrapped in IO
-- so that I can read a file with the sample input, as well as one with my
-- assigned input.
fileReader :: String -> IO [((Int, Int), (Int, Int))]
fileReader s = do
  content <- readFile s
  return $ fmap lineReader $ lines content

-- here's the two files i just mentioned
sampleInput = fileReader "input1"
problemInput = fileReader "/tmp/10.1"


-- my stopping heuristic is based on identifying the number of neighbors
-- among the points. probably the first local maximum would work, and not require
-- any visual inspection, but i didn't really try.
--
-- note that being neighbors has nothing to do with velocities, so
-- i had originally written this method with just points as in puts.
-- however, i got anxious about fmap-ing `fst` a bunch to pull out the points,
-- so figured it might be quicker to just change the method signature to
-- include parameters it wasn't going to use.
areNeighbors :: ((Int, Int),(Int,Int)) -> ((Int, Int),(Int,Int)) -> Bool
areNeighbors ((a,b),(_,_)) ((c, d),(_,_)) = aside || diagonal
  where
    aside    = abs(a-c) + abs(b-d) == 1 -- up, down, left, right
    diagonal = abs(a-c) == 1 && abs(b-d) == 1 -- left/right one step and up/down one step


-- see if a point has a neighbor among the other points
-- we sorta luck out that a point isn't a neighbor of itself from the definition,
-- although probably the 'first local maximum' stopping criteria would still work.
hasNeighbor :: ((Int, Int),(Int,Int)) -> [((Int, Int),(Int,Int))] -> Bool
hasNeighbor p ps = any (areNeighbors p) ps


-- given the current picture, how many neighbors are there?
-- this count is a little wonky, as it's really saying "how many points have
-- neighbors that come later in the list", but it works as a first pass
numNeighbors :: [((Int, Int),(Int,Int))] -> Int
numNeighbors [] = 0
numNeighbors [p] = 0
numNeighbors (p:ps) = (if hasNeighbor p ps then 1 else 0) + numNeighbors ps


-- advance one point by n steps
stepOneN :: Int -> ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
stepOneN n ((x, y), (x', y')) = ((x + n * x', y + n * y'), (x', y'))

-- i had started with simply 'stepOne', so re-wrote it as a particular case of
-- stepOneN after i decided the other method was useful, but already had code
-- calling a thing of this name with this signature.
stepOne :: ((Int, Int), (Int, Int)) -> ((Int, Int), (Int, Int))
stepOne = stepOneN 1


-- advance the whole picture by n steps
stepAllN :: Int -> [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
stepAllN n = fmap (stepOneN n)

-- again, just a convenience to advance by one step
stepAll :: [((Int, Int), (Int, Int))] -> [((Int, Int), (Int, Int))]
stepAll = stepAllN 1


-- print out the current picture. this could be bad for some of the early
-- pictures, which have on the order of 100k rows or columns (with my
-- assigned input). also note that i didn't bother enough trying to get
-- rows vs columns sorted out, so this prints out transposed backwards.
-- reading it that way was still faster than getting the logic right,
-- and time counts with these puzzles :)
-- there's an oriented converter below
prettyPrint :: [((Int, Int), (Int, Int))] -> [String]
prettyPrint ps = mkPicture (fmap fst ps) minRow maxRow minCol maxCol
  where
    minRow = minimum $ fmap (fst.fst) ps
    maxRow = maximum $ fmap (fst.fst) ps
    minCol = minimum $ fmap (snd.fst) ps
    maxCol = maximum $ fmap (snd.fst) ps
    rows = mkPicture (fmap fst ps) minRow maxRow minCol maxCol


-- helper method to make a picture given the bounding box and just the points
mkPicture :: [(Int, Int)] -> Int -> Int -> Int -> Int -> [String]
mkPicture ps minRow maxRow minCol maxCol = [mkRow n ps minCol maxCol | n <- [minRow..maxRow]]

-- helper method to make a single 'row' of the picture (actually ends up being
-- columns, but whatever)
mkRow :: Int -> [(Int, Int)] -> Int -> Int -> String
mkRow n ps minCol maxCol = row
  where
    cols = fmap snd $ filter ((==n).fst) ps
    row = [if elem c cols then '#' else '.' | c <- [minCol..maxCol]]


-- since our inputs are wrapped in IO, it seems handy to have this helper
-- which takes in such a thing and prints the picture.
ioPrint :: IO [((Int, Int), (Int, Int))] -> IO ()
ioPrint iops = do
  ps <- iops
  mapM_ print $ prettyPrint ps


-- flip the output of prettyPrint, so that it's readable
orientedPrint :: [String] -> [String]
orientedPrint ss = [colString row ss | row <- [0..((length $ head ss)-1)]]
  where
    colString row ss = [s !! row | s <- ss]

-- wrapper to make it that much easier to print a thing that's wrapped in io
ioPrintOriented :: IO [((Int, Int), (Int, Int))] -> IO ()
ioPrintOriented iops = do
  ps <- iops
  mapM_ print $ orientedPrint $ prettyPrint ps


-- this method takes a number of steps, and the starting picture,
-- and returns the number of neighbors for each of the next steps of the picture
getSizes :: Int -> [((Int, Int), (Int, Int))] -> [Int]
getSizes 0 ps = []
getSizes n ps = (numNeighbors ps):(getSizes (n-1) (stepAll ps))


-- i wrote this as a secondary heuristic as i was trying to decide if my
-- solution was working or not. i think finding the minimum total area of
-- the points would probably work as a stopping criteria, as an alternative
-- to the maximum number of neighbors.
dims :: Int -> [((Int, Int), (Int, Int))] -> [(Int,Int)]
dims 0 ps = []
dims n ps = (dim):(dims (n-1) (stepAll ps))
  where
    minRow = minimum $ fmap (fst.fst) ps
    maxRow = maximum $ fmap (fst.fst) ps
    minCol = minimum $ fmap (snd.fst) ps
    maxCol = maximum $ fmap (snd.fst) ps
    dim = (maxRow - minRow, maxCol - minCol)


-- the area covered by the points. when the points are all aligned, we expect
-- the minimum area.
area :: [((Int, Int), (Int, Int))] -> Int
area ps = (uncurry (*)) $ head $ dims 1 ps


-- basically with all the above i ended up just using the repl to get to the answer.
-- perhaps the most helpful, final line i needed (for both parts), was
nearAns = fmap (\ps -> zip (getSizes 1000 ps) [10500..]) $ fmap (stepAllN 10500) problemInput
-- from there, i visually inspected the output, and then called
-- `ioPrint $ fmap (stepAllN 10630) problemInput`, where 10630 was the step with the
-- largest neumber of neighbors.


-- i couldn't help myself as I was writing up my notes for the day,
-- so decided to code up an automated way to find the solution (the moment of the
-- solution, not the message in it). i thought i'd end up letting it run for a
-- few minutes, but actually this finished in 5 seconds.
automatedAnswer :: Int -> Int -> [((Int, Int), (Int, Int))] -> Int
automatedAnswer stepNum lastArea ps =
  case curArea > lastArea of
    True  -> stepNum - 1
    False -> automatedAnswer (stepNum + 1) (min curArea lastArea) (stepAll ps)
  where
    curArea = area ps


-- and an io-wrapped version of the automated answer, just that much easier
-- to play with in the repl, since i want to refer to it twice on the same line
ioAnswer :: IO [((Int, Int), (Int, Int))] -> IO Int
ioAnswer iops = do
  ps <- iops
  return $ automatedAnswer 0 (area ps) ps
