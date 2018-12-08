module Day where

import qualified Text.Regex as R (mkRegex, matchRegex)
import qualified Data.Map   as M (Map, empty, insert, findWithDefault, member, lookup, fromList)
import qualified Data.Set   as S (Set, empty, fromList, toList, member, filter)
import qualified Data.List  as L (sort, partition)
import qualified Data.Maybe as DM (catMaybes, isJust)
import Debug.Trace


-- Part 1
-- order the nodes in directed graph

-- we choose to represent the edges with just (Char, Char) pairs
-- we also assume that only direct dependencies are listed with edges
-- that is, if (a,b) and (b,c) are edges, there's not also (a,c)
-- it's not totally clear if we use this assumption, but possibly


-- still defaulting to regex usage for these line-parsing tasks
lineRegex = R.mkRegex "Step ([A-Z]) must be finished before step ([A-Z]) can begin."
linePair :: String -> (Char, Char)
linePair s = case R.matchRegex lineRegex s of
               Just [a,b] -> (head a, head b)

inputFromFile :: String -> IO [(Char, Char)]
inputFromFile s = do
  content <- readFile s
  return $ fmap linePair $ lines content

-- given sample input, pre-parsed or pulling from a file,
-- as well as my specific problem input
sampleInput = [('C','A'),('C','F'),('A','B'),('A','D'),('B','E'),('D','E'),('F','E')]
sampleFromFile = inputFromFile "input1"
givenProblem = inputFromFile "/tmp/7.1"


-- given a list of edges, we find all the nodes
nodes :: [(Char, Char)] -> [Char]
nodes ps = S.toList $ S.fromList $ concat $ fmap (\p -> [fst p, snd p]) ps

-- given a list of edges, re-arrange into a map from a node to its direct parent nodes
parents :: [(Char, Char)] -> M.Map Char [Char]
parents es = foldl incrMap M.empty es
  where
    incrMap :: M.Map Char [Char] -> (Char, Char) -> M.Map Char [Char]
    incrMap m (f,t) = M.insert t (f:cur) m
      where
        cur = M.findWithDefault [] t m

-- similar to parents, just capture the direct descendents of a node
children :: [(Char, Char)] -> M.Map Char [Char]
children es = fmap L.sort $ foldl incrMap M.empty es
  where
    incrMap :: M.Map Char [Char] -> (Char, Char) -> M.Map Char [Char]
    incrMap m (f,t) = M.insert f (t:cur) m
      where
        cur = M.findWithDefault [] f m

-- given a list of edges, find all nodes that don't have any parents
-- these are nodes which can be walked to next
roots :: [(Char, Char)] -> [Char]
roots ps = S.toList (S.filter (not . ((flip elem) (fmap snd ps))) $ S.fromList (fmap fst ps))


-- given a list of edges, and a list of root nodes, identify the sorted list of nodes
--
-- this method works, but after seeing part 2, I rearranged it to organize' with
-- some of the inner 'where' helper methods factored out
organize :: [(Char, Char)] -> [Char] -> [Char]
organize [] [] = []
organize [] fs = L.sort fs -- if there are no more edges, just sort the remaining roots
                           -- otherwise, we pick the lexicographically first option from
                           -- the roots of the given edges along with the 'fringe' roots
                           -- pop that dependency off, and recurse
organize ps fs = h : (organize offRoot (nf ++ newFringe))
  where
    r = head $ L.sort $ roots ps -- find the 'smallest' root among the edges
    (h,nf) = case length fs == 0 of -- see if there's a fringe node that's a smaller next node
                True  -> (r, [])
                False -> case (head fs) < r of -- the fringe has the next node to use
                           True  -> (head fs, tail fs)
                           False -> (r      , fs     )
    -- find any edges that are 'from' the identified root node
    (fromRoot, offRoot) = L.partition ((==r) . fst) ps -- seems a little odd we use r and not h here
    -- the next fringe nodes are those that were connected to the root and aren't
    -- referenced by any remaining edges
    newFringe = filter (not . ((flip elem) (nodes offRoot))) $ fmap snd fromRoot


-- now we refactor a little, and start using '-' as a placeholder for not
-- being present. Thus, an edge ('-','c') represents that 'c' is a root (unless
-- there are still other edges connected to id). As we pull nodes out of the graph,
-- we replace edges from them with edges like this.
--
-- this is probably where some other data modeling would be good,
-- like having two types of edges, actual and 'root', or something


-- this method identifies the node that an edge is 'from', or the node if the
-- edge is a 'root' edge with no 'from' (using the '-') signifier
edgeRoot :: (Char, Char) -> Char
edgeRoot ('-', c) = c
edgeRoot (c  , _) = c

-- find all the roots of edges, where edges now have this option of not having
-- a 'from' node. a node is a root if it is a 'from' or 'root' node and not a 'to' node.
markedRoots :: [(Char, Char)] -> [Char]
markedRoots ps = S.toList $ S.filter notARight lefts
  where
    lefts = S.fromList $ fmap edgeRoot ps
    -- a node is only a right if it is the second part of a tuple where the first part isn't '-'
    rights = fmap snd $ filter (not . (=='-') . fst) ps
    notARight :: Char -> Bool
    notARight = not . ((flip elem) rights)

-- given a character to replace, and a given character,
-- conver to '-' if the two are equal, otherwise leave the character alone
markChar :: Char -> Char -> Char
markChar m c = if (c == m) then '-' else c

-- mark both characters in a pair
markCharP :: Char -> (Char, Char) -> (Char, Char)
markCharP m (c,c') = (markChar m c, markChar m c')

-- given a task that has been resolved, we can eliminate the dependencies from
-- the edges by 'marking out' the task's character, and removing, entirely, any
-- edges that no longer refer to a real node
replaceNextTask :: Char -> [(Char, Char)] -> [(Char, Char)]
replaceNextTask m = filter (not . (==('-','-'))) . fmap (markCharP m)

-- if multiple tasks resolve, replace them all
replaceMultipleTasks :: [Char] -> [(Char, Char)] -> [(Char, Char)]
replaceMultipleTasks [] es = es
replaceMultipleTasks (c:cs) es = replaceMultipleTasks cs (replaceNextTask c es)

-- now we can simplify `organize` from above, by finding the next task
-- and then marking it as done in the work to be done, and recursing
organize' :: [(Char, Char)] -> [Char]
organize' [] = []
organize' ps = nextTask : (organize' $ replaceNextTask nextTask ps)
  where
    nextTask = head $ L.sort $ markedRoots ps


part1 :: [(Char, Char)] -> [Char]
part1 ps = organize ps []


part1' :: [(Char, Char)] -> [Char]
part1' ps = organize' ps


-- Part 2
-- In this part, we have multiple workers, and each task takes a different
-- amount of time. Workers cannot begin a task until all tasks they depend
-- on have been completed


-- the cost of a task varies between the sample input and the problem input,
-- so we add a little flexibility here
taskCost :: Int -> Char -> Int
taskCost b c = b + M.findWithDefault 0 c (M.fromList $ zipWith (,) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" [1..])

exampleTaskCost :: Char -> Int
exampleTaskCost = taskCost 0

problemTaskCost :: Char -> Int
problemTaskCost = taskCost 60

-- given a current workload, when will all the active tasks end?
expirations :: [Maybe (Char, Int)] -> [Int]
expirations = fmap snd . DM.catMaybes

-- support function for finding min/max expiration times relative to a given time
extremalExpiration :: ([Int] -> Int) -> [Int] -> Int -> Int
extremalExpiration _ [] t = t
extremalExpiration f xs _ = f xs

-- what is the earliest time a task expires, after a given time?
earliestExpiration :: [Maybe (Char, Int)] -> Int -> Int
earliestExpiration m t = extremalExpiration minimum (expirations m) t

-- similarly, what is the latest time a task expires, after a given time?
latestExpiration :: [Maybe (Char, Int)] -> Int -> Int
latestExpiration m t = extremalExpiration maximum (expirations m) t

-- given a time, and a task, does that task expire before or on the given time?
doesExpire :: Int -> (Char, Int) -> Bool
doesExpire t (c,i) = t >= i

-- given a workload, and a given time, which tasks expire at or by that time?
whatExpires :: [Maybe (Char, Int)] -> Int -> [Char]
whatExpires ms i = fmap fst $ DM.catMaybes $ filter exs ms
  where
    exs Nothing = False
    exs (Just p) = doesExpire i p

-- free up a worker if their task expires by/on the given time
causeExpiration :: Int -> Maybe (Char, Int) -> Maybe (Char, Int)
causeExpiration _ Nothing = Nothing
causeExpiration t j@(Just (c,e)) = if (t >= e) then Nothing else j

-- free up all workers whose tasks expire by/on the given time
causeExpirations :: [Maybe (Char, Int)] -> Int -> [Maybe (Char, Int)]
causeExpirations ms t = fmap (causeExpiration t) ms

-- declare a given task done, and so 'mark' it in the dependency graph
unblockWork :: Char -> [(Char, Char)] -> [(Char, Char)]
unblockWork c = fmap markFst
  where markFst (x,y) = if x == c then ('-',y) else (x,y)

-- given current workload and remaining dependency graph, what tasks could be started?
-- this is any root task from the dependency graph that isn't currently in progress
availableWork :: [Maybe (Char, Int)] -> [(Char, Char)] -> [Char]
availableWork ms es = L.sort nonBlockedWork
  where
    possibleWork = markedRoots es
    inProgressWork = DM.catMaybes $ fmap (fmap fst) ms
    nonBlockedWork = filter (not . ((flip elem) inProgressWork)) possibleWork

-- given a timestep, a function that defines how much a task costs,
-- a current workload, and a list of new work to start,
-- assign any new work to any free workers
assignWork :: Int -> (Char -> Int) -> [Maybe (Char, Int)] -> [Char] -> [Maybe (Char, Int)]
assignWork t _ m [] = m
assignWork t f m w = take (length m) $ alreadyBusy ++ newWorkers ++ untasked
  where
    alreadyBusy = filter DM.isJust m
    numUntasked = (length m) - (length alreadyBusy)
    untasked = mkUntaskedWorkers numUntasked
    newWorkers = fmap (\c -> Just (c, t + f c)) w


-- the following method doesn't quite work, but it was my first pass and I've decided to leave it
-- the problem is the coordination of what things are expired and when and what that means
-- about updating the graph. the next version is cleaner anyway.
--
-- given a task cost function, a current assignment list, a current time, and remaining work,
-- find the time all the tasks will be complete by.
--
-- we capture the current assignment list as a list of 'worker assignments',
-- where a worker assignment is Nothing if the worker isn't assigned anything, or
-- the pair consisting of the task and when it will be completed.
part2 :: (Char -> Int) -> [Maybe (Char, Int)] -> Int -> [(Char, Char)] -> Int
part2 _ m t [] = traceShow ("___", t, m) $ latestExpiration m t
part2 f m t es
  | all DM.isJust m || ((length $ availableWork m es) == 0), -- everybody busy, or all work blocked
    let t' = earliestExpiration m t
        m' = causeExpirations m t'
        es' = replaceMultipleTasks (whatExpires m' t') es
    = traceShow ("_W_", t, m, es) $ part2 f m' t' es'
  | otherwise
    = traceShow ("_A_", t, m, es) $ part2 f (causeExpirations m' t') t' es''
      where
        numFreeWorkers = length $ filter (not . DM.isJust) m
        workToAssign = take numFreeWorkers $ availableWork m es
        m' = traceShow ("   ", workToAssign) $ assignWork t f m workToAssign
        es' = replaceMultipleTasks (whatExpires m' t') es
        es'' = filter (\p -> not (fst p == '-' && elem (snd p) workToAssign)) es'
        t' = earliestExpiration m' t


-- same goal as `part2`, just this time don't try to be clever and find the
-- earliest time the workload will change, just take time one step at a time
part2' :: (Char -> Int) -> [Maybe (Char, Int)] -> Int -> [(Char, Char)] -> Int
part2' f m t [] = latestExpiration m t -- if there's no more work to add, just finish up current work
part2' f m t es = part2' f m'' t' es''
  where
    t' = 1+t -- the next time we'll consider
    m' = causeExpirations m t -- free up any work that expires on the current timestep
    we = whatExpires m t      -- identify those tasks that expired
    es' = replaceMultipleTasks we es -- update the dependency graph, since some tasks may have resolved
    numFreeWorkers = length $ filter (not . DM.isJust) m' -- see if anybody's free
    workToAssign = take numFreeWorkers $ availableWork m' es' -- figure out what work is next
    m'' = assignWork t f m' workToAssign -- assign the work
    es'' = filter (\p -> not (fst p == '-' && elem (snd p) workToAssign)) es' -- and clear up dependencies of things just started

mkUntaskedWorkers :: Int -> [Maybe (Char, Int)]
mkUntaskedWorkers w = take w $ repeat Nothing

samplePart2 :: [(Char, Char)] -> Int
samplePart2 = part2' exampleTaskCost (mkUntaskedWorkers 2) 0

realPart2 :: [(Char, Char)] -> Int
realPart2 = part2' problemTaskCost (mkUntaskedWorkers 5) 0

