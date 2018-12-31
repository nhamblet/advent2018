module Day where

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Debug.Trace

data GZip = GZip [Bool] Bool [Bool] Int Int Int

instance Show GZip where
  show gz = show' $ goFarLeft gz
    where
      show' gz@(GZip _ c rs i mi ma) = (show mi) ++ ": " ++ (fmap (\b -> if b then '#' else '.') (c:rs)) ++ ": " ++ (show ma)


mkState :: String -> GZip
mkState s = mkState' 0 0 ((length s)-1) (fmap (=='#') s)

mkState' :: Int -> Int -> Int -> [Bool] -> GZip
mkState' minI startIdx maxI rights = GZip falseLeft (head rights) (tail rights) startIdx minI maxI
  where
    falseLeft = replicate (startIdx - minI) False

mkState'' :: Int -> Int -> Int -> [Bool] -> GZip
mkState'' minI startIdx maxI lefts = GZip (tail lefts) (head lefts) falseRight startIdx minI maxI
  where
    falseRight = replicate (maxI-startIdx) False


readStateLine :: String -> GZip
readStateLine s = mkState $ drop (length "initial state: ") s

type RulePair = ((Bool, Bool, Bool, Bool, Bool), Bool)

readRuleLine :: String -> RulePair
readRuleLine s = ((a,b,c,d,e), ruleOut)
  where
    [a,b,c,d,e] = fmap (=='#') $ take 5 s
    ruleOut = (last s) == '#'

readInputFile :: String -> IO (GZip, [RulePair])
readInputFile s = do
  content <- readFile s
  return (readStateLine $ head $ lines content, fmap readRuleLine $ drop 2 $ lines content)

testZipper = mkState "#..#.#..##......###...###"
sampleInputIO = readInputFile "input1"
problemInputIO = readInputFile "/tmp/12.1"
testRules = fmap readRuleLine [
  "...## => #",
  "..#.. => #",
  ".#... => #",
  ".#.#. => #",
  ".#.## => #",
  ".##.. =. #",
  ".#### => #",
  "#.#.# => #",
  "#.### => #",
  "##.#. => #",
  "##.## => #",
  "###.. => #",
  "###.# => #",
  "####. => #"]
testRuleFunction = ruleFunction testRules


positiveRules :: [RulePair] -> [RulePair]
positiveRules = filter snd

type RuleFunction = (Bool, Bool, Bool, Bool, Bool) -> Bool

ruleFunction :: [RulePair] -> RuleFunction
ruleFunction rs t = elem t $ fmap fst $ positiveRules rs

left :: GZip -> GZip
left gz@(GZip [] c rs i mi ma) = gz
left (GZip (p:ps) c rs i mi ma) = GZip ps p (c:rs) (i-1) mi ma

getLeft :: GZip -> Bool
getLeft (GZip [] _ _ _ _ _) = False
getLeft (GZip (l:ls) _ _ _ _ _) = l

getTwoLeft :: GZip -> (Bool, Bool)
getTwoLeft (GZip [] _ _ _ _ _) = (False, False)
getTwoLeft (GZip (l:(l':ls)) _ _ _ _ _) = (l', l)
getTwoLeft (GZip (l:ls) _ _ _ _ _) = (False, l)

right :: GZip -> GZip
right gz@(GZip ls c [] i mi ma) = gz
right (GZip ls c (r:rs) i mi ma) = GZip (c:ls) r rs (i+1) mi ma

getRight :: GZip -> Bool
getRight (GZip _ _ [] _ _ _) = False
getRight (GZip _ _ (r:rs) _ _ _) = r

getTwoRight :: GZip -> (Bool, Bool)
getTwoRight (GZip _ _ [] _ _ _) = (False, False)
getTwoRight (GZip _ _ (r:(r':rs)) _ _ _) = (r,r')
getTwoRight (GZip _ _ (r:rs) _ _ _) = (r, False)

goFarLeft :: GZip -> GZip
goFarLeft gz@(GZip ls c rs i mi ma)
  | i > mi = goFarLeft $ left gz
  | otherwise = gz

sumPosCoordsToRight :: GZip -> Int
sumPosCoordsToRight gz@(GZip l c r i mi ma)
  | i == ma = thisCount
  | otherwise = thisCount + (sumPosCoordsToRight $ right gz)
  where
    thisCount = if c then i else 0

sumPosCoords :: GZip -> Int
sumPosCoords g = sumPosCoordsToRight $ goFarLeft g

curVal :: GZip -> (Bool, Int)
curVal (GZip ls c rs i mi ma) = (c, i)

tupleAt :: GZip -> (Bool, Bool, Bool, Bool, Bool)
tupleAt gz = (a,b,c,d,e)
  where
    (a,b) = getTwoLeft gz
    c     = fst $ curVal gz
    (d,e) = getTwoRight gz

evolveSingle :: RuleFunction -> GZip -> Bool
evolveSingle rf gz = rf $ tupleAt gz

evolve :: RuleFunction -> GZip -> GZip
evolve rf gz = evolveFromFarLeft rf [] $ goFarLeft gz

evolveN :: Int -> RuleFunction -> GZip -> GZip
evolveN 0 _ gz = gz
evolveN n rf gz = if n `mod` 10000000 == 0 then traceShow n $ ans else ans
  where
    ans@(GZip ls c rs i mi ma) = evolveN (n-1) rf (evolve rf gz)

evolveFromFarLeft :: RuleFunction -> [Bool] -> GZip -> GZip
evolveFromFarLeft rf bs gz@(GZip l c [] i mi ma) = mkState'' mi (i+numExtra) (ma+numExtra) (extras++(cur:bs))
  where
    cur = rf $ tupleAt gz
    b' = rf (getLeft gz, c, False, False, False)
    b'' = rf (c, False, False, False, False)
    extras = if b'' then [b'',b'] else (if b' then [b'] else [])
    numExtra = length extras
evolveFromFarLeft rf bs gz@(GZip [] c rs i mi ma) = evolveFromFarLeft rf bs rgz'
  where
    bs = [cur] ++ extras --,b',b'']
    cur = rf $ tupleAt gz
    b' = rf (False, False, False, c, getRight gz)
    b'' = rf (False, False, False, False, c)
    extras = if b'' then [b',b''] else (if b' then [b'] else [])
    numExtras = length extras
    rgz@(GZip l' c' r' i' mi' ma') = right gz
    rgz' = GZip l' c' r' i' (mi'-numExtras) ma'
evolveFromFarLeft rf bs gz@(GZip _ c (r:rs) i mi ma) = evolveFromFarLeft rf (b:bs) (right gz)
  where
    b = rf $ tupleAt gz


evolveIO :: Int -> IO (GZip, [RulePair]) -> IO (GZip, [RulePair])
evolveIO n setup = do
  (gz, rs) <- setup
  return $ (evolveN n (ruleFunction rs) gz, rs)


part1 :: IO (GZip, [RulePair]) -> IO Int
part1 setup = fmap (sumPosCoords.fst) $ evolveIO 20 setup


-- my laptop didn't like being asked to run this one
part2 :: IO (GZip, [RulePair]) -> IO Int
part2 setup = fmap (sumPosCoords.fst) $ evolveIO 50000000000 setup


sums :: RuleFunction -> GZip -> [Int]
sums rf gz = List.unfoldr ff gz
  where
    ff :: GZip -> Maybe (Int, GZip)
    ff gz = Just (sumPosCoords gz, evolve rf gz)


-- printing out a bunch of things, it looks like the differences converge to 42
showPart2 :: IO (GZip, [RulePair]) -> IO ()
showPart2 setup = do
  (gz, rs) <- setup
  mapM_ print $ zip [0..] (sums (ruleFunction rs) gz)


-- it seemed likely that the solution was going to depend on differences between
-- successive sums, so i looked at those, and they were consistently 42, after
-- a few thousand steps. so, taking one value as a place to start, i got the
-- following function and gave it 50000000000 as input, and it worked out
part2' :: Int -> Int
part2' n = (n-1500)*42 + 63061
