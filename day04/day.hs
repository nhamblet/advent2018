module Day4 where

import Data.List (sort)
import Text.Regex (matchRegex, mkRegex)
import qualified Data.Map as M (Map, empty, insert, findWithDefault, toList)
import Data.List.Unique (count)
import Data.Tuple (swap)
import GHC.Exts (sortWith)


data SimpleTime = YMDHM Int Int Int Int Int deriving (Eq, Show, Ord)


class Dated d where
  date :: d -> SimpleTime


-- Wakeup and Asleep data doesn't need the hour, it's always 0,
-- but we leave it because it makes sorting easier
data StatementLine =
    NewGuardLine Int Int Int Int Int String -- Year Month Day Hour Minute Id
  | WakeUpLine Int Int Int Int              -- Year Month Day Minute
  | FallAsleepLine Int Int Int Int          -- Year Month Day Minute
  deriving (Eq, Show)


instance Dated StatementLine where
  date (NewGuardLine   y m d hh mm _) = YMDHM y m d hh mm
  date (WakeUpLine     y m d mm)      = YMDHM y m d 0  mm
  date (FallAsleepLine y m d mm)      = YMDHM y m d 0  mm


instance Ord StatementLine where
  compare sl sl' = compare (date sl) (date sl')


dateRegex     = "\\[([0-9]+)-([0-9]+)-([0-9]+) ([0-9]+):([0-9]+)\\] "
newGuardRegex = mkRegex $ dateRegex ++ "Guard #([^\\ ]+) begins shift"
wakeupRegex   = mkRegex $ dateRegex ++ "wakes up"
asleepRegex   = mkRegex $ dateRegex ++ "falls asleep"

mkNewGuard :: [String] -> StatementLine
mkNewGuard [year, month, day, hour, minute, id] = NewGuardLine (read year) (read month) (read day) (read hour) (read minute) id

mkWakeup :: [String] -> StatementLine
mkWakeup [year, month, day, _, minute] = WakeUpLine (read year) (read month) (read day) (read minute)

mkAsleep :: [String] -> StatementLine
mkAsleep [year, month, day, _, minute] = FallAsleepLine (read year) (read month) (read day) (read minute)

mkStatementLine :: String -> StatementLine
mkStatementLine s =
  case matchRegex newGuardRegex s of
    Just newGuardMatch -> mkNewGuard newGuardMatch
    Nothing -> case matchRegex wakeupRegex s of
      Just wakeupMatch -> mkWakeup wakeupMatch
      Nothing -> case matchRegex asleepRegex s of
        Just asleepMatch -> mkAsleep asleepMatch
        -- partial function, to fail at runtime if we didn't parse correctly


givenLines :: IO [StatementLine]
givenLines = do
  content <- readFile "input1"
  return $ fmap mkStatementLine (lines content)

shuffledLines :: IO [StatementLine]
shuffledLines = do
  content <- readFile "input1.shuf"
  return $ fmap mkStatementLine (lines content)

-- 1002 lines in my file
myLines :: IO [StatementLine]
myLines = do
  content <- readFile "/tmp/4.1"
  return $ fmap mkStatementLine (lines content)


-- now i can read in all the lines, and sort them by their date
-- i need to read that as an event log, and now tag awake/asleep events to the guard id

data GuardEvent =
    NewGuard Int Int Int Int Int String
  | Wakeup   Int Int Int     Int String
  | Asleep   Int Int Int     Int String
  deriving (Eq, Show)

-- this partial function somewhat double-checks (via runtime failure)
-- that all events have a guard associated with them
mkEvent :: String -> StatementLine -> (GuardEvent, String)
mkEvent _  (NewGuardLine y m d hh mm id) = (NewGuard y m d hh mm id, id)
mkEvent id (WakeUpLine y m d mm)         = (Wakeup y m d mm id, id)
mkEvent id (FallAsleepLine y m d mm)     = (Asleep y m d mm id, id)


toEvents :: [StatementLine] -> [GuardEvent]
toEvents ss = drop 1 $ fmap fst $ scanl eventFolder (NewGuard (-1) (-1) (-1) (-1) (-1) "", "") (sort ss)
  where
    eventFolder :: (GuardEvent, String) -> StatementLine -> (GuardEvent, String)
    eventFolder = mkEvent . snd


-- ok, so, now i've converted all the lines into new guard-aware events

-- for Part 1, we need to find the guard that sleeps the most

-- first, let's further fold the events down to get a list of sleep events per guard
-- here, a sleep event is the start and end minute
sleepAmounts :: [GuardEvent] -> M.Map String [(Int, Int)]
sleepAmounts es = fst $ foldl eventFolder (M.empty, 0) es
  where
    eventFolder :: (M.Map String [(Int, Int)], Int) -> GuardEvent -> (M.Map String [(Int, Int)], Int)
    eventFolder (map, _)       (NewGuard _ _ _ _ mm  _)  = (map, mm)
    eventFolder (map, _)       (Asleep   _ _ _   mm id) = (map, mm)
    eventFolder (map, lastMin) (Wakeup   _ _ _   mm id) = (M.insert id tot map, mm)
      where tot = (lastMin, mm) : (M.findWithDefault [] id map)


-- find the total time asleep, and the minute asleep the most
sleepStats :: [(Int,Int)] -> (Int, Int)
sleepStats ses = (totalMinutes, mostMinute)
  where
    allMinutes = concat [[s..(e-1)] | (s,e) <- ses]
    totalMinutes = length allMinutes
    mostMinute = snd $ head $ reverse $ sort $ fmap swap $ count allMinutes


targetGuard :: M.Map String (Int, Int) -> (String, Int)
targetGuard m = (k, minute)
  where
    sorted = sortWith (fst . snd) $ M.toList m
    target = head $ reverse sorted
    k      = fst target
    minute = (snd . snd) target


-- the answer we're supposed to provide is the product of the guard's id
-- (as an int) with the minute we picked
convertAnswer :: (String, Int) -> Int
convertAnswer (s,i) = (read s :: Int) * i


part1 :: [String] -> Int
part1 ss = convertAnswer $ targetGuard $ fmap sleepStats $ sleepAmounts $ toEvents $ fmap mkStatementLine ss


doPart1 = do
  content <- readFile "/tmp/4.1"
  putStrLn $ show $ part1 $ lines content



-- part 2
-- first pass: 19914, apparently too low

-- find the guard/minute pair where the guard is asleep the most that minute

-- we already have the [(Int, Int)] for each guard, just want to re-process it

-- return the number of times asleep and the minute asleep the most
-- this way the order is the same as what targetGuard uses
secondSleepStats :: [(Int, Int)] -> (Int, Int)
secondSleepStats ses = ans
  where
    minuteCounts = count $ concat [[s..(e-1)] | (s,e) <- ses]
    ans          = swap $ head $ reverse $ sortWith snd minuteCounts


part2 :: [String] -> Int
part2 ss = convertAnswer $ targetGuard $ fmap secondSleepStats $ sleepAmounts $ toEvents $ fmap mkStatementLine ss

doPart2 = do
  content <- readFile "/tmp/4.1"
  putStrLn $ show $ part2 $ lines content
