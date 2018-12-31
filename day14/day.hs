module Day where

import qualified Data.Vector as V
import qualified Data.Maybe as Maybe


-- a scoreboard has a vector of the scores, as well as the positions
-- of elves 1 and 2
data Scoreboard = SCS (V.Vector Int) Int Int deriving Show

mkScoreboard :: Scoreboard
mkScoreboard = SCS ((V.singleton 3) V.++ (V.singleton 7)) 0 1

advanceScoreboard :: Scoreboard -> Scoreboard
advanceScoreboard (SCS v i j) = SCS nextVec i' j'
  where
    newScores = (\c -> read [c] :: Int) <$> show ((v V.! i) + (v V.! j))
    nextVec = v V.++ (V.generate (length newScores) (newScores !!))
    i' = (i + 1 + (v V.! i)) `mod` (V.length nextVec)
    j' = (j + 1 + (v V.! j)) `mod` (V.length nextVec)

mkScores :: Int -> Scoreboard
mkScores n = go mkScoreboard
  where
    go :: Scoreboard -> Scoreboard
    go sb@(SCS v _ _)
      | V.length v >= n + 11 = sb
      | otherwise = go $ advanceScoreboard sb

-- my puzzle input was 513401. this method took 2607s to run (~45 minutes)
scoreAfter :: Int -> String
scoreAfter n = concat $ show <$> V.toList next10
  where
    sb@(SCS v i j) = mkScores n
    next10   = V.slice n 10 v


-- part 2

advanceUntilEndsWith :: [Int] -> Scoreboard -> Scoreboard
advanceUntilEndsWith target sb@(SCS v i j)
  | endsCorrectly = sb
  | otherwise     = advanceUntilEndsWith target $ advanceScoreboard sb
  where
    endsCorrectly = Maybe.isJust $ findTargetEnd target v


findTargetEnd :: [Int] -> V.Vector Int -> Maybe Int
findTargetEnd target v
  | endsAt (lv - lt) = Just $ lv - lt
  | endsAt (lv - lt - 1) = Just $ lv - lt - 1
  | endsAt (lv - lt - 2) = Just $ lv - lt - 2
  | otherwise = Nothing
  where
    lt = length target
    lv = V.length v
    endsAt idx = (V.toList (V.slice idx lt v) == target)


findTargetSB :: [Int] -> Scoreboard -> Maybe Int
findTargetSB t sb@(SCS v i j) = findTargetEnd t v
