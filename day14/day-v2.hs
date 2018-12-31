module Day where

import qualified Data.Vector as V


-- a scoreboard has a vector of the scores, as well as the positions
-- of elves 1 and 2
data Scoreboard = SCS (V.Vector Int) Int Int Int

instance Show Scoreboard where
  show (SCS v l i j) = (show i) ++ ", " ++ (show j) ++ ": " ++ (show $ V.slice 0 l v)

mkScoreboard :: Int -> Scoreboard
mkScoreboard n = SCS vec 2 0 1
  where
    emptyVec = V.replicate n 0
    vec      = emptyVec V.// [(0,3),(1,7)]

advanceScoreboard :: Scoreboard -> Scoreboard
advanceScoreboard (SCS v l i j) = SCS nextVec l' i' j'
  where
    newScores = (\c -> read [c] :: Int) <$> show ((v V.! i) + (v V.! j))
    nextVec = v V.// (zip [l..] newScores)
    l' = l + (length newScores)
    i' = (i + 1 + (v V.! i)) `mod` l'
    j' = (j + 1 + (v V.! j)) `mod` l'

mkScores :: Int -> Scoreboard
mkScores n = go $ mkScoreboard (n+15)
  where
    go :: Scoreboard -> Scoreboard
    go sb@(SCS v l _ _)
      | l >= n + 11 = sb
      | otherwise   = go $ advanceScoreboard sb

-- took 33370 seconds
scoreAfter :: Int -> String
scoreAfter n = concat $ show <$> V.toList next10
  where
    sb@(SCS v l i j) = mkScores n
    next10   = V.slice n 10 v
