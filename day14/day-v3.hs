{-# LANGUAGE FlexibleContexts #-}

module Day where

import Data.Vector.Unboxed (freeze)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MV
import Control.Monad (replicateM_)
import Debug.Trace
import Control.Monad.Primitive

-- https://www.schoolofhaskell.com/user/commercial/content/vector#mutable-vectors
main n m = do
  sb <- MV.replicate (n+2000) (0 :: Int)
  MV.write sb 0 2
  MV.write sb 1 0
  MV.write sb 2 1
  MV.write sb 3 3
  MV.write sb 4 7
  replicateM_ m $ do
    l <- MV.read sb 0
    i <- MV.read sb 1
    j <- MV.read sb 2
    s <- MV.read sb (3 + i)
    s' <- MV.read sb (3 + j)
    let ns = (\c -> read [c] :: Int) <$> show (s + s')
    sb <- if (3+l+(length ns)) > (MV.length sb - 1) then traceShow "growing" (MV.grow sb 1000) else return sb
    mapM_ (\p -> MV.write sb (3+l+(fst p)) (snd p)) (zip [0..] ns)
    let l' = if (l+length ns) `mod` 10000 < 4 then traceShow (l+length ns) (l+length ns) else l + length ns
    MV.write sb 0 l'
    let i' = (i+s+1) `mod` l'
    let j' = (j+s'+1) `mod` l'
    MV.write sb 1 i'
    MV.write sb 2 j'
    
  iv <- freeze sb
  print $ U.slice n 20 iv


tryGrow = do
  m <- MV.replicate 10 (0 :: Int)
  MV.grow m 10
  print $ MV.length m


growTo10 :: (PrimMonad m) => m (MV.MVector (PrimState m) Int) -> m (MV.MVector (PrimState m) Int)
growTo10 mv = do
  v <- mv
  if MV.length v >= 10 then return v else MV.grow v 10



doUntil :: (PrimMonad m) =>
  m (MV.MVector (PrimState m) Int)
  -> (MV.MVector (PrimState m) Int-> m (MV.MVector (PrimState m) Int))
  -> (m (MV.MVector (PrimState m) Int) -> m Bool)
  -> m (MV.MVector (PrimState m) Int)
doUntil mv f p = do
  v <- mv
  let w = f v
  v' <- w
  g <- p w
  if g then w else doUntil w f p


growToN :: (PrimMonad m) => Int -> m (MV.MVector (PrimState m) Int) -> m (MV.MVector (PrimState m) Int)
growToN n wv = doUntil wv growBy10 (hasLength n)
  where
    --growBy10 :: MV.MVector (PrimState m) Int -> m (MV.MVector (PrimState m) Int)
    growBy10 mv = MV.grow mv 10000

hasLength :: (PrimMonad m) => Int -> m (MV.MVector (PrimState m) Int) -> m Bool
hasLength n v = (>n).(MV.length) <$> v


--numRecipes :: (PrimMonad m) => m (MV.MVector (PrimState m) Int) -> m Int
numRecipes wv = do
  v <- wv
  MV.read v 0


--hasEnoughRecipes :: (PrimMonad m) => Int -> m (MV.MVector (PrimState m) Int) -> m Bool
hasEnoughRecipes n wv = (>n) <$> (numRecipes wv)


--nextRecipes :: (PrimMonad m) => MV.MVector (PrimState m) Int -> m (MV.MVector (PrimState m) Int)
nextRecipes :: MV.MVector RealWorld Int -> IO (MV.MVector RealWorld Int)
nextRecipes v = do
  l <- MV.read v 0
  i <- MV.read v 1
  j <- MV.read v 2
  s <- MV.read v (3 + i)
  s' <- MV.read v (3 + j)
  let ns = (\c -> read [c] :: Int) <$> show (s + s')
  v' <- growToN (5+l+(length ns)) (return v)
  mapM_ (\p -> MV.write v' (3+l+(fst p)) (snd p)) (zip [0..] ns)
  let l' = l + length ns
  MV.write v' 0 l'
  let i' = (i+s+1) `mod` l'
  let j' = (j+s'+1) `mod` l'
  MV.write v' 1 i'
  MV.write v' 2 j'
  return v'

--mkRecipes :: (PrimMonad m) => m (MV.MVector (PrimState m) Int)
--mkRecipes :: IO (MV.MVector RealWorld Int)
mkRecipes = do
  v <- MV.replicate 100000 (0 :: Int)
  MV.write v 0 2
  MV.write v 1 0
  MV.write v 2 1
  MV.write v 3 3
  MV.write v 4 7
  return v

--evolveN :: (PrimMonad m) => Int -> MV.MVector (PrimState m) Int -> m (MV.MVector (PrimState m) Int)
evolveN 0 v = return v
evolveN n v = do
  v' <- nextRecipes v
  evolveN (n-1) v'

evolveN' n w = do
  v <- w
  return $ evolveN n v

-- my puzzle input was 513401. this method took --- forever! no idea, because i killed it
-- (starting with mkRecipes of size 100000, growing by 10000 as needed)
part1 n = do
  v <- doUntil mkRecipes nextRecipes (hasEnoughRecipes (n+10))
  iv <- freeze v
  print $ U.slice (n+3) 10 iv


--showTop :: (PrimMonad m) => m (MV.MVector (PrimState m) Int) -> IO ()
showTop w = do
  v <- w
  v' <- freeze v
  print $ U.slice 0 30 v'
