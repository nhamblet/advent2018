module Day where

import GHC.Exts (sortWith)
import qualified Data.Map as Map
import qualified Data.Function.Memoize as Memo
import qualified Data.List as List
import qualified Data.Matrix as Matrix
import Debug.Trace


-- when playing with things in the repl, if you don't specify this as Int
-- it seems to default to Integer, and that's different, and I got tired
-- chasing it around. maybe i ended up fixing it some other way, with the
-- floor/fromIntegral/mod steps below.
myGridSerialNumber = 9435 :: Int


-- given the serial number, x-coord, y-coord
-- give the power level of the cell
cellPowerLevel :: Int -> Int -> Int -> Int
cellPowerLevel serial x y = cellPower
  where
    rackId = x + 10
    timesY = rackId * y
    withSerial = timesY + serial
    timesRack = withSerial * rackId
    hundo = hundredsDigit timesRack
    cellPower = hundo - 5

    hundredsDigit :: Int -> Int
    hundredsDigit num = (floor ((fromIntegral num)/100)) `mod` 10


-- the total power of a cell, in part 1, is given by the sum of the powers
-- of the cell and it's adjacent cells
totalPowerFunction :: Int -> Int -> Int -> Int
totalPowerFunction serial x y = (ul x y) + (u x y) + (ur x y) + (l x y) + (c x y) + (r x y) + (dl x y) + (d x y) + (dr x y)
  where
    cpf = cellPowerLevel serial
    ul x y = cpf (x-1) (y-1)
    u  x y = cpf (x  ) (y-1)
    ur x y = cpf (x+1) (y-1)
    l  x y = cpf (x-1) (y  )
    c  x y = cpf (x  ) (y  )
    r  x y = cpf (x+1) (y  )
    dl x y = cpf (x-1) (y+1)
    d  x y = cpf (x  ) (y+1)
    dr x y = cpf (x+1) (y+1)


-- given a serial number find the cell with the highest total power, and its power
highestTotalPower :: Int -> ((Int, Int), Int)
highestTotalPower serial = head $ reverse $ sortWith snd [((x,y), tpf x y) | x <- [2..299], y <- [2..299]]
  where
    tpf = totalPowerFunction serial


-- in part 1, we want the coordinates of the cell above and to the left of the cell
-- with the maximum total power
part1 :: Int -> (Int,Int)
part1 serial = minus1 $ fst $ highestTotalPower serial
  where
    minus1 (a,b) = (a-1,b-1)


-- in part 2, we are allowed to take the total power with any size window
-- at first, i tried just brute forcing, thinking my computer time was cheaper / easier
-- to use than my personal time. i didn't add enough traceShows to evaluate progress,
-- but after a while running and the day running out, it hadn't finished, so i tried
-- some other things while i waited, and eventually got there

-- some failed speedups
-- not shown is failed parallelism, because i was mostly doing things in a stack repl
-- and wasn't sure how to make that work (or maybe i got it to 'work' but it didn't speed
-- things up enough, or something)

-- these work out to be painfully slow, which is unfortunate
myMap = Map.fromList [((x,y),cellPowerLevel myGridSerialNumber x y) | x <- [1..299], y <- [1..299]]

mappedPowerFunction :: Int -> Int -> Int -> Int
mappedPowerFunction serial x y = (Map.!) myMap (x,y)

memoizedPF :: Int -> Int -> Int
memoizedPF = Memo.memoize2 (cellPowerLevel myGridSerialNumber)


-- finally, using matrices seemed to work well enough here. 

-- i also bailed on being generic with respect to the serial number
myOtherMatrix = Matrix.matrix 300 300 (uncurry $ cellPowerLevel myGridSerialNumber)


-- given a serial number and a size and an x/y, get the total power of the
-- cells in the region from that x/y down and to the right by the size
--
-- this function is still too slow
sizedTotalPowerFunction :: Int -> Int -> Int -> Int -> Int
sizedTotalPowerFunction serial size x y = sum cellPowers
  where
    cellPowers = [Matrix.getElem x' y' myOtherMatrix | x' <- [x..(x+size)], y' <- [y..(y+size)]]

-- since this uses sizedTotalPowerFunction, it's still too slow
part2 :: Int -> (Int,Int,Int)
part2 serial = fst $ head $ reverse $ sortWith snd pls
  where
    pls = [((x,y,s), sizedTotalPowerFunction serial s x y) | x <- [1..300], y <- [1..300], s <- [1..(min (301-x) (301-y))]]


-- here i got frustrated that i hadn't found maximumBy yet, or, after i found it,
-- that it made me pass a binary function that returned an Ordering, where, for example,
-- i'd like a maximumWith, similar to sortWith
ff :: Ord b => (a -> b) -> (b, a) -> a -> (b, a)
ff f (m,a) a' = if (f a' > m) then (f a', a') else (m, a)

-- i don't understand why this doesn't compile. it looks ok to me, but maybe i'm just tired
--maxBy' :: Ord b => (a -> b) -> [a] -> a
--maxBy' f (a:as) = foldl (ff f) (f a, a) as

maxBy :: Ord b => (a -> b) -> [a] -> a
maxBy f as = List.maximumBy ordf as
  where
    ordf a a' = compare (f a) (f a')


-- this takes a long time still
reducedPart2 :: Int -> Int -> (Int, Int, Int)
--reducedPart2 maxSize serial = fst $ maxBy snd $ pls
reducedPart2 maxSize serial = fst $ head $ reverse $ sortWith snd $ pls
  where
    pls = [((x,y,s), sizedTotalPowerFunction serial s x y) | x <- [1..300], y <- [1..300], s <- [1..(min (min (300-x) (300-y)) maxSize)]]


-- finally down here i make real progress on part 2

-- create a list of matrices, where the n-th element of the list consists of matrices
-- where the (x,y) coordinate has a value that is the sum of the nxn box whose upper-right
-- corner is (x,y).
--
-- unfoldr still isn't the function i'd really hope to use here, it's one step more
-- flexible than i want. i think what i want has signature (a -> Maybe a) -> a -> [a]
-- where unfoldr has signature (b -> Maybe (a, b)) -> b -> [a]. but it works.
--
-- also, this might not be too hard to re-generalize to take an input that's the serial
-- number of the grid, although you sorta 
myMatrices :: [Matrix.Matrix Int]
myMatrices = List.unfoldr nextM (myOtherMatrix,  1) -- the snd is the size of the window
  where
    nextM :: (Matrix.Matrix Int, Int) -> Maybe (Matrix.Matrix Int, (Matrix.Matrix Int, Int))
    nextM (m, s)
      -- the "previous" answer (m, s) was the sum of the matrix sub-regions of size s
      -- we can use that for _most_ of the size (s+1) matrix, just need to add an extra
      -- boundary worth of cells
      | s > 300 = Nothing
      | otherwise = Just (m, (nm, s+1))
      where
        nm = Matrix.matrix (300-s) (300-s) sumCells
        -- the cells to count are the current cell, the (up to) (s-1) elements to the right,
        -- the (up to) (s-1) elements down, and then the (up to) (s-1)*(s-1) below and to the
        -- right (which we can get from the previous iteration of the unfolding).
        --
        -- as written, 'alongRow' and 'downCol' actually count the 'atCell', so to get the
        -- right total value we actually subtract it once, instead of adding it
        sumCells (x, y) = -(atCell x y) + (alongRow x y) + (downCol x y) + (belowRight x y)
        atCell x y = Matrix.getElem x y myOtherMatrix
        belowRight x y = Matrix.getElem (x+1) (y+1) m
        alongRow x y = sum [ Matrix.getElem x' y myOtherMatrix | x' <- [x..(min (x+s) 300)]]
        downCol x y = sum [ Matrix.getElem x y' myOtherMatrix | y' <- [y..(min (y+s) 300)]]

-- identify the coordinate with the maximum value in a matrix, and that value
matrixMax :: Matrix.Matrix Int -> ((Int, Int), Int)
matrixMax m = maxBy snd [((x, y), Matrix.getElem x y m) | x <- [1..(Matrix.ncols m)], y <- [1..(Matrix.nrows m)]]

-- find the maximum total window size
part2' :: (Int, Int, Int)
part2' = xtract $ maxBy (snd.fst) [ts s (matrixMax (myMatrices !! (s-1)), s) | s <- [1..300]]
  where
    xtract (((x,y),v),s) = (x,y,s)
    ts a b = traceShow (a,b) b

