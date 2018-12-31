module Day where

import GHC.Exts (sortWith)
import qualified Data.Matrix as Matrix
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Debug.Trace

type GridMap = Matrix.Matrix Char

data Direction = L | R | U | D deriving (Eq, Show)

data Traveller = Traveller Int Int Direction Int deriving (Eq, Show)

instance Ord Traveller where
  (Traveller i j d t) <= (Traveller i' j' d' t') = (i <= i' ) && (j <= j')

mkTraveller :: Int -> Int -> Char -> Maybe Traveller
mkTraveller i j c
  | c == 'v' = Just $ Traveller i j D 0
  | c == '^' = Just $ Traveller i j U 0
  | c == '<' = Just $ Traveller i j L 0
  | c == '>' = Just $ Traveller i j R 0
  | otherwise = Nothing

sampleInput = [
  "/->-\\        ",
  "|   |  /----\\",
  "| /-+--+-\\  |",
  "| | |  | v  |",
  "\\-+-/  \\-+--/",
  "  \\------/   "
  ]

(sampleGM, sampleTs) = readLines sampleInput

readGC :: Char -> Char
readGC c
  | c == '>' || c == '<' = '-'
  | c == '^' || c == 'v' = '|'
  | otherwise = c

readGridMap :: [String] -> GridMap
readGridMap ss = Matrix.fromList (length ss) (length $ head ss) chars
  where
    chars = concat $ (fmap readGC) <$>  ss

readTravellers :: [String] -> [Traveller]
readTravellers ss = ts
  where
    ts = concat $ (uncurry rts) <$> zip [0..] ss
    rts :: Int -> String -> [Traveller]
    rts r s = Maybe.catMaybes $ (uncurry $ mkTraveller r) <$> zip [0..] s

readLines :: [String] -> (GridMap, [Traveller])
readLines ss = (gm, ts)
  where
    gm = readGridMap ss
    ts = readTravellers ss

readFromFile :: String -> IO (GridMap, [Traveller])
readFromFile fname = do
  content <- readFile fname
  return $ readLines $ lines content

advanceAll :: GridMap -> [Traveller] -> ([Traveller], Maybe (Int, Int))
advanceAll gm ts = (nts, cols)
  where
    ots = List.sort ts
    nts = (advanceOne gm) <$> ts
    cols = detectCollisions ots nts

advanceN :: Int -> GridMap -> [Traveller] -> [Traveller]
advanceN 0 _ ts = ts
advanceN n gm ts = advanceN (n-1) gm ts'
  where
    (ts', _) = advanceAll gm ts

advanceThroughTurn :: Traveller -> Traveller
advanceThroughTurn t@(Traveller i j d ts)
  | d == L && ts `mod` 3 == 0 = Traveller (i+1) j D (ts+1)
  | d == L && ts `mod` 3 == 1 = Traveller i (j-1) L (ts+1)
  | d == L && ts `mod` 3 == 2 = Traveller (i-1) j U (ts+1)
  | d == R && ts `mod` 3 == 0 = Traveller (i-1) j U (ts+1)
  | d == R && ts `mod` 3 == 1 = Traveller i (j+1) R (ts+1)
  | d == R && ts `mod` 3 == 2 = Traveller (i+1) j D (ts+1)
  | d == U && ts `mod` 3 == 0 = Traveller i (j-1) L (ts+1)
  | d == U && ts `mod` 3 == 1 = Traveller (i-1) j U (ts+1)
  | d == U && ts `mod` 3 == 2 = Traveller i (j+1) R (ts+1)
  | d == D && ts `mod` 3 == 0 = Traveller i (j+1) R (ts+1)
  | d == D && ts `mod` 3 == 1 = Traveller (i+1) j D (ts+1)
  | d == D && ts `mod` 3 == 2 = Traveller i (j-1) L (ts+1)

gridAt :: GridMap -> Traveller -> Char
gridAt gm (Traveller i j _ _) = Matrix.getElem (i+1) (j+1) gm

advanceOne :: GridMap -> Traveller -> Traveller
advanceOne gm t@(Traveller i j d ts)
  | gat == '-' && d == L = Traveller i (j-1) d ts
  | gat == '-' && d == R = Traveller i (j+1) d ts
  | gat == '|' && d == U = Traveller (i-1) j d ts
  | gat == '|' && d == D = Traveller (i+1) j d ts
  | gat == '/' && d == U = Traveller i (j+1) R ts
  | gat == '/' && d == L = Traveller (i+1) j D ts
  | gat == '/' && d == R = Traveller (i-1) j U ts
  | gat == '/' && d == D = Traveller i (j-1) L ts
  | gat == '\\' && d == U = Traveller i (j-1) L ts
  | gat == '\\' && d == D = Traveller i (j+1) R ts
  | gat == '\\' && d == L = Traveller (i-1) j U ts
  | gat == '\\' && d == R = Traveller (i+1) j D ts
  | gat == '+' = advanceThroughTurn t
  | otherwise = undefined -- something went wrong
  where
    gat = gridAt gm t

detectCollisions :: [Traveller] -> [Traveller] -> Maybe (Int, Int)
detectCollisions ots nts = ans
  where
    tCollides :: Int -> Bool
    tCollides n = hasCollision (nts !! n) ((drop (n+1) ots) ++ (take n nts))
    hasCollision :: Traveller -> [Traveller] -> Bool
    hasCollision t ts = any (collides t) ts
    collides :: Traveller -> Traveller -> Bool
    collides (Traveller i j d t) (Traveller i' j' d' t') = i == i' && j == j'
    ans = tcoord <$> (nts !!) <$> List.find tCollides [0..((length nts)-1)]
    tcoord :: Traveller -> (Int, Int)
    tcoord (Traveller i j _ _) = (i, j)


showState :: GridMap -> [Traveller] -> [String]
showState gm ts = ans
  where
    ans = [[baseOrT i j | j <- [1..(Matrix.ncols gm)]] | i <- [1..(Matrix.nrows gm)]]
    tChar :: Int -> Int -> Maybe Char
    tChar i j = showT <$> List.find (tAt i j) ts
    tAt :: Int -> Int -> Traveller -> Bool
    tAt i j (Traveller i' j' _ _) = i == i' && j == j'
    baseOrT :: Int -> Int -> Char
    baseOrT i j = Maybe.fromMaybe (corners $ Matrix.getElem i j gm) (tChar (i-1) (j-1))
    showT :: Traveller -> Char
    showT (Traveller _ _ U _) = '^'
    showT (Traveller _ _ D _) = 'v'
    showT (Traveller _ _ L _) = '<'
    showT (Traveller _ _ R _) = '>'
    corners :: Char -> Char
    corners '\\' = '*'
    corners c    = c


findFirstCrash :: GridMap -> [Traveller] -> (Int, (Int, Int))
findFirstCrash gm ts = go 0 gm ts
  where
    go n gm ts = nextStep ts ts' collision
      where
        (ts',collision) = advanceAll gm ts
        nextStep :: [Traveller] -> [Traveller] -> Maybe (Int, Int) -> (Int, (Int, Int))
        nextStep ots nts Nothing = go (n+1) gm nts
        nextStep ots nts (Just (i,j)) = (n, (i, j))


debugP1 :: Int -> IO ()
debugP1 n = do
  (gm, ts) <- readFromFile "task.input"
  let ts' = advanceN n gm ts
  mapM_ print $ showState gm ts'

