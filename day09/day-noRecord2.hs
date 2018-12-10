module Day where

import qualified Data.Map as Map (Map, empty, findWithDefault, insert, toList)
import GHC.Exts (sortWith)


-- the inputs we are given are the number of players and how many marbles they play
data GameSetup = GameSetup Int Int deriving Show

-- the current score is a map from player number to score
type GameScores = Map.Map Int Int

-- for the given samples, we record the game setup and winning score
type Sample = (GameSetup, Int)
sample1 :: Sample; sample1 = (GameSetup 5 25, 32)
sample2 :: Sample; sample2 = (GameSetup 10 1618, 8317) :: Sample
sample3 :: Sample; sample3 = (GameSetup 13 7999, 146373) :: Sample
sample4 :: Sample; sample4 = (GameSetup 17 1104, 2764) :: Sample
sample5 :: Sample; sample5 = (GameSetup 21 6111, 54718) :: Sample
sample6 :: Sample; sample6 = (GameSetup 30 5807, 37305) :: Sample

myChallengeGameSetup :: GameSetup
myChallengeGameSetup = GameSetup 459 72103


-- we represent a board as the things 'left'/counter-clockwise of the current marble,
-- the current marble, and the things 'right'/clockwise from the current marble
data GameBoard = Board [Int] Int [Int] deriving Show

-- the base board has just the `0` marble on it, as the current marble
mkNewBoard :: GameBoard
mkNewBoard = Board [] 0 []

currentMarble :: GameBoard -> Int
currentMarble (Board _ m _) = m

stepClockwise :: GameBoard -> GameBoard
stepClockwise b@(Board [] _ []) = b
stepClockwise (Board ccw m (h:cw)) = Board (m:ccw) h cw
stepClockwise (Board ccw m []) = stepClockwise $ Board [] m (reverse ccw)

stepCounterClockwise :: GameBoard -> GameBoard
stepCounterClockwise b@(Board [] _ []) = b
stepCounterClockwise (Board (h:ccw) m cw) = Board ccw h (m:cw)
stepCounterClockwise (Board [] m cw) = stepCounterClockwise $ Board (reverse cw) m []

-- convenience method to take multiple steps counter-clockwise,
-- since that's one of the key rules in the game
stepsCCW :: Int -> GameBoard -> GameBoard
stepsCCW 0 b = b
stepsCCW n b = stepsCCW (n-1) (stepCounterClockwise b)

insertClockwiseFromCurrent :: Int -> GameBoard -> GameBoard
insertClockwiseFromCurrent n (Board ccw m cw) = Board (m:ccw) n cw

removeCurrentMarble :: GameBoard -> GameBoard
removeCurrentMarble (Board ccw _ (h:cw)) = Board ccw h cw
removeCurrentMarble (Board ccw m []) = removeCurrentMarble $ Board [] m (reverse ccw)


-- the state of a game before placing `nextMarbleToPlace`
-- we always know whose turn it is, it is (nextMarbleToPlace % numPlayers)
data ActiveGame = ActiveGame Int GameBoard GameScores deriving Show
data FinishedGame = FinishedGame GameBoard GameScores

data GameState = Active ActiveGame | Finished FinishedGame


mkNewGame :: ActiveGame
mkNewGame = ActiveGame 1 mkNewBoard Map.empty

-- the winner is the player with the highest score. we also return their score
winner :: FinishedGame -> (Int, Int)
winner (FinishedGame _ scores) = head $ reverse $ sortWith snd $ Map.toList scores
-- with record syntax, it wasn't so obvious that I was cheating with partial functions

-- mark a game as done
finish :: ActiveGame -> FinishedGame
finish (ActiveGame _ board scores) = FinishedGame board scores

-- how to tell when a game is over
outOfMarbles :: GameSetup -> ActiveGame -> Bool
outOfMarbles (GameSetup _ lastMarble) (ActiveGame nextMarble _ _) = nextMarble > lastMarble

-- given the number of players, and current game state, advance the game one step
-- either by making one play, or marking the game as Finished
advance :: GameSetup -> ActiveGame -> GameState
advance params game
  | outOfMarbles params game = Finished $ finish game
  | otherwise                = Active   $ playOne params game


-- make one play
playOne :: GameSetup -> ActiveGame -> ActiveGame
playOne params game@(ActiveGame nextMarble _ _) =
  if doPlay23 then play23 params game else playNormal params game
  where
    doPlay23 = nextMarble `mod` 23 == 0

-- make one play, assuming the GameState is not finished, and the marble is not divisible by 23
playNormal :: GameSetup -> ActiveGame -> ActiveGame
playNormal _ (ActiveGame nextMarble board scores) = ActiveGame (1 + nextMarble) nextBoard scores
  -- the next marble for the next state is always one more than the current model
  -- in the 'normal' play, the scores are unchanged
  -- so we just need to move clockwise by 1 step, and the insert the next marble clockwise of that
  where
    nextBoard = insertClockwiseFromCurrent nextMarble $ stepClockwise board

-- make one play, assuming the GameState is not finished, and the marble _is_ divisible by 23
play23 :: GameSetup -> ActiveGame -> ActiveGame
play23 (GameSetup numPlayers _) (ActiveGame nextMarble board scores) =
  ActiveGame (1 + nextMarble) nextBoard newScores
  where
    -- we first move counter-clockwise 7 steps
    cwBoard = stepsCCW 7 board
    -- and get the current marble at that position, and remove it
    scoringMarble = currentMarble cwBoard
    nextBoard = removeCurrentMarble cwBoard
    -- the player whose turn it is then gets 23+(removed marble) points added to their score
    scoreChange = nextMarble + scoringMarble
    scoringPlayer = nextMarble `mod` numPlayers
    previousScore = Map.findWithDefault 0 scoringPlayer scores
    newScores = Map.insert scoringPlayer (previousScore + scoreChange) scores
    

-- run a game to completion
run :: GameSetup -> GameState -> FinishedGame
run _ (Finished f) = f
run params (Active   g) = run params (advance params g)


-- for part 1 (and, actually, part 2), we run a game to completion, then find the score of the winner
part1 :: GameSetup -> Int
part1 params = snd $ winner $ run params $ Active mkNewGame

