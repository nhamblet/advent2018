module Day where

import Text.Regex (mkRegex, matchRegex)
import qualified Data.Map as Map (Map, empty, findWithDefault, lookup, insert, toList)
import GHC.Exts (sortWith)

-- coming off yesterday's puzzle, I'll try to use more data types today
-- i also wondered about record syntax in earlier puzzles, so might play with it more too


-- the inputs we are given are the number of players and how many marbles they play
data GameSetup = GameSetup { numPlayers :: Int, lastMarble :: Int }

-- the current score is map from player number to score
type GameScores = Map.Map Int Int

-- for the given samples, we record the game setup and winning score
sample1 = (GameSetup 5 25, 32)
sample2 = (GameSetup 10 1618, 8317)
sample3 = (GameSetup 13 7999, 146373)
sample4 = (GameSetup 17 1104, 2764)
sample5 = (GameSetup 21 6111, 54718)
sample6 = (GameSetup 30 5807, 37305)

myChallengeGameSetup = GameSetup 459 72103


-- we represent a board as the things 'left'/counter-clockwise of the current marble,
-- the current marble, and the things 'right'/clockwise from the current marble
data GameBoard = Board [Int] Int [Int]
mkNewBoard = Board [] 0 []


currentMarble :: GameBoard -> Int
currentMarble (Board _ m _) = m

stepClockwise :: GameBoard -> GameBoard
stepClockwise b@(Board [] m []) = b
stepClockwise (Board ccw m (h:cw)) = Board (m:ccw) h cw
stepClockwise (Board ccw m []) = stepClockwise $ Board [] m (reverse ccw)

stepCounterClockwise :: GameBoard -> GameBoard
stepCounterClockwise b@(Board [] m []) = b
stepCounterClockwise (Board (h:ccw) m cw) = Board ccw h (m:cw)
stepCounterClockwise (Board [] m cw) = stepCounterClockwise $ Board (reverse cw) m []

insertClockwiseFromCurrent :: Int -> GameBoard -> GameBoard
insertClockwiseFromCurrent n (Board ccw m cw) = Board (m:ccw) n cw

removeCurrentMarble :: GameBoard -> GameBoard
removeCurrentMarble (Board ccw m (h:cw)) = Board ccw h cw
removeCurrentMarble (Board ccw m []) = removeCurrentMarble $ Board [] m (reverse ccw)





-- the state of a game before placing `nextMarbleToPlace`
-- we always know whose turn it is, it is (nextMarbleToPlace % numPlayers)
data GameState = ActiveGame { nextMarbleToPlace :: Int,
                              board :: GameBoard,
                              scores :: GameScores
                            }
               | FinishedGame { board :: GameBoard, scores :: GameScores }

mkNewGame :: GameState
mkNewGame = ActiveGame 1 mkNewBoard Map.empty

winner :: GameState -> (Int, Int)
winner game = head $ reverse $ sortWith snd $ Map.toList (scores game)

finish :: GameState -> GameState
finish game = FinishedGame (board game) (scores game)

-- given the number of players, and current game state, advance the game
advance :: GameSetup -> GameState -> GameState
advance params game = undefined
  where
    curPlayer = (nextMarbleToPlace game) `mod` (numPlayers params)

advanceOrFinish :: GameSetup -> GameState -> GameState
advanceOrFinish params game
  | nextMarbleToPlace game > lastMarble params = finish game
  | otherwise                                  = advance params game
