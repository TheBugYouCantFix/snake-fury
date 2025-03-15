module GameState where

import RenderState (BoardInfo (..), Point, DeltaBoard, RenderMessage(..), CellType(..))
import qualified RenderState as Board
import Data.Sequence (Seq(Empty), Seq(..), empty, viewr, ViewR(..), (|>),  mapWithIndex, (!?), (<|), (|>))
import qualified Data.Sequence as Seq (Seq(Empty))
import System.Random ( uniformR, RandomGen(split), StdGen, Random (randomR))
import Data.Maybe (isJust)
import Foreign (new)
import Data.Foldable (toList, foldl')
import Data.Sequence (Seq,)
import qualified Data.Sequence as Seq
import Debug.Trace
data Movement = North | South | East | West deriving (Show, Eq)

data SnakeSeq = SnakeSeq {snakeHead :: Point, snakeBody :: Seq Point} deriving (Show, Eq)

data GameState = GameState
  { snakeSeq :: SnakeSeq
  , applePosition :: Point
  , movement :: Movement
  , randomGen :: StdGen
  }
  deriving (Show, Eq)

-- | This function should calculate the opposite movement.
opositeMovement :: Movement -> Movement
opositeMovement North = South
opositeMovement South = North
opositeMovement East = West
opositeMovement West = East


-- | Purely creates a random point within the board limits
makeRandomPoint :: BoardInfo -> StdGen -> (Point, StdGen)
makeRandomPoint (BoardInfo h w) = randomR ((1, 1), (h, w))

{-
We can't test makeRandomPoint, because different implementation may lead to different valid result.
-}


-- | Check if a point is in the snake
inSnake :: Point -> SnakeSeq  -> Bool
inSnake point (SnakeSeq sHead sBody) = point == sHead || point `elem` sBody

-- | Calculates de new head of the snake. Considering it is moving in the current direction
--   Take into acount the edges of the board
nextHead :: BoardInfo -> GameState -> Point
nextHead bi gs = let p = snakeHead $ snakeSeq gs
  in movementToDeltaHandled p bi gs


movementToDelta :: Point -> GameState -> Point
movementToDelta (x, y) gs = case movement gs of
        East -> (x, y + 1)
        West -> (x, y - 1)
        North -> (x - 1, y)
        South -> (x + 1, y)

handleDelta :: Point -> BoardInfo -> Point
handleDelta (x, y) (BoardInfo h w)
  | x < 1 = (w, y)
  | x > w = (1, y)
  | y < 1 = (x, h)
  | y > h = (x, 1)
  | otherwise = (x, y)

movementToDeltaHandled :: Point -> BoardInfo -> GameState -> Point
movementToDeltaHandled p bi gs = handleDelta (movementToDelta p gs) bi


-- | Calculates a new random apple, avoiding creating the apple in the same place, or in the snake body
newApple :: BoardInfo -> GameState -> (Point, StdGen)
newApple bi gs = let
    (p, stdGen') = makeRandomPoint bi $ randomGen gs
    in if inSnake p $ snakeSeq gs then newApple bi gs else (p, stdGen')

-- | Moves the snake based on the current direction. It sends the adequate RenderMessage
-- Notice that a delta board must include all modified cells in the movement.
-- For example, if we move between this two steps
--        - - - -          - - - -
--        - 0 $ -    =>    - - 0 $
--        - - - -    =>    - - - -
--        - - - X          - - - X
-- We need to send the following delta: [((2,2), Empty), ((2,3), Snake), ((2,4), SnakeHead)]
--
-- Another example, if we move between this two steps
--        - - - -          - - - -
--        - - - -    =>    - X - -
--        - - - -    =>    - - - -
--        - 0 $ X          - 0 0 $
-- We need to send the following delta: [((2,2), Apple), ((4,3), Snake), ((4,4), SnakeHead)]

pointOp :: (Int -> Int -> Int) -> Point -> Point -> Point
pointOp f p1 p2 = (f (fst p2) (fst p1), f (snd p2) (snd p2))


   