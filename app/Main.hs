module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game


type Position = (Float, Float)

type Velocity = (Float, Float)

data PlayerBall = PlayerBall Position Velocity

data EnemyBall = EnemyBall Position

-- | A data structure to hold the state of the game.
data GameState
  = Game
  { 
    mainBall :: PlayerBall,
    enemyBalls :: [EnemyBall]
  }


width, height, offset :: Int
width = 700
height = 700
offset = 100

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

window :: Display
window = InWindow "Game" (width, height) (offset, offset)

initialState :: GameState
initialState =
  Game
    { 
      mainBall = PlayerBall (0, 0) (1, 1),
      enemyBalls = []
    }

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> GameState -> GameState
update seconds = moveBall (seconds * 100)

-- | Convert a game state into a picture.
render ::
  GameState       -- The game state to render.
  -> Picture      -- A picture of this game state.
render state =
  pictures [ball]
  where
    (PlayerBall playerPosition _) = mainBall state
    ball = uncurry translate playerPosition $ color ballColor $ circleSolid 10
    ballColor = dark red

moveBall :: Float -> GameState -> GameState
moveBall seconds state = state {mainBall = movedPlayerBall}
  where
    -- Old locations and velocities.
    (PlayerBall playerPos playerVel) = mainBall state
    (x, y) = playerPos
    (vx, vy) = playerVel

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds
    newPos = (x', y')
    movedPlayerBall = PlayerBall newPos playerVel

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- For an 's' keypress, reset the ball to the center.
handleKeys (EventKey (Char 's') Down _ _) state =
  state {mainBall = PlayerBall (0, 0) (1, 1)}
  
-- Do nothing for all other events.
handleKeys _ game = game