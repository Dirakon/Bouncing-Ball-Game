module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

type Position = (Float, Float)
type Velocity = (Float, Float)

data PlayerBall = PlayerBall Position Velocity

data EnemyBall = EnemyBall Position

data MetaInfo = MetaInfo {
  ballsLeft :: Int,
  mousePosition :: Position,
  cannonPosition :: Position,
  leftWallX :: Float,
  rightWallX :: Float,
  floorY :: Float,
  ceilingY :: Float
}

-- | A data structure to hold the state of the game.
data GameState
  = Game
  {
    mainBall :: Maybe PlayerBall,
    enemyBalls :: [EnemyBall],
    metaInfo :: MetaInfo
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
      mainBall = Nothing,
      enemyBalls = [],
      metaInfo = MetaInfo{
        ballsLeft = 3,
        mousePosition = (0,0),
        cannonPosition = (0,300),
        leftWallX = -300,
        rightWallX = 300,
        floorY = -300,
        ceilingY = 300
      }
    }

main :: IO ()
main = play window background fps initialState render handleKeys update

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> GameState -> GameState
update seconds = moveBall (seconds * 1000)

-- | Convert a game state into a picture.
render ::
  GameState       -- The game state to render.
  -> Picture      -- A picture of this game state.
render state =
  pictures [ball]
  where
    ball = case mainBall state of
      Nothing -> blank
      Just (PlayerBall playerPosition _) -> uncurry translate playerPosition $ color ballColor $ circleSolid 10
    ballColor = dark red

moveBall :: Float -> GameState -> GameState
moveBall seconds state = state {mainBall = movedPlayerBall} {metaInfo = newMetaInfo}
  where

    -- Decrease balls lefts if ball dies this frame
    newMetaInfo = case mainBall state of
      Just ball -> case movedPlayerBall of
        Nothing -> oldMetaInfo {ballsLeft = ballsLeft oldMetaInfo-1}
        _ -> oldMetaInfo
      _ -> oldMetaInfo
      where 
        oldMetaInfo = metaInfo state

    -- Move/bounce/destroy ball
    movedPlayerBall = case mainBall state of
      Nothing -> Nothing
      Just (PlayerBall playerPos playerVel) -> if alive then Just (PlayerBall newPos newVel) else Nothing
        where
          -- Old locations and velocities.
          (x, y) = playerPos
          (oldVx,oldVy) = playerVel

          -- New velocities
          vx
            | collidedWithLeftWall = abs oldVx
            | collidedWithRightWall = (- abs oldVx)
            | otherwise = oldVx
          vy
            | collidedWithCeiling = (- abs oldVy) 
            | otherwise =  oldVy
          newVel = (vx,vy)

          -- New locations.
          x' = x + vx * seconds
          y' = y + vy * seconds
          newPos = (x', y')

          -- Wall collision checks
          alive = not collidedWithFloor
          collidedWithLeftWall = x <= leftWallX (metaInfo state)
          collidedWithRightWall = x >= rightWallX (metaInfo state)
          collidedWithFloor = y <= floorY (metaInfo state)
          collidedWithCeiling = y >= ceilingY (metaInfo state)

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState

-- Spawn ball on mouse click (if has balls left and ball is not deployed yet)
handleKeys (EventKey (MouseButton LeftButton) Down xPos yPos) state =
  state {mainBall = newMainBall}
  where
    newMainBall = case mainBall state of
      Nothing -> if hasAnyBallsLeft then  Just spawnedBall else Nothing
      alreadyExistingBall ->  alreadyExistingBall
      
    hasAnyBallsLeft = ballsLeft (metaInfo state) > 0
    spawnedBall = PlayerBall cannonCoords ballVelocity
    cannonCoords = cannonPosition (metaInfo state)
    mouseCoords = mousePosition (metaInfo state)
    ballVelocity = normalizeV ballDirection
    ballDirection = vectorDiff mouseCoords cannonCoords

-- Update mouse position in meta info
handleKeys (EventMotion  (xPos, yPos)) state =
  state {metaInfo = newMetaInfo}
  where
    oldMetaInfo = metaInfo state
    newMetaInfo = oldMetaInfo {mousePosition = (xPos,yPos)}

-- For an 's' keypress, reset the game state
handleKeys (EventKey (Char 's') Down _ _) state =
  initialState
  
-- Do nothing for all other events.
handleKeys _ game = game


vectorDiff :: Vector -> Vector -> Vector
vectorDiff vec1 vec2 = (fst vec1 - fst vec2, snd vec1 - snd vec2)
