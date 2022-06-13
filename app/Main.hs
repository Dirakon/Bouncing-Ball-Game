module Main (main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector

type Position = (Float, Float)
type Velocity = (Float, Float)
type Restitution = Float
type Speed = Float
type Cords = (Float, Float)

data PlayerBall = PlayerBall Position Velocity Restitution Speed

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
      enemyBalls = [
        EnemyBall (-150,200), EnemyBall (-100,200), EnemyBall (-50,200),
        EnemyBall (0,200), EnemyBall (50,200), EnemyBall (100,200), 
        EnemyBall (150,200)
        ],
      metaInfo = MetaInfo{
        ballsLeft = 300,
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
update seconds = moveBall seconds

-- | Convert a game state into a picture.
render ::
  GameState       -- The game state to render.
  -> Picture      -- A picture of this game state.
render state =
  pictures [ball] <> pictures allEnemyBalls
  where
    allEnemyBalls = map drawEnemyBall (enemyBalls state)
      where
        drawEnemyBall :: EnemyBall -> Picture
        drawEnemyBall (EnemyBall enemyPosition) = uncurry translate enemyPosition $ color ballColor $ circleSolid 10
    ball = case mainBall state of
      Nothing -> blank
      Just (PlayerBall playerPosition _ _ _) -> uncurry translate playerPosition $ color ballColor $ circleSolid 10
    ballColor = dark red

startPlayerRestitution :: Restitution
startPlayerRestitution = 6
startPlayerSpeed :: Speed
startPlayerSpeed = 1000


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
      Just (PlayerBall playerPos playerVel playerRestitution speed) -> if 
        alive 
      then 
        Just (PlayerBall newPos newVel newPlayerRestitution speed) 
      else Nothing
        where

          -- Old locations and velocities.
          (x, y) = playerPos
          (oldVx,oldVy) = playerVel
          newPlayerRestitution = playerRestitution

          -- New velocities
          vx
            | collidedWithEnemyBall = fst newVec
            | collidedWithLeftWall = abs oldVx
            | collidedWithRightWall = (- abs oldVx)
            | otherwise = oldVx
          vy
            | collidedWithEnemyBall = snd newVec
            | collidedWithCeiling = (- abs oldVy) 
            | otherwise =  oldVy
          newVel = mulSV (speed*seconds) ( normalizeV (vx, vy))

          -- New locations.
          x' = case snd collisions of 
            True -> x - oldVx + vx
            False -> x + vx
          y' = case snd collisions of 
            True -> y - oldVy + vy
            False -> y + vy
          newPos = (x', y')

          -- Wall collision checks
          alive = not collidedWithFloor
          collidedWithLeftWall = x <= leftWallX (metaInfo state)
          collidedWithRightWall = x >= rightWallX (metaInfo state)
          collidedWithFloor = y <= floorY (metaInfo state)
          collidedWithCeiling = y >= ceilingY (metaInfo state)
          collidedWithEnemyBall = snd collisions
          newVec =
            if 
              snd collisions 
            then
              getNewVecFromArray (fst collisions)
            else
              (oldVx, oldVy)
              
          getNewVecFromArray :: [Maybe Cords] -> Cords
          getNewVecFromArray [] = (oldVx, oldVy)
          getNewVecFromArray (Nothing : js) = getNewVecFromArray js
          getNewVecFromArray (Just j : js) = getNewVec (fst j, snd j) (getColisionPoint (x, y) 10 (fst j, snd j) 10) (oldVx, oldVy)
          collisions = checkCollision (enemyBalls state)
            where
              checkCollision :: [EnemyBall] -> ([Maybe Cords], Bool)
              checkCollision [] = ([Nothing], False)
              checkCollision enemyBalls = anyCollision (map ttt enemyBalls)
                where
                  ttt :: EnemyBall -> (Maybe Cords, Bool)
                  ttt (EnemyBall enemyPosition) = ggg
                    where
                      (i, j) = enemyPosition
                      ggg = 
                        if 
                          condition 
                        then 
                          (Just (i, j), True) else (Nothing, False)
                        where
                          condition = sqrt((i - x)^2 + (j - y)^2) <= 20
                  anyCollision :: [(Maybe Cords, Bool)] -> ([Maybe Cords], Bool)
                  anyCollision [] = ([Nothing], False)
                  anyCollision arr = ((map fst arr), or (map snd arr))

          getColisionPoint :: 
            Cords -> 
            Float ->
            Cords ->
            Float ->
            Cords
          getColisionPoint 
            (playerBallX, playerBallY)
            playerRadius
            (enemyBallX, enemyBallY)
            enemyRadius =
            ((playerBallX * enemyRadius + enemyBallX * playerRadius) / radiusSum, 
            (playerBallY * enemyRadius + enemyBallY * playerRadius) / radiusSum)
              where
                radiusSum = playerRadius + enemyRadius
            
          getNewVec :: 
            Cords
            -> Cords
            -> Cords
            -> Cords
          getNewVec 
            (x_center, y_center)
            (x_collision, y_collision)
            (x_vec_abs, y_vec_abs) =
              if
                x_vec_abs * (y_collision - y_center) == y_vec_abs * (x_collision - x_center) 
              then
                (-x_vec_abs + 0.01, -y_vec_abs)
              else
                resultVector
              where
                x_new_vector = x_collision - x_ins
                y_new_vector = y_collision - y_ins
                x_ins = 2 * g - x_eov - a
                y_ins = 2 * h - y_eov - b
                a = x_center - x_collision
                b = y_center - y_collision
                g = h * x_collision / y_collision
                h = (x_eov + y_eov * temp) / (temp + 1 / temp)
                temp = b / a
                x_eov = a - x_vec_abs
                y_eov = b - y_vec_abs
                normalizedResVector = normalizeV (x_new_vector, y_new_vector)
                resultVector = (fst normalizedResVector * magV (x_vec_abs, y_vec_abs),
                  snd normalizedResVector * magV (x_vec_abs, y_vec_abs))
                      

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
    spawnedBall = PlayerBall cannonCoords ballVelocity startPlayerRestitution startPlayerSpeed
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
