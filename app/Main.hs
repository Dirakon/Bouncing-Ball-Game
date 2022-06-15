module Main (main) where

import Data.Bool (bool)
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Data.Word
import Data.ByteString (ByteString, pack)
import Types(Position, Velocity, Restitution, Speed, Coords, PlayerBall(..), EnemyBallType(..), EnemyPeg(..), MetaInfo(..))
import MapEditor
import Consts
import MathUtils


-- | A data structure to hold the state of the game.
data GameState = Game
  { mainBall :: Maybe PlayerBall,
    enemyBalls :: [EnemyPeg],
    metaInfo :: MetaInfo
  }


window :: Display
window = InWindow "Game" (width, height) (offset, offset)

initialState :: GameState
initialState =
  Game
    { mainBall = Nothing,
      enemyBalls =
        [ EnemyPeg (-150, -300) 10 (Destructible 1),
          EnemyPeg (-100, 0) 5 (Destructible 1),
          EnemyPeg (-50, -200) 20 (Destructible 1),
          EnemyPeg (0, -200) 30 (Destructible 3),
          EnemyPeg (50, 100) 10 (Destructible 1),
          EnemyPeg (100, 200) 5 (Destructible 1),
          EnemyPeg (150, 0) 2 (Destructible 1)
        ],
      metaInfo =
        MetaInfo
          { ballsLeft = 300,
            mousePosition = (0, 0),
            cannonPosition = (0, 300),
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
update seconds state = case mainBall state of
  Nothing -> state
  Just player -> moveBall player seconds state
  
purple :: [Word8]
purple = [128, 0, 128, 255]

whiter :: [Word8]
whiter = [255,255,255,255]

bitmapData :: ByteString
bitmapData = pack $ take (300*300*4) (cycle (purple++purple++purple++purple++purple ++purple++purple  ++ whiter))

--ourPicture :: Picture
--ourPicture = bitmapOfByteString 300 300 (BitmapFormat TopToBottom PxRGBA) bitmapData True
-- | Convert a game state into a picture.
render ::
  GameState -> -- The game state to render.
  Picture -- A picture of this game state.
render state =
  pictures [ball] <> pictures allEnemyBalls
  where
    allEnemyBalls = map drawEnemyBall (enemyBalls state)
      where
        drawEnemyBall :: EnemyPeg -> Picture
        drawEnemyBall (EnemyPeg enemyPosition radius _) = uncurry translate enemyPosition $ color ballColor $ circleSolid radius
    ball = case mainBall state of
      Nothing -> blank
      Just (PlayerBall playerPosition _ _ _ radius) -> uncurry translate playerPosition $ color ballColor $ circleSolid radius
    ballColor = dark red


moveBall :: PlayerBall -> Float -> GameState -> GameState
moveBall
  playerBall@(PlayerBall playerPos playerVel playerRestitution speed playerRadius)
  seconds
  state =
    state {mainBall = movedPlayerBall} {metaInfo = newMetaInfo} {enemyBalls = newEnemyBalls}
    where
      -- Decrease balls lefts if ball dies this frame
      newMetaInfo = case movedPlayerBall of
        Nothing -> oldMetaInfo {ballsLeft = ballsLeft oldMetaInfo - 1}
        _ -> oldMetaInfo
        where
          oldMetaInfo = metaInfo state

      -- Destroy colliding enemy balls
      newEnemyBalls =filter (not . wastedDurability) (map decreaseDurabilityOnCollidingEnemies (enemyBalls state))

      decreaseDurabilityOnCollidingEnemies enemy = case ballType enemy of
        Destructible durability ->
          if enemy `elem` collidingEnemies
            then enemy {ballType = Destructible (durability-1)}
            else enemy
        _ -> enemy

      wastedDurability enemy = case ballType enemy of
        Destructible durability -> durability <= 0
        _ -> False

      collidingEnemies = getCollidingBalls playerPos (enemyBalls state) playerRadius

      -- Move/bounce/destroy ball
      movedPlayerBall =
        if alive
          then Just (PlayerBall newPos (mulSV seconds newVel) playerRestitution newSpeed playerRadius)
          else Nothing
        where
          -- Old locations and velocities
          (x, y) = playerPos
          (oldVx, oldVy) = playerVel

          -- New velocities
          vx
            | collidedWithEnemyBall = fst velocityBouncedOnEnemies
            | collidedWithLeftWall = abs oldVx
            | collidedWithRightWall = -abs oldVx
            | otherwise = oldVx
          vy
            | collidedWithEnemyBall = snd velocityBouncedOnEnemies
            | collidedWithCeiling = -abs oldVy
            | otherwise = oldVy
          newVel = vectorSum[ gravity,  mulSV speed (normalizeV (vx, vy))]
          applyGravity vec = vectorSum [vec,mulSV seconds gravity]
          newSpeed = magV newVel

          -- New locations
          x' = x + vx
          y' = y + vy
          newPos = (x', y')

          -- Wall collisions
          alive = not collidedWithFloor
          collidedWithLeftWall = x <= leftWallX (metaInfo state)
          collidedWithRightWall = x >= rightWallX (metaInfo state)
          collidedWithFloor = y <= floorY (metaInfo state)
          collidedWithCeiling = y >= ceilingY (metaInfo state)

          -- Collision with enemy balls
          velocityBouncedOnEnemies = playerVelocityOnEnemyCollision playerBall collidingEnemies
          collidedWithEnemyBall = case listToMaybe collidingEnemies of
            Nothing -> False
            _ -> True

playerVelocityOnEnemyCollision :: PlayerBall -> [EnemyPeg] -> Vector
playerVelocityOnEnemyCollision
  playerBall@(PlayerBall playerPos@(playerX, playerY) playerVel@(oldVx, oldVy) playerRestitution speed playerRadius)
  collidingEnemies =
    vectorSum (getNewVectorsFromArray collidingEnemies)
    where
      getNewVectorsFromArray :: [EnemyPeg] -> [Coords]
      getNewVectorsFromArray [] = []
      getNewVectorsFromArray ((EnemyPeg position enemyRadius _) : js) =
        getNewVec position (getCollisionPoint playerPos playerRadius position enemyRadius) (oldVx, oldVy) :
        getNewVectorsFromArray js
      getCollisionPoint ::
        Coords ->
        -- Player position
        Float ->
        -- Player radius
        Coords ->
        -- Enemy position
        Float ->
        -- Enemy radius
        Coords
      -- Collision point
      getCollisionPoint
        (playerBallX, playerBallY)
        playerRadius
        (enemyBallX, enemyBallY)
        enemyRadius =
          ( (playerBallX * enemyRadius + enemyBallX * playerRadius) / radiusSum,
            (playerBallY * enemyRadius + enemyBallY * playerRadius) / radiusSum
          )
          where
            radiusSum = playerRadius + enemyRadius
      getNewVec ::
        Coords ->
          -- Position of enemy ball
        Coords ->
          -- Collision point of enemy ball
        Coords ->
          -- Old velocity of player ball
        Coords
        -- New velocity (on collision with this one enemy ball)
      getNewVec
        (x_center, y_center)
        (x_collision, y_collision)
        (x_vec_abs, y_vec_abs) =
          if x_vec_abs * b == y_vec_abs * a
            then (-x_vec_abs - 0.01, -y_vec_abs - 0.02)
            else resultVector
          where
            c = x_center -- center of static circle, x axis
            d = y_center -- center of static circle, y axis
            a = x_collision - c -- collision point, x axis, in system centered in (c, d)
            b = y_collision - d -- collision point, y axis, in system centered in (c, d)
            e = a - x_vec_abs -- point to mirror, x axis
            f = b - y_vec_abs -- point to mirror, y axis
            h = (e + f * b / a) / (b / a + a / b)
            g = h * a / b
            x = 2 * g - e - a -- new vector, x axis
            y = 2 * h - f - b -- new vector, y axis
            resultVector = (x, y)

getCollidingBalls :: Velocity -> [EnemyPeg] -> Float -> [EnemyPeg]
getCollidingBalls
  (playerX, playerY)
  enemyBalls
  playerRadius =
    filter collidingWith enemyBalls
    where
      collidingWith :: EnemyPeg -> Bool
      collidingWith (EnemyPeg enemyPosition enemyRadius _) =
        distanceFromEnemyCenter <= playerRadius + enemyRadius
        where
          (i, j) = enemyPosition
          distanceFromEnemyCenter = sqrt ((i - playerX) ^ 2 + (j - playerY) ^ 2)

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState
-- Spawn ball on mouse click (if has balls left and ball is not deployed yet)
handleKeys (EventKey (MouseButton LeftButton) Down xPos yPos) state =
  state {mainBall = newMainBall}
  where
    newMainBall = case mainBall state of
      Nothing -> if hasAnyBallsLeft then Just spawnedBall else Nothing
      alreadyExistingBall -> alreadyExistingBall

    hasAnyBallsLeft = ballsLeft (metaInfo state) > 0
    spawnedBall = PlayerBall cannonCoords ballVelocity startPlayerRestitution startPlayerSpeed startPlayerRadius
    cannonCoords = cannonPosition (metaInfo state)
    mouseCoords = mousePosition (metaInfo state)
    ballVelocity = normalizeV ballDirection
    ballDirection = vectorDiff mouseCoords cannonCoords

-- Update mouse position in meta info
handleKeys (EventMotion (xPos, yPos)) state =
  state {metaInfo = newMetaInfo}
  where
    oldMetaInfo = metaInfo state
    newMetaInfo = oldMetaInfo {mousePosition = (xPos, yPos)}

-- For an 's' key pressed, reset the game state
handleKeys (EventKey (Char 's') Down _ _) state =
  initialState
-- Do nothing for all other events.
handleKeys _ game = game
