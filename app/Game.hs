module Game where

import Consts
import Data.Bool (bool)
import Data.ByteString (ByteString, pack)
import Data.Maybe
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import MathUtils
import Types (Coords, EnemyBallType (..), EnemyPeg (..), GameState (..), MapInfo (..), MetaInfo (..), PlayerBall (..), Position, Restitution, Speed, Velocity, Sprites (cannonSprite))
import Graphics.Gloss.Geometry.Angle (radToDeg)

initialStateFrom :: MapInfo ->Sprites-> GameState
initialStateFrom mapInfo sprites =
  Game
    { mainBall = Nothing,
      metaInfo =
        MetaInfo
          { ballsLeft = 300,
            mousePosition = (0, 0),
            mapInfo = mapInfo
          },
      sprites = sprites
    }

-- | Update the game by moving the ball and bouncing off walls.
update :: Float -> GameState -> GameState
update seconds state = case mainBall state of
  Nothing -> state
  Just player -> moveAndBounceBall player seconds state

purple :: [Word8]
purple = [128, 0, 128, 255]

whiter :: [Word8]
whiter = [255, 255, 255, 255]

bitmapData :: ByteString
bitmapData = pack $ take (300 * 300 * 4) (cycle (purple ++ purple ++ purple ++ purple ++ purple ++ purple ++ purple ++ whiter))

--ourPicture :: Picture
--ourPicture = bitmapOfByteString 300 300 (BitmapFormat TopToBottom PxRGBA) bitmapData True

-- | Convert a game state into a picture.
render ::
  GameState -> -- The game state to render.
  Picture -- A picture of this game state.
render state =
  ballTrajectory<> pictures [ball] <> pictures allEnemyBalls<> cannonPicture 
  where
  
    cannonPicture =translate cannonX cannonY (rotate cannonRotation (cannonSprite (sprites state)))
    (cannonX,cannonY) = cannonPosition mapData
    cannonRotation =
        sign * radToDeg ( angleVV (1,0) directionFromCannonToMouse) - 90
        where
          sign = if snd directionFromCannonToMouse > 0 then (-1) else 1

    allEnemyBalls = map drawEnemyBall (enemyBalls mapData)
      where
        drawEnemyBall :: EnemyPeg -> Picture
        drawEnemyBall (EnemyPeg (x,y) radius _) = translate x y $ color ballColor $ circleSolid radius
    ball = case mainBall state of
      Nothing -> blank
      Just (PlayerBall (x,y) _ _ _ radius) -> translate x y $ color ballColor $ circleSolid radius


    ballTrajectory = case mainBall state of
      Nothing ->
        color
          yellow
          ( Line
              ( simulatedBallTrajectory
                  mapData
                  currentCannonPosition
                  directionFromCannonToMouse
                  startPlayerSpeed
                  (1 / fromIntegral fps)
              )
          )
      _ -> blank
    ballColor = dark red


    mapData = mapInfo (metaInfo state)
    mouseCoords = mousePosition (metaInfo state)
    currentCannonPosition = cannonPosition mapData
    directionFromCannonToMouse = normalizeV (vectorDiff mouseCoords currentCannonPosition)

simulatedBallTrajectory :: MapInfo -> Position -> Vector -> Float -> Float -> [Point]
simulatedBallTrajectory mapData startPosition@(x, y) dir@(dirX, dirY) startSpeed simulationDt =
  if collidedWithAnything
    then []
    else newPoint : simulatedBallTrajectory mapData newPoint nextDir newSpeed simulationDt
  where
    collidedWithAnything = or [collidedWithCeiling, collidedWithFloor, collidedWithLeftWall, collidedWithRightWall, collidedWithEnemyBalls]

    collidedWithEnemyBalls = case listToMaybe enemyCollisions of
      Nothing -> False
      _ -> True

    (CollisionInfo collidedWithCeiling collidedWithRightWall collidedWithFloor collidedWithLeftWall enemyCollisions) =
      collisionData
    (newPoint, nextDir, newSpeed, collisionData) = moveAndCollide startPosition simulationDt dir startSpeed 1 mapData

data CollisionInfo = CollisionInfo
  { collidedWithCeiling :: Bool,
    collidedWithRightWall :: Bool,
    collidedWithFloor :: Bool,
    collidedWithLeftWall :: Bool,
    collidedEnemyBalls :: [EnemyPeg]
  }

moveAndCollide :: Position -> Float -> Vector -> Float -> Float -> MapInfo -> (Vector, Vector, Float, CollisionInfo)
moveAndCollide ballPosition@(x, y) dt dir startSpeed radius mapData = (newPoint, nextDir, newSpeed, collisionData)
  where
    collisionData =
      CollisionInfo
        {
          collidedWithCeiling = y >= ceilingY mapData,
          collidedWithRightWall = x >= rightWallX mapData,
          collidedWithFloor = y <= floorY mapData,
          collidedWithLeftWall = x <= leftWallX mapData,
          collidedEnemyBalls = collidingEnemies
        }

    -- New directions
    normalizedDir = normalizeV dir
    (movX, movY) = applyGravity (mulSV startSpeed normalizedDir)
    nextDir = normalizeV (vectorSum [gravity, (movX, movY)])
    applyGravity vec = vectorSum [vec, gravity]
    newSpeed = magV (movX, movY)
    -- New locations
    x' = x + (dt * movX)
    y' = y + (dt * movY)
    newPoint = (x', y')

    collidingEnemies = getCollidingBalls ballPosition (enemyBalls mapData) radius

moveAndBounceBall :: PlayerBall -> Float -> GameState -> GameState
moveAndBounceBall
  playerBall@(PlayerBall playerPos playerDir playerRestitution speed playerRadius)
  seconds
  state =
    state {mainBall = movedPlayerBall} {metaInfo = newMetaInfo}
    where
      -- Decrease balls lefts if ball dies this frame
      newMetaInfo = case movedPlayerBall of
        Nothing -> oldMetaInfo {ballsLeft = ballsLeft oldMetaInfo - 1} {mapInfo = newMapInfo}
        _ -> oldMetaInfo {mapInfo = newMapInfo}
        where
          oldMetaInfo = metaInfo state
          newMapInfo = oldMapInfo {enemyBalls = newEnemyBalls}
            where
              oldMapInfo = mapInfo oldMetaInfo
      -- Destroy colliding enemy balls
      newEnemyBalls = filter (not . wastedDurability) (map decreaseDurabilityOnCollidingEnemies (enemyBalls (mapInfo (metaInfo state))))

      decreaseDurabilityOnCollidingEnemies enemy = case ballType enemy of
        Destructible durability ->
          if enemy `elem` collidingEnemies
            then enemy {ballType = Destructible (durability - 1)}
            else enemy
        _ -> enemy

      wastedDurability enemy = case ballType enemy of
        Destructible durability -> durability <= 0
        _ -> False

      collidingEnemies = getCollidingBalls playerPos (enemyBalls (mapInfo (metaInfo state))) playerRadius

      -- Move/bounce/destroy ball
      movedPlayerBall =
        if alive
          then Just (PlayerBall newPos nextDir playerRestitution newSpeed playerRadius)
          else Nothing
        where
          -- Old locations and velocities
          (x, y) = playerPos
          (oldDirX, oldDirY) = playerDir

          -- New directions
          dirX
            | collidedWithEnemyBall = fst velocityBouncedOnEnemies
            | collidedWithLeftWall = abs oldDirX
            | collidedWithRightWall = -abs oldDirX
            | otherwise = oldDirX
          dirY
            | collidedWithEnemyBall = snd velocityBouncedOnEnemies
            | collidedWithCeiling = -abs oldDirY
            | otherwise = oldDirY

          (CollisionInfo collidedWithCeiling collidedWithRightWall collidedWithFloor collidedWithLeftWall _) =
            collisionData
          (newPos, nextDir, newSpeed, collisionData) = moveAndCollide playerPos seconds (dirX, dirY) speed 1 (mapInfo (metaInfo state))

          -- Wall collisions
          alive = not collidedWithFloor

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
handleKeys (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) state =
  state {mainBall = newMainBall}
  where
    newMainBall = case mainBall state of
      Nothing -> if hasAnyBallsLeft then Just spawnedBall else Nothing
      alreadyExistingBall -> alreadyExistingBall

    hasAnyBallsLeft = ballsLeft (metaInfo state) > 0
    spawnedBall = PlayerBall cannonCoords ballVelocity startPlayerRestitution startPlayerSpeed startPlayerRadius
    cannonCoords = cannonPosition (mapInfo (metaInfo state))
    mouseCoords = (xPos, yPos)
    ballVelocity = normalizeV ballDirection
    ballDirection = vectorDiff mouseCoords cannonCoords

-- Update mouse position in meta info
handleKeys (EventMotion (xPos, yPos)) state =
  state {metaInfo = newMetaInfo}
  where
    oldMetaInfo = metaInfo state
    oldMapInfo = mapInfo oldMetaInfo
    newMetaInfo = oldMetaInfo {mousePosition = (xPos, yPos)}

-- Do nothing for all other events.
handleKeys _ game = game
