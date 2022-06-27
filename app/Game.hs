module Game where

import Consts
import Data.Bool (bool)
import Data.ByteString (ByteString, pack)
import Data.Maybe
import Data.Word
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Interface.IO.Game
import MathUtils
import Render (renderEnemies, renderMap, renderPlayer)
import TextSizeAnalysis (alignedCenterText, estimateTextWidth)
import Types (Coords, EnemyBallType (..), EnemyPeg (..), MapInfo (..), MetaInfo (..), PlayerBall (..), Position, Restitution, Speed, Sprites (cannonSprite), Velocity)

-- | A data structure to hold the state of the game.
data GameState = Game
  { mainBall :: Maybe PlayerBall,
    ballsLeft :: Int,
    userMousePosition :: Position,
    mapInfo :: MapInfo,
    initialMap :: MapInfo,
    metaInfo :: MetaInfo
  }

-- Generate initial game state depending on the map and metaInfo.
initialStateFrom :: MapInfo -> MetaInfo -> GameState
initialStateFrom mapInfo metaInfo =
  Game
    { mainBall = Nothing,
      ballsLeft = 6,
      userMousePosition = (0, 0),
      mapInfo = mapInfo,
      initialMap = mapInfo,
      metaInfo = metaInfo
    }

-- | Update the game by moving the ball and bouncing of walls and enemies.
update :: Float -> GameState -> GameState
update seconds state = case mainBall state of
  Nothing -> state
  Just player -> moveAndBounceBall player seconds state

-- | Convert a game state into a picture.
render ::
  GameState -> -- The game state to render.
  Picture -- A picture of this game state.
render state =
  ballTrajectory <> ball <> allEnemyBalls <> cannonPicture <> textPicture <> mapPicture
  where
    -- Text rendering
    textPicture = translate 0 (- fromIntegral height / 2 + 20) (scale 0.2 0.2 (color green (alignedCenterText textToPrint)))
    textToPrint
      | Game.allDestroyableBallsAreDestroyed state = "You won! Press enter to visit the next level."
      | lives > 0 = "Balls left: " ++ show lives ++ " (press space to edit the level)"
      | otherwise = "You lost... Press 's' to restart..."

    -- Cannon rendering
    cannonPicture = translate cannonX cannonY (rotate cannonRotation (cannonSprite (sprites $metaInfo state)))
    (cannonX, cannonY) = cannonPosition mapData
    cannonRotation =
      sign * radToDeg (angleVV (1, 0) directionFromCannonToMouse) - 90
      where
        sign = if snd directionFromCannonToMouse > 0 then (-1) else 1

    mapPicture = renderMap mapData

    -- Enemy balls rendering
    allEnemyBalls = renderEnemies (enemyBalls mapData)

    -- Player rendering
    ball = maybe blank renderPlayer (mainBall state)

    -- Ball trajectory rendering
    ballTrajectory =
      if playerBallIsDeployed
        then blank
        else
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

    mapData = mapInfo state
    lives = ballsLeft state
    playerBallIsDeployed = case mainBall state of
      Nothing -> False
      _ -> True
    noEnemyBallsLeft = case listToMaybe (enemyBalls mapData) of
      Nothing -> True
      _ -> False
    mouseCoords = userMousePosition state
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

    (newPoint, nextDir, newSpeed, collisionData) = moveAndCollide startPosition simulationDt dir startSpeed 1 mapData
    (CollisionInfo collidedWithCeiling collidedWithRightWall collidedWithFloor collidedWithLeftWall enemyCollisions) =
      collisionData

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
        { collidedWithCeiling = y >= ceilingY mapData,
          collidedWithRightWall = x >= rightWallX mapData,
          collidedWithFloor = y <= floorY mapData,
          collidedWithLeftWall = x <= leftWallX mapData,
          collidedEnemyBalls = collidingEnemies
        }

    -- New direction and speed
    normalizedDir = normalizeV dir
    (movX, movY) = applyGravity (mulSV startSpeed normalizedDir)
    applyGravity vec = vectorSum [vec, gravity]
    nextDir = normalizeV (movX, movY)
    newSpeed = magV (movX, movY)

    -- New locations
    x' = x + (dt * movX)
    y' = y + (dt * movY)
    newPoint = (x', y')

    collidingEnemies = getCollidingBalls ballPosition (enemyBalls mapData) radius

moveAndBounceBall :: PlayerBall -> Float -> GameState -> GameState
moveAndBounceBall
  playerBall@(PlayerBall playerPos@(x, y) playerDir@(oldDirX, oldDirY) playerRestitution speed playerRadius)
  seconds
  state =
    state {mainBall = movedPlayerBall} {mapInfo = newMapInfo} {ballsLeft = newBallsLeft} {metaInfo = newMetaInfo}
    where
      -- Decrease balls lefts if ball dies this frame
      newBallsLeft = case movedPlayerBall of
        Nothing -> ballsLeft state - 1
        _ -> ballsLeft state

      newMetaInfo =
        updateMetaInfoSounds [bumpSound] (metaInfo state)
        where
          bumpSound =
            if or [collidedWithEnemyBall, collidedWithLeftWall, collidedWithRightWall, collidedWithCeiling]
              then Just "bump"
              else Nothing

      -- Update map
      newMapInfo = oldMapInfo {enemyBalls = newEnemyBalls}
        where
          oldMapInfo = mapInfo state

      -- Destroy colliding enemy balls
      newEnemyBalls = filter (not . wastedDurability) (map decreaseDurabilityOnCollidingEnemies (enemyBalls (mapInfo state)))
      decreaseDurabilityOnCollidingEnemies enemy = case ballType enemy of
        Destructible durability ->
          if enemy `elem` collidingEnemies
            then enemy {ballType = Destructible (durability - 1)}
            else enemy
        _ -> enemy
      wastedDurability enemy = case ballType enemy of
        Destructible durability -> durability <= 0
        _ -> False

      -- Update ball (destroy on collision with floor)
      movedPlayerBall =
        if alive
          then Just (PlayerBall newPos nextDir playerRestitution newSpeed playerRadius)
          else Nothing
      alive = not collidedWithFloor

      -- Change current direction on collision
      curDirX
        | collidedWithEnemyBall = fst velocityBouncedOnEnemies
        | collidedWithLeftWall = abs oldDirX
        | collidedWithRightWall = - abs oldDirX
        | otherwise = oldDirX
      curDirY
        | collidedWithEnemyBall = snd velocityBouncedOnEnemies
        | collidedWithCeiling = - abs oldDirY
        | otherwise = oldDirY

      -- Get collision info and updated directions and speed
      (CollisionInfo collidedWithCeiling collidedWithRightWall collidedWithFloor collidedWithLeftWall collidingEnemies) =
        collisionData
      (newPos, nextDir, newSpeed, collisionData) = moveAndCollide playerPos seconds (curDirX, curDirY) speed playerRadius (mapInfo state)

      -- Collision with enemy balls
      velocityBouncedOnEnemies = playerVelocityOnEnemyCollision playerBall collidingEnemies
      collidedWithEnemyBall = case listToMaybe collidingEnemies of
        Nothing -> False
        _ -> True

-- Change player velocity when colliding with enemy pegs
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
            then (- x_vec_abs - 0.01, - y_vec_abs - 0.02)
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

-- Get all enemy pegs colliding with the player
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
-- Restart game on 's' entered
handleKeys (EventKey (Char 's') Down _ _) state =
  initialStateFrom (initialMap state) (updateMetaInfoSounds [Just "reset_level"] (metaInfo state))
-- Spawn ball on mouse click (if has balls left and ball is not deployed yet)
handleKeys (EventKey (MouseButton LeftButton) Down _ (xPos, yPos)) state =
  state {mainBall = newMainBall} {metaInfo = newMetaInfo}
  where
    newMainBall = case mainBall state of
      Nothing -> if hasAnyBallsLeft then Just spawnedBall else Nothing
      alreadyExistingBall -> alreadyExistingBall

    newMetaInfo =
      updateMetaInfoSounds [spawnSound] (metaInfo state)
      where
        spawnSound =
          if not ballIsAlreadyPlaced && hasAnyBallsLeft
            then Just "ball_spawn"
            else Nothing

    ballIsAlreadyPlaced = case mainBall state of
      Nothing -> False
      _ -> True
    hasAnyBallsLeft = ballsLeft state > 0
    spawnedBall = PlayerBall cannonCoords ballVelocity startPlayerRestitution startPlayerSpeed startPlayerRadius
    cannonCoords = cannonPosition (mapInfo state)
    mouseCoords = (xPos, yPos)
    ballVelocity = normalizeV ballDirection
    ballDirection = vectorDiff mouseCoords cannonCoords

-- Update mouse position in meta info
handleKeys (EventMotion (xPos, yPos)) state =
  state {userMousePosition = (xPos, yPos)}
-- Do nothing for all other events.
handleKeys _ game = game

updateMetaInfoSounds :: [Maybe String] -> MetaInfo -> MetaInfo
updateMetaInfoSounds [] metaInfo = metaInfo
updateMetaInfoSounds (maybeString : others) oldMetaInfo =
  oldMetaInfo {soundRequestList = newSoundRequests}
  where
    oldSoundRequests = soundRequestList oldMetaInfo
    newSoundRequests =
      oldSoundRequests ++ soundRequestAdditions
    soundRequestAdditions = maybeToList maybeString

allDestroyableBallsAreDestroyed :: GameState -> Bool
allDestroyableBallsAreDestroyed state = not (any isDestructible (enemyBalls (mapInfo state)))
  where
    isDestructible (EnemyPeg _ _ Indestructible) = False
    isDestructible _ = True