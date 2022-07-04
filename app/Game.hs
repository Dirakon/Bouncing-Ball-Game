{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module Game where

import Consts
import Data.Maybe
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle (radToDeg)
import Graphics.Gloss.Interface.IO.Game
import MathUtils
import Render (renderBackground, renderEnemies, renderMap, renderPlayer)
import TextSizeAnalysis (alignedCenterText)
import Types (Coords, EnemyBallType (..), EnemyPeg (..), MapInfo (..), MetaInfo (..), PlayerBall (..), Position, Sprites (..))

-- | A data structure to hold the state of the game.
data GameState = Game
  { mainBall :: Maybe PlayerBall,
    ballsLeft :: Int,
    userMousePosition :: Position,
    mapInfo :: MapInfo,
    initialMap :: MapInfo,
    metaInfo :: MetaInfo
  }  
  deriving (Show)


-- Generate initial game state depending on the map and metaInfo.
initialStateFrom :: MapInfo -> MetaInfo -> Coords -> GameState
initialStateFrom mapInfo metaInfo mouseCords =
  Game
    { mainBall = Nothing,
      ballsLeft = 6,
      userMousePosition = mouseCords,
      mapInfo = mapInfo,
      initialMap = mapInfo,
      metaInfo = metaInfo
    } 
   
-- | Update the game by moving the ball and bouncing of walls and enemies.
update :: Float -> GameState -> GameState
update seconds state = case mainBall state of
  Nothing -> updateBackgroundTrack state
  Just player -> updateBackgroundTrack (moveAndBounceBall player seconds state)
  where
    updateBackgroundTrack state = 
      state {metaInfo = newMetaInfo}
      where
        newMetaInfo = (metaInfo state) {requestedBackgroundTrackId = backgroundTrackId (mapInfo state)}

-- | Convert a game state into a picture.
render ::
  GameState -> -- The game state to render.
  Picture -- A picture of this game state.
render state =
  mapBackground <> ballTrajectory <> ball <> allEnemyBalls <> cannonPicture <> textPicture <> mapPicture
  where
    mapBackground = renderBackground mapData $metaInfo state

    -- Text rendering
    textPicture = translate 0 (- fromIntegral height / 2 + 20) (scale 0.2 0.2 (color black (alignedCenterText textToPrint)))
    textToPrint
      | Game.allDestroyableBallsAreDestroyed state = "You won! Press enter to visit the next level."
      | lives > 0 = "Balls left: " ++ show lives ++ " (press space to edit the level)"
      | otherwise = "You lost... Press 's' to restart..."

    -- Cannon rendering
    cannonPicture = translate cannonX cannonY (rotate cannonRotation (cannonSprite (sprites $ metaInfo state)))
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
                ( currentCannonPosition :
                  simulatedBallTrajectory
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
    mouseCoords = userMousePosition state
    currentCannonPosition = cannonPosition mapData
    directionFromCannonToMouse = normalizeV (vectorDiff mouseCoords currentCannonPosition)

simulatedBallTrajectory :: MapInfo -> Position -> Vector -> Float -> Float -> [Point]
simulatedBallTrajectory mapData startPosition dir startSpeed simulationDt =
  if collidedWithAnything
    then [newPoint]
    else newPoint : simulatedBallTrajectory mapData newPoint nextDir newSpeed simulationDt
  where
    collidedWithAnything = case collisionData of
      Nothing -> False
      _ -> True

    (newPoint, nextDir, newSpeed, collisionData) = moveAndCollide startPosition simulationDt dir startSpeed 1 mapData

type CollisionInfo = Maybe (SomeCollision, Float)

data SomeCollision
  = EnemyCollision EnemyPeg Coords
  | RightWallCollision
  | LeftWallCollision
  | FloorCollision
  | CeilingCollision

moveAndCollide :: Position -> Float -> Vector -> Float -> Float -> MapInfo -> (Vector, Vector, Float, CollisionInfo)
moveAndCollide ballPosition@(x, y) dt dir startSpeed radius mapData = (newPoint, nextDir, newSpeed, collisionData)
  where
    -- New direction and speed
    normalizedDir = normalizeV dir
    (movX, movY) = applyGravity (mulSV startSpeed normalizedDir)
    applyGravity vec = vectorSum [vec, gravity]
    nextDir = normalizeV (movX, movY)
    newSpeed = magV (movX, movY)

    -- Estimated locations (before collisions)
    x' = x + (dt * movX)
    y' = y + (dt * movY)
    predictedNextPoint = (x', y')

    movementStart = ballPosition

    (collisionData, newPoint) =
      case minimumByTotal compareByDistanceToPlayerStart allCollisions of
        Nothing -> (Nothing, predictedNextPoint)
        Just (collision, collisionPoint) -> (Just (collision, calculateDtLeft collisionPoint), collisionPoint)
      where
        calculateDtLeft (x', y') = dt * (distanceBetween (x, y) (x', y') / distanceBetween (x, y) predictedNextPoint)
        compareByDistanceToPlayerStart (_, p1) (_, p2)
          | distanceBetween ballPosition p1 > distanceBetween ballPosition p2 = GT
          | otherwise = LT

    allCollisions = catMaybes [collisionWithFloor, collisionWithCeiling, collisionWithLeftWall, collisionWithRightWall, collisionWithEnemy]
      where
        collisionWithFloor =
          case segmentHorizontalLineIntersection movementStart predictedNextPoint (radius + floorY mapData) of
            Nothing -> Nothing
            Just collisionPoint ->
              if alreadyColliding
                then Nothing
                else Just (FloorCollision, collisionPoint)
              where
                alreadyColliding = y <= radius + floorY mapData
        collisionWithCeiling =
          case segmentHorizontalLineIntersection movementStart predictedNextPoint (- radius + ceilingY mapData) of
            Nothing -> Nothing
            Just collisionPoint ->
              if alreadyColliding
                then Nothing
                else Just (CeilingCollision, collisionPoint)
              where
                alreadyColliding = y >= - radius + ceilingY mapData
        collisionWithLeftWall =
          case segmentVerticalLineIntersection movementStart predictedNextPoint (radius + leftWallX mapData) of
            Nothing -> Nothing
            Just collisionPoint ->
              if alreadyColliding
                then Nothing
                else Just (LeftWallCollision, collisionPoint)
              where
                alreadyColliding = x <= radius + leftWallX mapData
        collisionWithRightWall =
          case segmentVerticalLineIntersection movementStart predictedNextPoint (- radius + rightWallX mapData) of
            Nothing -> Nothing
            Just collisionPoint ->
              if alreadyColliding
                then Nothing
                else Just (RightWallCollision, collisionPoint)
              where
                alreadyColliding = x >= - radius + rightWallX mapData
        collisionWithEnemy =
          case getFirstEnemyIntersection movementStart predictedNextPoint (enemyBalls mapData) radius of
            Nothing -> Nothing
            Just (enemy, collisionPoint) ->
              if alreadyColliding
                then Nothing
                else Just (EnemyCollision enemy onEnemyCollisionPoint, vectorSum [mulSV (- playerOtsckokCoefficient) normalizedDir, collisionPoint])
              where
                onEnemyCollisionPoint = vectorSum [collisionPoint, mulSV radius $ normalizeV ((enemyPosition enemy) `vectorDiff` collisionPoint)]
                alreadyColliding = distanceBetween (enemyPosition enemy) movementStart <= enemyRadius enemy + radius

moveAndBounceBall :: PlayerBall -> Float -> GameState -> GameState
moveAndBounceBall
  (PlayerBall playerPos (oldDirX, oldDirY) playerRestitution speed playerRadius)
  seconds
  state =
    if dtLeft <= 0
      then newState
      else case movedPlayerBall of
        Nothing -> newState
        Just ball -> moveAndBounceBall ball dtLeft newState
    where
      newState = state {mainBall = movedPlayerBall} {mapInfo = newMapInfo} {ballsLeft = newBallsLeft} {metaInfo = newMetaInfo}

      -- Decrease balls lefts if ball dies this frame
      newBallsLeft = case movedPlayerBall of
        Nothing -> ballsLeft state - 1
        _ -> ballsLeft state

      -- Play sounds
      newMetaInfo =
        updateMetaInfoSounds [bumpSound] (metaInfo state)
        where
          bumpSound =
            case collisionData of
              Nothing -> Nothing
              Just (FloorCollision, _) -> Nothing
              _ -> Just "bump"

      -- Update map
      newMapInfo = oldMapInfo {enemyBalls = newEnemyBalls}
        where
          oldMapInfo = mapInfo state

      -- Destroy colliding enemy balls
      newEnemyBalls = case collisionData of
        Just (EnemyCollision enemy _, _) ->
          filter (not . wastedDurability) (map (decreaseDurabilityOnCollidingEnemies enemy) (enemyBalls (mapInfo state)))
        _ -> enemyBalls (mapInfo state)
      decreaseDurabilityOnCollidingEnemies collidedEnemy enemy = case ballType enemy of
        Destructible durability ->
          if enemy == collidedEnemy
            then enemy {ballType = Destructible (durability - 1)}
            else enemy
        _ -> enemy
      wastedDurability enemy = case ballType enemy of
        Destructible durability -> durability <= 0
        _ -> False

      -- Update ball (destroy on collision with floor)
      movedPlayerBall =
        if alive
          then Just (PlayerBall newPos nextDir playerRestitution newProcessedSpeed playerRadius)
          else Nothing
      alive = case collisionData of
        Just (FloorCollision, _) -> False
        _ -> newProcessedSpeed > nonMovingPlayerThreshold

      newProcessedSpeed = case collisionData of
        Nothing -> newSpeed
        _ -> newSpeed * (1 - playerVelocityLoseCoefficient)

      -- Change direction on collision
      nextDir = case collisionData of
        Nothing -> (curDirX, curDirY)
        Just (EnemyCollision enemy collisionPoint, _) -> playerVelocityOnEnemyCollision (curDirX, curDirY) enemy collisionPoint
        Just (RightWallCollision, _) -> (- abs curDirX, curDirY)
        Just (LeftWallCollision, _) -> (abs curDirX, curDirY)
        Just (CeilingCollision, _) -> (curDirX, - abs curDirY)
        Just (FloorCollision, _) -> (curDirX, curDirY)

      -- Get collision info and updated directions & speed
      (newPos, (curDirX, curDirY), newSpeed, collisionData) =
        moveAndCollide playerPos seconds (oldDirX, oldDirY) speed playerRadius (mapInfo state)
      dtLeft = case collisionData of
        Nothing -> 0
        Just (_, dt') -> dt'

-- | Change player velocity when colliding with one enemy peg
playerVelocityOnEnemyCollision :: Vector -> EnemyPeg -> Coords -> Vector
playerVelocityOnEnemyCollision
  (oldVx, oldVy)
  (EnemyPeg position _ _)
  collisionPoint =
    getNewVec position collisionPoint (oldVx, oldVy)
    where
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

-- Get first enemy peg intersecting with the player
getFirstEnemyIntersection :: Position -> Position -> [EnemyPeg] -> Float -> Maybe (EnemyPeg, Position)
getFirstEnemyIntersection
  playerStart
  playerEnd
  enemyBalls
  playerRadius =
    minimumByTotal compareByDistanceToPlayerStart (concatMap getCollisionPosition enemyBalls)
    where
      compareByDistanceToPlayerStart (_, p1) (_, p2)
        | distanceBetween playerStart p1 > distanceBetween playerStart p2 = GT
        | otherwise = LT

      getCollisionPosition enemy@(EnemyPeg enemyPosition enemyRadius _) =
        case segmentCircleFirstIntersection playerStart playerEnd (enemyPosition, enemyRadius + playerRadius) of
          Nothing -> []
          Just collisionPoint -> [(enemy, collisionPoint)]

-- | Respond to key events.
handleKeys :: Event -> GameState -> GameState
-- Restart game on 's' entered
handleKeys (EventKey (Char 's') Down _ _) state =
  initialStateFrom (initialMap state) (updateMetaInfoSounds [Just "reset_level"] (metaInfo state)) (0, 0)
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
  updateMetaInfoSounds others $oldMetaInfo {soundRequestList = newSoundRequests}
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