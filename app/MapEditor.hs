module MapEditor where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Types (Coords, EnemyBallType (..), EnemyPeg (..), MapInfo (..), Position, Sprites, PlayerBall (playerRadius), MetaInfo)
import Consts (width, height, wallColor)
import Data.Bits (Bits(shiftL))
import Graphics.UI.GLUT.Fonts
import Data.Data (ConstrRep(FloatConstr))
import TextSizeAnalysis (estimateTextWidth, alignedCenterText)

-- | A data structure to hold the state of the map editor.
data MapEditorState = Game
  { currentBall :: Maybe EnemyPeg,
    mapInfo :: MapInfo,
    userMousePosition :: Position,
    metaInfo :: MetaInfo
  }

changeRadius :: Maybe EnemyPeg -> Float -> Maybe EnemyPeg
changeRadius peg delta = case peg of
  Nothing -> Nothing
  Just ball -> if newRadius > 0
    then Just (ball {enemyRadius = currentRadius + delta})
    else Just (ball {enemyRadius = currentRadius})
    where
      currentRadius = enemyRadius ball
      newRadius = currentRadius + delta

changeBallType :: Maybe EnemyPeg -> Int -> Maybe EnemyPeg
changeBallType peg delta = case peg of
  Nothing -> Nothing
  Just ball -> case ballType ball of
    Indestructible ->
      if
        delta > 0
      then
        Just (ball {ballType = Destructible delta})
      else
        Just ball
    Destructible curDurability ->
      if
        newDurability > 0
      then
        Just (ball {ballType = Destructible newDurability})
      else
        Just (ball {ballType = Indestructible})
      where
        newDurability = curDurability + delta


inBoundaries :: Coords -> Float -> Bool
inBoundaries cords radius = condition
  where
    condition = x + radius / 2 < rightWallX emptyMap && x - radius / 2 > leftWallX emptyMap &&
      y + radius / 2 < ceilingY emptyMap && y - radius / 2 > floorY emptyMap
      where
        (x, y) = cords


-- | Update mouse position.
handleKeys (EventMotion (xPos, yPos)) state =
  state {userMousePosition = newUserMousePosition}
  where
    newUserMousePosition = (xPos, yPos)

-- | Place a new enemy ball.
handleKeys (EventKey (MouseButton RightButton) Down _ _) state =
  state
    { mapInfo = newMapInfo
    }
  where
    newMapInfo = oldMapInfo {enemyBalls = newEnemyBalls}
    oldMapInfo = mapInfo state

    newEnemyBalls = enemyBalls oldMapInfo ++
      [EnemyPeg (userMousePosition state) curBallRadius curBallType |
        inBoundaries (userMousePosition state) curBallRadius]
    curBallRadius = maybe 10 enemyRadius curBall
    curBallType = maybe (Destructible 1) ballType curBall
    curBall = currentBall state

-- | Remove balls intersecting with current ball.
handleKeys (EventKey (MouseButton LeftButton) Down _ _) state =
  state
    { mapInfo = newMapInfo
    }
  where
    newMapInfo = oldMapInfo {enemyBalls = newEnemyBalls}
    oldMapInfo = mapInfo state
    newEnemyBalls = filter (not . checkColission) (enemyBalls oldMapInfo)
      where
        checkColission x = sqrt ((enemyX - mouseX) ^ 2 + (enemyY - mouseY) ^ 2) <= sumRadius
          where
            (enemyX, enemyY) = enemyPosition x
            sumRadius = enemyRadius x + currentRadius
            currentRadius = maybe 10 enemyRadius (currentBall state)
        (mouseX, mouseY) = userMousePosition state

-- | Change on Wheel Up:
-- | * Size without Shift
-- | * Durability with Shift
handleKeys (EventKey (MouseButton WheelUp) Down modif _) state =
  state
    { currentBall = newCurrentBall
    }
  where
    oldCurBall = currentBall state
    newCurrentBall = case shift_ of
      Down -> changeBallType oldCurBall 1
      Up -> changeRadius oldCurBall 1
    (Modifiers shift_ _ _) = modif

-- | Change on Wheel Down:
-- | * Size without Shift
-- | * Durability with Shift
handleKeys (EventKey (MouseButton WheelDown) Down modif _) state =
  state
    { currentBall = newCurrentBall
    }
  where
    oldCurBall = currentBall state
    newCurrentBall = case shift_ of
      Down -> changeBallType oldCurBall (-1)
      Up -> changeRadius oldCurBall (-1)
    (Modifiers shift_ _ _) = modif

-- | Do nothing for all other events.
handleKeys _ state = state

emptyMap :: MapInfo
emptyMap =
  MapInfo
    { enemyBalls = [],
      cannonPosition = (0, 280),
      leftWallX = -300,
      rightWallX = 300,
      floorY = -300,
      ceilingY = 300
    }

-- Get editor state from map, and meta-info
editorStateFrom :: MapInfo -> MetaInfo -> MapEditorState
editorStateFrom map metaInfo =
  Game
    { currentBall = Just (EnemyPeg (0, 0) 10 (Destructible 1)),
      userMousePosition = (0, 0),
      mapInfo = map,
      metaInfo = metaInfo
    }

render ::
  MapEditorState -> -- The map state to render.
  Picture
render state =
   ball <> pictures allEnemyBalls <> textPicture <> wallsPicture <> texTT
  where
    -- Text rendering
    textPicture = translate 0  (- fromIntegral height / 2 + 20) (scale 0.2 0.2 (color green (alignedCenterText textToPrint)))
    textToPrint = "Press space to play the level"
    texTT =  translate playerX (playerY + radius / 5) $ scale mult mult (color green (alignedCenterText textToPrint))
      where
        (textWidth,textHeight) = estimateTextWidth textToPrint
        textToPrint = case player of
          Nothing -> show 0
          Just b ->
            case ballType b of
              Indestructible -> "Inf"
              Destructible t -> show t
        player = currentBall state
        playerCords = maybe (0, 0) enemyPosition player
        (playerX, playerY) = playerCords
        radius = maybe 10 enemyRadius player
        mult = radius / 100 / sqrt(fromIntegral (length textToPrint))


    wallsPicture = leftWall <> rightWall <> ceiling
      where
        leftWall = translate (curLeftX - wallWidth) (curFloorY + wallHeight / 2) $
          color wallColor $ rectangleSolid wallWidth wallHeight
        rightWall = translate (curRightX + wallWidth) (curFloorY + wallHeight / 2) $
          color wallColor $ rectangleSolid wallWidth wallHeight
        ceiling = translate 0 (curCeilingY + ceilWidth) $
          color wallColor $ rectangleSolid ceilLength ceilWidth
        curLeftX = leftWallX currentMapInfo
        curRightX = rightWallX currentMapInfo
        curFloorY = floorY currentMapInfo
        curCeilingY = ceilingY currentMapInfo
        currentMapInfo = mapInfo state
        wallWidth = 30
        ceilWidth = 30
        wallHeight = curCeilingY - curFloorY + ceilWidth
        ceilLength = curRightX - curLeftX + 3 * wallWidth

    allEnemyBalls = map drawEnemyBall (enemyBalls (mapInfo state))
      where
        drawEnemyBall :: EnemyPeg -> Picture
        drawEnemyBall (EnemyPeg enemyPosition radius _) = uncurry translate enemyPosition $ color red $ circleSolid radius
    ball = case currentBall state of
      Nothing -> blank
      Just (EnemyPeg enemyPosition enemyRadius _) -> uncurry translate enemyPosition $ color red $ circleSolid enemyRadius
-- | Update the game by moving the ball and bouncing off walls.
update ::
  Float ->
  MapEditorState ->
  MapEditorState
update seconds state = case currentBall state of
  Nothing -> state
  Just curBall ->
    if
      inBoundaries (userMousePosition state) (enemyRadius curBall)
    then
      moveCurBall curBall seconds state
    else
      state

moveCurBall ::
  EnemyPeg ->
  Float ->
  MapEditorState ->
  MapEditorState
moveCurBall
  (EnemyPeg enemyPosition enemyRadius ballType)
  seconds
  state =
    state {currentBall = newCurrentBall}
    where
      newCurrentBall = case currentBall state of
        Nothing -> Nothing
        Just oldCurBall -> Just oldCurBall {enemyPosition = newPosition}
      newPosition = userMousePosition state
