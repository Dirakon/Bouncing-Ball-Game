module MapEditor where

import Consts (height, wallColor, width)
import Data.Bits (Bits (shiftL))
import Data.Data (ConstrRep (FloatConstr))
import Data.Maybe (maybeToList)
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Graphics.UI.GLUT.Fonts
import Render (renderEnemies, renderMap)
import TextSizeAnalysis (alignedCenterText, estimateTextWidth)
import Types (Coords, EnemyBallType (..), EnemyPeg (..), MapInfo (..), MetaInfo, PlayerBall (playerRadius), Position, Sprites(..))
import Graphics.Gloss (Picture)

-- | A data structure to hold the state of the map editor.
data MapEditorState = Game
  { currentBall :: Maybe EnemyPeg,
    mapInfo :: MapInfo,
    userMousePosition :: Position,
    metaInfo :: MetaInfo,
    backgroundFiles :: [String],
    mapBackgrounds :: [Maybe Picture],
    currentBackground :: Picture,
    currentBackgroundIndex :: Int
  }

safeFirstPicture :: [Maybe Picture] -> Picture
safeFirstPicture [] = blank
safeFirstPicture (x:xs) = case x of
  Just t -> t
  Nothing -> blank

safeGetPictureById :: [Maybe Picture] -> Int -> Picture
safeGetPictureById [] _ = blank
safeGetPictureById arr index = iter (0, arr) index
  where
    iter (_, []) _ = blank
    iter (i, (x:xs)) lookFor = 
      if lookFor == i then
        case x of
          Nothing -> blank
          Just t -> t
      else
        iter (i + 1, xs) lookFor

changePictureIndex :: Int -> Int -> Int -> Int
changePictureIndex index delta len = 
  if len /= 0 then
      if tempIndex >= 0 then
        tempIndex
      else
        len + tempIndex
    else
      0
  where
    tempIndex = ((index + delta) `mod` len)

changeRadius :: Maybe EnemyPeg -> Float -> Maybe EnemyPeg
changeRadius peg delta = case peg of
  Nothing -> Nothing
  Just ball ->
    if newRadius > 0
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
      if delta > 0
        then Just (ball {ballType = Destructible delta})
        else Just ball
    Destructible curDurability ->
      if newDurability > 0
        then Just (ball {ballType = Destructible newDurability})
        else Just (ball {ballType = Indestructible})
      where
        newDurability = curDurability + delta

inBoundaries :: Coords -> Float -> Bool
inBoundaries cords radius = condition
  where
    condition =
      x + radius / 2 < rightWallX emptyMap && x - radius / 2 > leftWallX emptyMap
        && y + radius / 2 < ceilingY emptyMap
        && y - radius / 2 > floorY emptyMap
      where
        (x, y) = cords

handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state {currentBackground = newBackground, currentBackgroundIndex = newLevelIndex}
  where
    stateBackgrounds = mapBackgrounds state
    numberOfLevels = length $ stateBackgrounds
    newLevelIndex = changePictureIndex (currentBackgroundIndex state) (-1) numberOfLevels
    newBackground = if numberOfLevels == 0 then
      blank
    else
      safeGetPictureById stateBackgrounds newLevelIndex

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) state =
  state {currentBackground = newBackground, currentBackgroundIndex = newLevelIndex}
  where
    stateBackgrounds = mapBackgrounds state
    numberOfLevels = length $ stateBackgrounds
    newLevelIndex = changePictureIndex (currentBackgroundIndex state) 1 numberOfLevels
    newBackground = if numberOfLevels == 0 then
      blank
    else
      safeGetPictureById stateBackgrounds newLevelIndex

-- | Update mouse position.
handleKeys (EventMotion (xPos, yPos)) state =
  state {userMousePosition = newUserMousePosition}
  where
    newUserMousePosition = (xPos, yPos)

handleKeys (EventKey (MouseButton RightButton) Down _ _) state =
  state
    { mapInfo = newMapInfo
    }
  where
    newMapInfo = oldMapInfo {enemyBalls = newEnemyBalls}
    oldMapInfo = mapInfo state

    newEnemyBalls =
      enemyBalls oldMapInfo
        ++ [ EnemyPeg (userMousePosition state) curBallRadius curBallType
             | inBoundaries (userMousePosition state) curBallRadius
           ]
    curBallRadius = maybe 10 enemyRadius curBall
    curBallType = maybe (Destructible 1) ballType curBall
    curBall = currentBall state

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
handleKeys _ state = state

emptyMap :: MapInfo
emptyMap =
  MapInfo
    { enemyBalls = [],
      cannonPosition = (0, 280),
      leftWallX = -300,
      rightWallX = 300,
      floorY = -300,
      ceilingY = 300,
      backgroundId = 0
    }


-- Get editor state from map, and meta-info
editorStateFrom :: MapInfo -> MetaInfo -> Sprites -> [String] -> MapEditorState
editorStateFrom map metaInfo gameSprites pictures =
  Game
    { currentBall = Just (EnemyPeg (0, 0) 10 (Destructible 1)),
      userMousePosition = (0, 0),
      mapInfo = map,
      metaInfo = metaInfo,
      backgroundFiles = pictures,
      mapBackgrounds = backgrounds gameSprites,
      currentBackground = safeFirstPicture (backgrounds gameSprites),
      currentBackgroundIndex = 0
    }

render ::
  MapEditorState -> -- The map state to render.
  Picture
render state =
  mapBackground <> allEnemyBalls <> textPicture <> mapPicture
  where
    mapBackground = currentBackground state
    -- Text rendering
    textPicture = translate 0 (- fromIntegral height / 2 + 20) (scale 0.2 0.2 (color green (alignedCenterText textToPrint)))
    textToPrint = "Press space to play the level"

    mapPicture = renderMap (mapInfo state)

    allEnemyBalls = renderEnemies (enemyBalls (mapInfo state) ++ maybeToList (currentBall state))

-- | Update the game by moving the ball.
update ::
  Float ->
  MapEditorState ->
  MapEditorState
update seconds state = case currentBall state of
  Nothing -> state
  Just curBall ->
    if inBoundaries (userMousePosition state) (enemyRadius curBall)
      then moveCurBall curBall seconds state
      else state

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
