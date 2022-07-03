{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module MapEditor where

import Consts (backgroundTracks, height)
import Data.Maybe (maybeToList)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Render (renderBackground, renderEnemies, renderMap)
import TextSizeAnalysis (alignedCenterText)
import Types (Coords, EnemyBallType (..), EnemyPeg (..), MapInfo (..), MetaInfo (requestedBackgroundTrackId, sprites), Position, Sprites (backgrounds))

-- | A data structure to hold the state of the map editor.
data MapEditorState = Game
  { currentBall :: Maybe EnemyPeg,
    mapInfo :: MapInfo,
    userMousePosition :: Position,
    metaInfo :: MetaInfo,
    backgroundFiles :: [String]
  }

changeIndex :: Int -> Int -> Int -> Int
changeIndex index delta len =
  if len /= 0
    then
      if tempIndex >= 0
        then tempIndex
        else len + tempIndex
    else 0
  where
    tempIndex = (index + delta) `mod` len

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

-- Change background picture on arrow left/right
handleKeys :: Event -> MapEditorState -> MapEditorState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgrounds $sprites $metaInfo state
    numberOfLevels = length stateBackgrounds
    newMap = (mapInfo state) {backgroundPictureId = newLevelIndex}
    newLevelIndex = changeIndex (backgroundPictureId (mapInfo state)) (-1) numberOfLevels
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgrounds $sprites $metaInfo state
    numberOfLevels = length stateBackgrounds
    newMap = (mapInfo state) {backgroundPictureId = newLevelIndex}
    newLevelIndex = changeIndex (backgroundPictureId (mapInfo state)) 1 numberOfLevels
-- Change background track on arrow left/right
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgroundTracks
    numberOfTracks = length stateBackgrounds
    newMap = (mapInfo state) {backgroundTrackId = newTrackId}
    newTrackId = changeIndex (backgroundTrackId (mapInfo state)) (-1) numberOfTracks
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgroundTracks
    numberOfTracks = length stateBackgrounds
    newMap = (mapInfo state) {backgroundTrackId = newTrackId}
    newTrackId = changeIndex (backgroundTrackId (mapInfo state)) 1 numberOfTracks
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
      backgroundPictureId = 0,
      backgroundTrackId = 0
    }

-- Get editor state from map, and meta-info
editorStateFrom :: MapInfo -> MetaInfo -> [String] -> Coords -> MapEditorState
editorStateFrom map metaInfo pictures userMousePosition =
  Game
    { currentBall = Just (EnemyPeg (0, 0) 10 (Destructible 1)),
      userMousePosition = userMousePosition,
      mapInfo = map,
      metaInfo = metaInfo,
      backgroundFiles = pictures
    }

render ::
  MapEditorState -> -- The map state to render.
  Picture
render state =
  mapBackground <> mapPicture <> allEnemyBalls <> textPicture
  where
    mapBackground = renderBackground (mapInfo state) (metaInfo state)

    -- Text rendering
    textPicture = translate 0 (- fromIntegral height / 2 + 20) (scale 0.2 0.2 (color black (alignedCenterText textToPrint)))
    textToPrint = "Press space to play the level"

    mapPicture = renderMap (mapInfo state)

    allEnemyBalls = renderEnemies (enemyBalls (mapInfo state) ++ maybeToList (currentBall state))

-- | Update the game by moving the ball.
update ::
  Float ->
  MapEditorState ->
  MapEditorState
update _ state = case currentBall state of
  Nothing ->
    updateBackgroundTrack state
  Just curBall ->
    updateBackgroundTrack
      ( if inBoundaries (userMousePosition state) (enemyRadius curBall)
          then moveCurBall state
          else state
      )
  where
    updateBackgroundTrack state =
      state {metaInfo = newMetaInfo}
      where
        newMetaInfo = (metaInfo state) {requestedBackgroundTrackId = backgroundTrackId (mapInfo state)}

moveCurBall ::
  MapEditorState ->
  MapEditorState
moveCurBall
  state =
    state {currentBall = newCurrentBall}
    where
      newCurrentBall = case currentBall state of
        Nothing -> Nothing
        Just oldCurBall -> Just oldCurBall {enemyPosition = newPosition}
      newPosition = userMousePosition state
