{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module MapEditor where

import Consts (backgroundTracks, height)
import Data.Maybe (maybeToList)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Render (renderBackground, renderEnemies, renderMap)
import TextSizeAnalysis (alignedCenterText)
import Types (Coords, EnemyBallType (..), EnemyPeg (..), MapInfo (..), MetaInfo (requestedBackgroundTrackId, sprites, userMousePosition), Position, Sprites (backgrounds))

-- | A data structure to hold the state of the map editor.
data MapEditorState = Game
  { currentBall :: Maybe EnemyPeg, -- ^ Current ball following mouse.
    mapInfo :: MapInfo, -- ^ Info about current map.
    metaInfo :: MetaInfo, -- ^ Meta info about game (Mouse position, etc.).
    backgroundFiles :: [String] -- ^ File names of backgrounds.
  }deriving (Show)

-- | Retrun index after performing and addition 
-- operation with delta
changeIndex :: 
  Int -- ^ Current index.
  -> Int -- ^ Delta for addition.
  -> Int -- ^ Current length of 'backgroundFiles'.
  -> Int -- ^ New index.
changeIndex index delta len =
  if len /= 0
    then
      if tempIndex >= 0
        then tempIndex
        else len + tempIndex
    else 0
  where
    tempIndex = (index + delta) `mod` len

-- | Change radius of ball on delta.
changeRadius :: 
  Maybe EnemyPeg -- ^ Current ball.
  -> Float -- ^ Delta.
  -> Maybe EnemyPeg -- ^ New ball with changed radius.
changeRadius peg delta = case peg of
  Nothing -> Nothing
  Just ball ->
    if newRadius > 0
      then Just (ball {enemyRadius = currentRadius + delta})
      else Just (ball {enemyRadius = currentRadius})
    where
      currentRadius = enemyRadius ball
      newRadius = currentRadius + delta

-- | Change durability of ball with its type.
changeBallType :: 
  Maybe EnemyPeg -- ^ Current ball.
  -> Int -- ^ Delta.
  -> Maybe EnemyPeg -- ^ New ball with changed durability.
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

-- | Check weather ball with 'Coords' is in boundaries.
inBoundaries :: 
  Coords -- ^ Coordinates to check. 
  -> Float -- ^ Radidius of ball.
  -> Bool -- ^ Result value of check.
inBoundaries cords radius = condition
  where
    condition =
      x + radius / 2 < rightWallX emptyMap && x - radius / 2 > leftWallX emptyMap
        && y + radius / 2 < ceilingY emptyMap
        && y - radius / 2 > floorY emptyMap
      where
        (x, y) = cords

handleKeys :: 
  Event -- ^ Event.
  -> MapEditorState -- ^ Current state of map editor.
  -> MapEditorState -- ^ New map state of map editor.

-- | Change background on previous picture with arrow Left.
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgrounds $sprites $metaInfo state
    numberOfLevels = length stateBackgrounds
    newMap = (mapInfo state) {backgroundPictureId = newLevelIndex}
    newLevelIndex = changeIndex (backgroundPictureId (mapInfo state)) (-1) numberOfLevels

-- | Change background on next picture with arrow Right.
handleKeys (EventKey (SpecialKey KeyRight) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgrounds $sprites $metaInfo state
    numberOfLevels = length stateBackgrounds
    newMap = (mapInfo state) {backgroundPictureId = newLevelIndex}
    newLevelIndex = changeIndex (backgroundPictureId (mapInfo state)) 1 numberOfLevels

-- | Change music on previous with arrow Up.
handleKeys (EventKey (SpecialKey KeyUp) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgroundTracks
    numberOfTracks = length stateBackgrounds
    newMap = (mapInfo state) {backgroundTrackId = newTrackId}
    newTrackId = changeIndex (backgroundTrackId (mapInfo state)) (-1) numberOfTracks

-- | Change music on next with arrow Down.
handleKeys (EventKey (SpecialKey KeyDown) Down _ _) state =
  state {mapInfo = newMap}
  where
    stateBackgrounds = backgroundTracks
    numberOfTracks = length stateBackgrounds
    newMap = (mapInfo state) {backgroundTrackId = newTrackId}
    newTrackId = changeIndex (backgroundTrackId (mapInfo state)) 1 numberOfTracks

-- | Change current ball position on mouse move.
handleKeys (EventMotion (xPos, yPos)) state =
  state {metaInfo = (metaInfo state){userMousePosition = newUserMousePosition}}
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
        ++ [ EnemyPeg (userMousePosition (metaInfo state)) curBallRadius curBallType
             | inBoundaries (userMousePosition (metaInfo state)) curBallRadius
           ]
    curBallRadius = maybe 10 enemyRadius curBall
    curBallType = maybe (Destructible 1) ballType curBall
    curBall = currentBall state

-- | Remove ball on Left mouse button click.
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
        (mouseX, mouseY) = userMousePosition $ metaInfo state

-- | Increased the durability or size according
-- to pressed shift or not.
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

-- | Decrease the size or durability according
-- to pressed shift or not.
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

-- | Empty map info.
emptyMap :: MapInfo
emptyMap = MapInfo
    { enemyBalls = [], -- List of enemy balls.
      cannonPosition = (0, 280), -- Position of cannon.
      leftWallX = -300, -- Left wall X 'Coords'.
      rightWallX = 300, -- Right wall X 'Coords'.
      floorY = -300, -- Floor Y 'Coords'.
      ceilingY = 300, -- Ceiling Y 'Coords'.
      backgroundPictureId = 0, -- Id of background 'Picture'.
      backgroundTrackId = 0 -- Id of track.
    } 

-- | Get editor state from map, and meta-info.
editorStateFrom :: 
  MapInfo -- ^ Input 'MapInfo'.
  -> MetaInfo -- ^ Input 'MetaInfo'.
  -> [String]  -- ^ Input array of picture names.
  -> MapEditorState -- ^ Output 'MapEditorState'.
editorStateFrom map metaInfo pictures =
  Game
    { currentBall = Just (EnemyPeg (0, 0) 10 (Destructible 1)),
      mapInfo = map,
      metaInfo = metaInfo,
      backgroundFiles = pictures
    }

-- | Renderer of 'MapEditorState'.
render ::
  MapEditorState -- ^ The map state to render.
  -> Picture -- ^ The output picture.
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
  Float 
  -> MapEditorState -- ^ Input 'MapEditorState'.
  -> MapEditorState -- ^ Output 'MapEditorState'.
update _ state = case currentBall state of
  Nothing ->
    updateBackgroundTrack state
  Just curBall ->
    updateBackgroundTrack
      ( if inBoundaries (userMousePosition (metaInfo state)) (enemyRadius curBall)
          then moveCurBall state
          else state
      )
  where
    updateBackgroundTrack state =
      state {metaInfo = newMetaInfo}
      where
        newMetaInfo = (metaInfo state) {requestedBackgroundTrackId = backgroundTrackId (mapInfo state)}

-- | Move current ball using current mouse position.
moveCurBall ::
  MapEditorState -- ^ 'MapEditorState' before move.
  -> MapEditorState -- ^ 'MapEditorState' after move.
moveCurBall
  state =
    state {currentBall = newCurrentBall}
    where
      newCurrentBall = case currentBall state of
        Nothing -> Nothing
        Just oldCurBall -> Just oldCurBall {enemyPosition = newPosition}
      newPosition = userMousePosition $ metaInfo state
