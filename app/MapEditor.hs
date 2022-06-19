module MapEditor where

import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Types (Coords, EnemyBallType (..), EnemyPeg (..), MapInfo (..), Position, Sprites)
import Consts (width, height)

-- | A data structure to hold the state of the map editor.
data MapEditorState = Game
  { currentBall :: Maybe EnemyPeg,
    mapInfo :: MapInfo,
    userMousePosition :: Position,
    sprites :: Sprites
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

-- Update mouse position
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
    newEnemyBalls = enemyBalls oldMapInfo ++ [EnemyPeg (userMousePosition state) currentRadius (Destructible 1)]
    currentRadius = case currentBall state of
      Nothing -> 10
      Just ball -> enemyRadius ball
handleKeys (EventKey (MouseButton WheelUp) _ _ _) state =
  state
    { currentBall = newCurrentBall
    }
  where
    oldCurBall = currentBall state
    newCurrentBall = changeRadius oldCurBall 1
handleKeys (EventKey (MouseButton WheelDown) _ _ _) state =
  state
    { currentBall = newCurrentBall
    }
  where
    oldCurBall = currentBall state
    newCurrentBall = changeRadius oldCurBall (-1)

-- Do nothing for all other events.
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

-- Get editor state from map and sprites
editorStateFrom :: MapInfo -> Sprites -> MapEditorState
editorStateFrom map sprites =
  Game
    { currentBall = Just (EnemyPeg (0, 0) 10 (Destructible 1)),
      userMousePosition = (0, 0),
      mapInfo = map,
      sprites = sprites
    }

render ::
  MapEditorState -> -- The map state to render.
  Picture
render state =
  ball <> pictures allEnemyBalls <> textPicture
  where
    -- Text rendering
    textPicture = translate (- fromIntegral width / 2) (- fromIntegral height / 2 + 30) (scale 0.2 0.2 (color green (text textToPrint)))
    textToPrint = "Press space to play the level"

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
  Just curBall -> moveCurBall curBall seconds state

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
