module MapEditor where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Types(Position, Coords, EnemyBallType(..), EnemyPeg(..), MapInfo(..))
-- | A data structure to hold the state of the map.
data MapEditorState = Game
    { 
        currentBall :: Maybe EnemyPeg,
        thisMapInfo :: MapInfo,
        userMousePosition :: Position
    }

changeRadius :: Maybe EnemyPeg -> Float -> Maybe EnemyPeg
changeRadius peg delta = case peg of
                Nothing -> Nothing
                Just ball -> case newRadius > 0 of
                    True -> Just (ball {enemyRadius = currentRadius + delta})
                    False -> Just (ball {enemyRadius = currentRadius})
                    where
                        currentRadius = enemyRadius ball
                        newRadius = currentRadius + delta

-- Update mouse position in meta info
handleKeys (EventMotion (xPos, yPos)) state =
  state {userMousePosition = newUserMousePosition}
  where
    newUserMousePosition = (xPos, yPos)

handleKeys (EventKey (MouseButton RightButton) Down _ _) state =
  state {
    thisMapInfo = newMapInfo
    }
  where
    newMapInfo = oldMapInfo {enemyBalls = newEnemyBalls}
    oldMapInfo = thisMapInfo state
    newEnemyBalls = (enemyBalls oldMapInfo) ++ [EnemyPeg (userMousePosition state) currentRadius (Destructible 1)]
    currentRadius = case currentBall state of
        Nothing -> 10
        Just ball -> enemyRadius ball

handleKeys (EventKey (MouseButton WheelUp) _ _ _) state = 
    state {
        currentBall = newCurrentBall
    }
        where 
            oldCurBall = currentBall state
            newCurrentBall = changeRadius oldCurBall 1

handleKeys (EventKey (MouseButton WheelDown) _ _ _) state = 
    state {
        currentBall = newCurrentBall
    }
        where 
            oldCurBall = currentBall state
            newCurrentBall = changeRadius oldCurBall (-1)

-- Do nothing for all other events.
handleKeys _ state = state


initialMapEditorState :: MapEditorState
initialMapEditorState = Game {
    currentBall = Just (EnemyPeg (0, 0) 10 (Destructible 1)),
    userMousePosition = (0, 0),
    thisMapInfo = MapInfo
    {
        enemyBalls =
        [],
        cannonPosition = (0, 300),
        leftWallX = -300,
        rightWallX = 300,
        floorY = -300,
        ceilingY = 300
    } 
}

render ::
  MapEditorState -> -- The map state to render.
  Picture
render state = 
    pictures [ball] <> pictures allEnemyBalls
    where
      allEnemyBalls = map drawEnemyBall (enemyBalls (thisMapInfo  state))
        where
          drawEnemyBall :: EnemyPeg -> Picture
          drawEnemyBall (EnemyPeg enemyPosition radius _) = uncurry translate enemyPosition $ color red $ circleSolid radius
      ball = case currentBall state of
        Nothing -> blank
        Just (EnemyPeg enemyPosition enemyRadius _) -> uncurry translate enemyPosition $ color red $ circleSolid enemyRadius

-- | Update the game by moving the ball and bouncing off walls.
update :: 
    Float 
    -> MapEditorState
    -> MapEditorState
update seconds state = case currentBall state of
  Nothing -> state
  Just curBall -> moveCurBall curBall seconds state

moveCurBall ::
    EnemyPeg 
    -> Float
    -> MapEditorState
    -> MapEditorState
moveCurBall 
    curBall@(EnemyPeg enemyPosition enemyRadius ballType)
    seconds
    state =
        state {currentBall = newCurrentBall}
    where
        newCurrentBall = case currentBall state of
            Nothing -> Nothing
            Just oldCurBall -> Just oldCurBall {enemyPosition = newPosition}
        newPosition = userMousePosition state

tt :: Display -> Int -> IO ()
tt window fps = play window black fps initialMapEditorState render handleKeys update

