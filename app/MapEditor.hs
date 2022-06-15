module MapEditor where
import Types(Position, Coords, EnemyBallType(..), EnemyPeg(..), MapInfo(..))
-- | A data structure to hold the state of the map.
data MapEditorState = Game
    { 
        currentBall :: Maybe EnemyPeg,
        thisMapInfo :: MapInfo,
        userMousePosition :: Position
    }


tt :: IO ()
tt = print "123"
