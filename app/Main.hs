module Main (main) where
import Types
import Graphics.Gloss
import Consts
import qualified MapEditor
import qualified Game
import MapEditor (MapEditorState)
import Graphics.Gloss.Interface.IO.Game



window :: Display
window = InWindow "Game" (width, height) (offset, offset)

data FullGameState = GameOn GameState | EditorOn MapEditorState
main :: IO ()
--main = play window background fps initialState render handleKeys update
main = play window black fps initialFullState render handleKeys update
  where
    initialFullState = EditorOn MapEditor.initialMapEditorState
    render (EditorOn mapEditorState) = MapEditor.render mapEditorState
    render (GameOn gameState) = Game.render gameState

    update dt (EditorOn mapEditorState) =EditorOn (MapEditor.update dt mapEditorState)
    update dt (GameOn gameState) = GameOn( Game.update dt gameState)

    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (GameOn gameState) = EditorOn MapEditor.initialMapEditorState
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (EditorOn editorState) = GameOn (Game.initialStateFromMap (MapEditor.thisMapInfo editorState))

    handleKeys event (GameOn gameState) = GameOn (Game.handleKeys event gameState)
    handleKeys event (EditorOn editorState) = EditorOn(MapEditor.handleKeys event editorState)
