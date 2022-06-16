{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
module Main (main) where
import Types
import Graphics.Gloss
import Consts
import qualified MapEditor
import qualified Game
import MapEditor (MapEditorState)
import Graphics.Gloss.Interface.IO.Game

import Data.Binary
import Data.ByteString.Lazy as ByteStringLazy
import Data.Binary.Get (ByteOffset)


window :: Display
window = InWindow "Game" (width, height) (offset, offset)


saveLevel :: FilePath->MapInfo->IO ()
saveLevel fileName map= do
  ByteStringLazy.writeFile fileName (encode map)

loadLevel:: FilePath-> IO MapInfo
loadLevel fileName= do
  maybeDecodedMap <- (decodeFileOrFail fileName :: IO (Either (ByteOffset,String) MapInfo))
  case maybeDecodedMap of
      Left _ -> return MapEditor.emptyMap
      Right decodedMap -> return decodedMap

data FullGameState = GameOn (GameState,MapInfo) | EditorOn MapEditorState

main :: IO ()
main = do
  level <- loadLevel "level1"
  playIO  window black fps (initialFullState level) render handleKeys update
  where
    initialFullState level = EditorOn (MapEditor.editorStateFromMap level)
    render (EditorOn mapEditorState) = return $MapEditor.render mapEditorState
    render (GameOn (gameState,map)) = return $Game.render gameState

    update dt (EditorOn mapEditorState) = return $EditorOn (MapEditor.update dt mapEditorState)
    update dt (GameOn (gameState,map)) = return $GameOn( Game.update dt gameState,map)

    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (GameOn (gameState,map)) =return $ EditorOn (MapEditor.editorStateFromMap map)
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (EditorOn editorState) = do
      saveLevel "level1" map
      return $GameOn (Game.initialStateFromMap map,map)
      where
        map =  MapEditor.thisMapInfo editorState

    handleKeys event (GameOn (gameState,map)) = return $GameOn (Game.handleKeys event gameState,map)
    handleKeys event (EditorOn editorState) = return $EditorOn(MapEditor.handleKeys event editorState)
