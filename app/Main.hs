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
import Foreign

import Data.Binary

import qualified Data.ByteString.Internal as S
import qualified Data.ByteString          as S
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Builder as B
import Data.ByteString.Lazy as BS
import Text.Read (readMaybe)

toByteString   :: MapInfo -> ByteString
toByteString = encode . show


fromByteString :: ByteString -> MapInfo
fromByteString bs = case readMaybe (decoded) of
  Nothing -> MapEditor.emptyMap
  Just a -> a
  where
    decoded = case decodeOrFail bs of
      Left _ -> ""
      Right (_,_,val) -> val


window :: Display
window = InWindow "Game" (width, height) (offset, offset)


saveLevel :: String->MapInfo->IO ()
saveLevel fileName map= do
  BS.writeFile fileName (toByteString map)


loadLevel:: String-> IO MapInfo
loadLevel fileName= do
    contents <- BS.readFile fileName
    return ( fromByteString contents)


data FullGameState = GameOn (GameState,MapInfo) | EditorOn MapEditorState
main :: IO ()
--main = play window background fps initialState render handleKeys update
main = do
  level <- loadLevel "level1"
  playIO  window black fps (initialFullState level)render handleKeys update
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
