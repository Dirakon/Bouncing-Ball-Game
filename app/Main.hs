{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
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
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Data.Maybe


window :: Display
window = InWindow "Game" (width, height) (offset, offset)

loadSprites :: IO Sprites
loadSprites = do
  cannonSprite <- loadJuicyPNG "sprites/cannon.png"
  return (Sprites 
      {
        cannonSprite  = tryUse cannonSprite  
      }
    )
    where
      tryUse picture = fromMaybe blank picture


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
  sprites <- loadSprites
  playIO  window black fps (initialFullState level sprites) render handleKeys update
  where
    initialFullState level sprites = EditorOn (MapEditor.editorStateFrom level sprites)
    render (EditorOn mapEditorState) = return $MapEditor.render mapEditorState
    render (GameOn (gameState,map)) = return $Game.render gameState

    update dt (EditorOn mapEditorState) = return $EditorOn (MapEditor.update dt mapEditorState)
    update dt (GameOn (gameState,map)) = return $GameOn( Game.update dt gameState,map)

    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (GameOn (gameState,map)) = do
      return $ EditorOn (MapEditor.editorStateFrom map (sprites gameState))
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (EditorOn editorState) = do
      saveLevel "level1" map
      return $GameOn (Game.initialStateFrom map sprites,map)
      where
        map =  MapEditor.thisMapInfo editorState
        sprites = MapEditor.thisSprites editorState

    handleKeys event (GameOn (gameState,map)) = return $GameOn (Game.handleKeys event gameState,map)
    handleKeys event (EditorOn editorState) = return $EditorOn(MapEditor.handleKeys event editorState)
