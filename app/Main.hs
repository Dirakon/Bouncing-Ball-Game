{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main (main, MetaInfo) where

import Consts
import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy as ByteStringLazy hiding (map)
import Data.Maybe
import DevUtils (updateMaps)
import qualified Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import qualified MapEditor
import Sounds
import System.Directory (doesFileExist)
import Types

window :: Display
window = InWindow "Game" (width, height) (offset, offset)

loadJuicyPNGS :: [FilePath] -> [IO (Maybe Picture)]
loadJuicyPNGS = map loadJuicyPNG

listFromIO :: (Traversable t, Monad m) => t (m a) -> m (t a)
listFromIO = sequence

loadSprites :: IO Sprites
loadSprites = do
  cannonSprite <- loadJuicyPNG "sprites/cannon.png"
  backgrounds <- listFromIO $ loadJuicyPNGS pngBackgrounds
  return
    ( Sprites
        { cannonSprite = tryUse cannonSprite,
          backgrounds = backgrounds
        }
    )
  where
    tryUse picture = fromMaybe blank picture

saveLevel :: FilePath -> MapInfo -> IO ()
saveLevel fileName map = do
  ByteStringLazy.writeFile fileName (encode map)

loadLevel :: FilePath -> IO MapInfo
loadLevel fileName = do
  levelFileExists <- doesFileExist fileName
  unless levelFileExists $
    ByteStringLazy.writeFile fileName (encode MapEditor.emptyMap)
  maybeDecodedMap <- (decodeFileOrFail fileName :: IO (Either (ByteOffset, String) MapInfo))
  case maybeDecodedMap of
    Left _ -> return MapEditor.emptyMap
    Right decodedMap -> return decodedMap

data GameState = GameOn Game.GameState | EditorOn MapEditor.MapEditorState

main :: IO ()
main = do
  initSounds
  let initialLevelIndex = 0
  level <- loadLevel (getLevelPath initialLevelIndex)
  sprites <- loadSprites

  let initialMetaInfo = MetaInfo initialLevelIndex [] [] (-1) (-1) sprites

  let initialFullState = GameOn (Game.initialStateFrom level initialMetaInfo (0, 0))
  playIO window black fps initialFullState render handleKeys update
  closeSounds
  where
    -- Change mode on 'space' pressed
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (GameOn gameState) = do
      newMetaInfo <- playRequestedSounds $ addSoundToRequests "change_mode" (Game.metaInfo gameState)
      let userMousePosition = Game.userMousePosition gameState
      return $ EditorOn (MapEditor.editorStateFrom (Game.initialMap gameState) newMetaInfo pngBackgrounds userMousePosition) 
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (EditorOn editorState) = do
      newMetaInfo <- playRequestedSounds $ addSoundToRequests "change_mode" (MapEditor.metaInfo editorState)
      let userMousePosition = MapEditor.userMousePosition editorState
      saveLevel (getLevelPath (currentLevel newMetaInfo)) map
      return $ GameOn (Game.initialStateFrom map newMetaInfo userMousePosition)
      where
        map = MapEditor.mapInfo editorState

    -- Go to next level in play mode on 'enter' being pressed with no balls on the map
    handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) oldState@(GameOn gameState) = do
      -- setTextSizes
      if Game.allDestroyableBallsAreDestroyed gameState
        then do
          nextLevel <- loadLevel (getLevelPath (currentLevel metaInfo + 1))
          return (newState nextLevel)
        else do
          return oldState
      where
        mouseCoords = Game.userMousePosition gameState
        metaInfo = Game.metaInfo gameState
        newState nextLevel =
          GameOn (Game.initialStateFrom nextLevel (metaInfo {currentLevel = currentLevel metaInfo + 1}) mouseCoords)

    -- Choose handleKeys function depending on current mode
    handleKeys event (GameOn gameState) =
      return (GameOn (Game.handleKeys event gameState))
    handleKeys event (EditorOn editorState) = do
      return (EditorOn (MapEditor.handleKeys event editorState))

    -- Choose render function depending on current mode
    render (EditorOn mapEditorState) = return $ MapEditor.render mapEditorState
    render (GameOn gameState) = return $ Game.render gameState

    -- Choose update function depending on current mode
    update dt (EditorOn mapEditorState) = do
      newMetaInfo <- playRequestedSounds $ MapEditor.metaInfo mapEditorState
      return (EditorOn (MapEditor.update dt mapEditorState {MapEditor.metaInfo = newMetaInfo}))
    update dt (GameOn gameState) = do
      newMetaInfo <- playRequestedSounds $ Game.metaInfo gameState
      return (GameOn (Game.update dt gameState {Game.metaInfo = newMetaInfo}))

getLevelPath :: Int -> FilePath
getLevelPath levelIndex = "levels/level" ++ show levelIndex

addSoundToRequests:: String -> MetaInfo -> MetaInfo
addSoundToRequests sound metaInfo = 
  metaInfo {soundRequestList = sound : soundRequestList metaInfo}