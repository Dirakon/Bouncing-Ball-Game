{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main, MetaInfo) where

import Consts
import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy as ByteStringLazy
import Data.Maybe
import qualified Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import Graphics.UI.GLUT (Font (stringWidth), StrokeFont (Roman))
import qualified MapEditor
import Sounds
import System.Directory (doesFileExist)
import TextSizeAnalysis
import Types

window :: Display
window = InWindow "Game" (width, height) (offset, offset)

loadSprites :: IO Sprites
loadSprites = do
  cannonSprite <- loadJuicyPNG "sprites/cannon.png"
  return
    ( Sprites
        { cannonSprite = tryUse cannonSprite
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

  let initialMetaInfo = MetaInfo initialLevelIndex [] [] sprites

  let initialFullState = GameOn (Game.initialStateFrom level initialMetaInfo)
  playIO window black fps initialFullState render handleKeys update
  closeSounds
  where
    -- Change mode on 'space' pressed
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (GameOn gameState) = do
      playAllSounds [] ["change_mode"]
      return $ EditorOn (MapEditor.editorStateFrom (Game.initialMap gameState) (Game.metaInfo gameState))
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (EditorOn editorState) = do
      playAllSounds [] ["change_mode"]
      saveLevel (getLevelPath (currentLevel metaInfo)) map
      return $ GameOn (Game.initialStateFrom map metaInfo)
      where
        map = MapEditor.mapInfo editorState
        metaInfo = MapEditor.metaInfo editorState

    -- Go to next level in play mode on 'enter' being pressed with no balls on the map
    handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) oldState@(GameOn gameState) = do
      -- setTextSizes
      if Game.allDestroyableBallsAreDestroyed gameState
        then do
          nextLevel <- loadLevel (getLevelPath (currentLevel metaInfo + 1))
          return (newState nextLevel gameState)
        else do
          return oldState
      where
        metaInfo = Game.metaInfo gameState
        newState nextLevel gameState =
          GameOn (Game.initialStateFrom nextLevel (metaInfo {currentLevel = currentLevel metaInfo + 1}))

        noEnemyBallsLeft = case listToMaybe (enemyBalls (Game.mapInfo gameState)) of
          Nothing -> True
          _ -> False

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