{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

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
import MapEditor (MapEditorState)
import qualified MapEditor
import System.Directory (doesFileExist)
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

data FullGameState = GameOn (GameState, MapInfo, Int) | EditorOn (MapEditorState, Int)

main :: IO ()
main = do
  level <- loadLevel (getLevelPath 0)
  sprites <- loadSprites
  let initialFullState = GameOn (Game.initialStateFrom level sprites,level, 0)
  playIO window black fps initialFullState render handleKeys update
  where
    -- Go to play or editor mode on 'space' pressed
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (GameOn (gameState, map, currentLevel)) = do
      return $ EditorOn (MapEditor.editorStateFrom map (sprites gameState), currentLevel)
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (EditorOn (editorState, currentLevel)) = do
      saveLevel (getLevelPath currentLevel) map
      return $ GameOn (Game.initialStateFrom map sprites, map, currentLevel)
      where
        map = MapEditor.thisMapInfo editorState
        sprites = MapEditor.thisSprites editorState

    -- Go to next level in play mode on 'enter' being pressed with no balls on the map
    handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) oldState@(GameOn (gameState, map, currentLevel)) = do
      if noEnemyBallsLeft
        then do
          nextLevel <- loadLevel (getLevelPath (currentLevel + 1))
          return (newState nextLevel gameState)
        else do
          return oldState
      where
        newState nextLevel gameState =
          GameOn (Game.initialStateFrom nextLevel (sprites gameState), nextLevel, currentLevel + 1)

        noEnemyBallsLeft = case listToMaybe (enemyBalls (mapInfo (metaInfo gameState))) of
          Nothing -> True
          _ -> False

    -- Choose handleKeys function depending on current mode
    handleKeys event (GameOn (gameState, map, currentLevel)) =
      return $ GameOn (Game.handleKeys event gameState, map, currentLevel)
    handleKeys event (EditorOn (editorState, currentLevel)) =
      return $ EditorOn (MapEditor.handleKeys event editorState, currentLevel)

    -- Choose render function depending on current mode
    render (EditorOn (mapEditorState, _)) = return $ MapEditor.render mapEditorState
    render (GameOn (gameState, map, _)) = return $ Game.render gameState

    -- Choose update function depending on current mode
    update dt (EditorOn (mapEditorState, currentLevel)) =
      return $ EditorOn (MapEditor.update dt mapEditorState, currentLevel)
    update dt (GameOn (gameState, map, currentLevel)) =
      return $ GameOn (Game.update dt gameState, map, currentLevel)

getLevelPath :: Int -> FilePath
getLevelPath levelIndex = "levels/level" ++ show levelIndex