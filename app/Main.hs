{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Main where

import Consts
import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy as ByteStringLazy hiding (map)
import Data.Maybe
import DevUtils (updateMaps)
import Game (GameState)
import qualified Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy (loadJuicyPNG)
import MapEditor (emptyMap)
import qualified MapEditor
import Sounds
import System.Directory (doesFileExist)
import Types

-- | Gloss window configuration.
window :: Display
window = InWindow "Bouncing Crusher" (width, height) (offset, offset)

-- | Load multiple PNGs with gloss-juicy
loadJuicyPNGS :: [FilePath] -> [IO (Maybe Picture)]
loadJuicyPNGS = map loadJuicyPNG

listFromIO :: (Traversable t, Monad m) => t (m a) -> m (t a)
listFromIO = sequence

-- | Load all sprites from files into Sprites.
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

-- | Save level by path.
saveLevel :: FilePath -> MapInfo -> IO ()
saveLevel fileName map = do
  ByteStringLazy.writeFile fileName (encode map)

-- | Load levels sequentially from some index.
-- If a level cannot be loaded, the procedure is stopped and level list is returned.
loadAllSequentialLevelsFrom :: Int -> IO [MapInfo]
loadAllSequentialLevelsFrom start = do
  maybeLoadedLevel <- loadLevel (getLevelPath start)
  case maybeLoadedLevel of
    Nothing -> return []
    Just level -> do
      otherLevels <- loadAllSequentialLevelsFrom (start + 1)
      return $ level : otherLevels

-- | Try load level by path.
loadLevel :: FilePath -> IO (Maybe MapInfo)
loadLevel fileName = do
  levelFileExists <- doesFileExist fileName
  if not levelFileExists
    then do
      return Nothing
    else do
      maybeDecodedMap <- (decodeFileOrFail fileName :: IO (Either (ByteOffset, String) MapInfo))
      case maybeDecodedMap of
        Left _ -> return Nothing
        Right decodedMap -> return $ Just decodedMap

-- | Try load the level only if preloaded levels have been exhausted.
-- If level cannot be loaded, load empty map.
-- Return new level and updated metaInfo.
loadLevelIfUnloaded :: MetaInfo -> IO (MetaInfo, MapInfo)
loadLevelIfUnloaded oldMetaInfo = do
  case listToMaybe (preloadedLevels oldMetaInfo) of
    Just neededLevel -> do
      print neededLevel
      return
        ( oldMetaInfo
            { currentLevel = currentLevel oldMetaInfo + 1,
              preloadedLevels = Prelude.drop 1 (preloadedLevels oldMetaInfo)
            },
          neededLevel
        )
    Nothing -> do
      maybeLoadedLevel <- loadLevel (getLevelPath (currentLevel oldMetaInfo + 1))
      return
        ( oldMetaInfo {currentLevel = currentLevel oldMetaInfo + 1},
          fromMaybe emptyMap maybeLoadedLevel
        )

-- | Game state that represents if the game is in MapEditor or in Play Mode.
data GameState = GameOn Game.GameState | EditorOn MapEditor.MapEditorState deriving (Show)

main :: IO ()
main = do
  initSounds
  let initialLevelIndex = 0

  levels <- loadAllSequentialLevelsFrom initialLevelIndex
  print levels
  let firstLevel = fromMaybe emptyMap (listToMaybe levels)

  sprites <- loadSprites

  let initialMetaInfo = MetaInfo initialLevelIndex [] [] (-1) (-1) sprites (Prelude.drop 1 levels) (0, 0)

  let initialFullState = GameOn (Game.initialStateFrom firstLevel initialMetaInfo)
  playIO window black fps initialFullState render handleKeys update
  closeSounds
  where
    -- Change mode on 'space' pressed
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (GameOn gameState) = do
      newMetaInfo <- playRequestedSounds $ addSoundToRequests "change_mode" (Game.metaInfo gameState)
      return $ EditorOn (MapEditor.editorStateFrom (Game.initialMap gameState) newMetaInfo pngBackgrounds)
    handleKeys (EventKey (SpecialKey KeySpace) Down _ _) (EditorOn editorState) = do
      newMetaInfo <- playRequestedSounds $ addSoundToRequests "change_mode" (MapEditor.metaInfo editorState)
      saveLevel (getLevelPath (currentLevel newMetaInfo)) map
      return $ GameOn (Game.initialStateFrom map newMetaInfo)
      where
        map = MapEditor.mapInfo editorState

    -- Go to next level in play mode on 'enter' being pressed with no balls on the map
    handleKeys (EventKey (SpecialKey KeyEnter) Down _ _) oldState@(GameOn gameState) = do
      -- setTextSizes
      if Game.allDestroyableBallsAreDestroyed gameState
        then do
          (newMetaInfo, nextLevel) <- loadLevelIfUnloaded metaInfo
          newState nextLevel newMetaInfo
        else do
          return oldState
      where
        metaInfo = Game.metaInfo gameState
        newState nextLevel newMetaInfo = do
          -- TODO: same track unloading
          return $ GameOn (Game.initialStateFrom nextLevel (newMetaInfo {currentBackgroundTrackId = -1}))

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

-- | Convert level index to filePath
getLevelPath :: Int -> FilePath
getLevelPath levelIndex = "levels/level" ++ show levelIndex

-- | Update sound requests by adding one sounds, and return new MetaInfo
addSoundToRequests :: String -> MetaInfo -> MetaInfo
addSoundToRequests sound metaInfo =
  metaInfo {soundRequestList = sound : soundRequestList metaInfo}