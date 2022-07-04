{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module DevUtils where

import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy as ByteStringLazy hiding (map)
import GHC.Generics (Generic)
import Types

-- | Synonym of MapInfo, used for transitioning existing map to new MapInfo format.
data NewMapInfo = MapInfo
  { enemyBalls :: [EnemyPeg],
    cannonPosition :: Position,
    leftWallX :: Float,
    rightWallX :: Float,
    floorY :: Float,
    ceilingY :: Float,
    backgroundPictureId :: Int,
    backgroundTrackId :: Int
  }
  deriving (Show, Read, Generic)

instance Binary NewMapInfo

-- | Save level in new MapInfo format.
saveNewLevel :: FilePath -> NewMapInfo -> IO ()
saveNewLevel fileName map = do
  ByteStringLazy.writeFile fileName (encode map)

-- | Load level from old MapInfo format.
loadOldLevel :: FilePath -> IO MapInfo
loadOldLevel fileName = do
  maybeDecodedMap <- (decodeFileOrFail fileName :: IO (Either (ByteOffset, String) MapInfo))
  case maybeDecodedMap of
    Left _ -> error $ "Unexpected behaviour! Cannot read old format of map: " ++ show fileName
    Right decodedMap -> return decodedMap

-- | Transition old MapInfo format into new MapInfo format.
updateVersion :: MapInfo -> NewMapInfo
updateVersion oldMap =
  DevUtils.MapInfo
    { DevUtils.enemyBalls = Types.enemyBalls oldMap,
      DevUtils.cannonPosition = Types.cannonPosition oldMap,
      DevUtils.leftWallX = Types.leftWallX oldMap,
      DevUtils.rightWallX = Types.rightWallX oldMap,
      DevUtils.floorY = Types.floorY oldMap,
      DevUtils.ceilingY = Types.ceilingY oldMap,
      DevUtils.backgroundPictureId = Types.backgroundPictureId oldMap,
      DevUtils.backgroundTrackId = Types.backgroundTrackId oldMap
    }

getLevelPath :: Int -> FilePath
getLevelPath levelIndex = "levels/level" ++ show levelIndex

-- | Update maps from X to Y: transition from MapInfo to NewMapInfo.
updateMaps :: Int -> Int -> IO ()
updateMaps firstMapToUpdate lastMapToUpdate = do
  oldMap <- loadOldLevel (getLevelPath firstMapToUpdate)
  let newMap = updateVersion oldMap
  saveNewLevel (getLevelPath firstMapToUpdate) newMap
  unless (firstMapToUpdate == lastMapToUpdate) $
    updateMaps (firstMapToUpdate + 1) lastMapToUpdate
