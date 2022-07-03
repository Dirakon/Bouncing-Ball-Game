{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# LANGUAGE DeriveGeneric #-}

module DevUtils where

import GHC.Generics (Generic)
import Control.Monad (unless)
import Data.Binary
import Data.Binary.Get (ByteOffset)
import Data.ByteString.Lazy as ByteStringLazy hiding (map)
import Types


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

saveNewLevel :: FilePath -> NewMapInfo -> IO ()
saveNewLevel fileName map = do
  ByteStringLazy.writeFile fileName (encode map)

loadOldLevel :: FilePath -> IO MapInfo
loadOldLevel fileName = do

  maybeDecodedMap <- (decodeFileOrFail fileName :: IO (Either (ByteOffset, String) MapInfo))
  case maybeDecodedMap of
    Left _ -> undefined -- Unexpected behaviour, runtime error is expected in DevUtils
    Right decodedMap -> return decodedMap

updateVersion :: MapInfo -> NewMapInfo
updateVersion oldMap = DevUtils.MapInfo{
    DevUtils.enemyBalls = Types.enemyBalls oldMap,
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

updateMaps :: Int->Int -> IO ()
updateMaps firstMapToUpdate lastMapToUpdate = do
    oldMap <- loadOldLevel (getLevelPath firstMapToUpdate)
    let newMap = updateVersion oldMap
    saveNewLevel (getLevelPath firstMapToUpdate) newMap 
    unless (firstMapToUpdate == lastMapToUpdate) $
        updateMaps (firstMapToUpdate +1) lastMapToUpdate
