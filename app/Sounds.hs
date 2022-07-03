{-# HLINT ignore "Use foldM" #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Sounds where

#ifdef SoundEnabled
import qualified SDL
import qualified SDL.Mixer  as Mix
import           Data.Default.Class (def)
import SDL.Mixer (Chunk)
#endif

import Consts (backgroundTracks)
import Control.Exception (Exception, SomeException, try)
import Control.Monad (unless)
import Data.Maybe (listToMaybe)
import GHC.Base (IO (IO))
import Types (MetaInfo (currentBackgroundTrackId, requestedBackgroundTrackId, soundList, soundRequestList), SoundList)
import qualified Data.ByteString as ByteString

initSounds :: IO ()
initSounds = do
# ifdef SoundEnabled
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  -- open device
  Mix.openAudio def 256
# endif
  return ()

closeSounds :: IO ()
closeSounds = do
# ifdef SoundEnabled
  -- close device
  Mix.closeAudio

  -- quit
  Mix.quit
  SDL.quit
# endif
  return ()

playRequestedSounds :: MetaInfo -> IO MetaInfo
playRequestedSounds metaInfo = do
  unless (requestedBackgroundTrackId metaInfo == currentBackgroundTrackId metaInfo) $
    playBackgroundMusic (requestedBackgroundTrackId metaInfo)
  newSoundList <- playAllSounds (soundList metaInfo) (soundRequestList metaInfo)
  return
    $metaInfo
      { soundList = newSoundList,
        soundRequestList = [],
        currentBackgroundTrackId = requestedBackgroundTrackId metaInfo
      }

playBackgroundMusic :: Int -> IO ()
playBackgroundMusic musicIndex = do
# ifdef SoundEnabled
  print $ "Looking for track #" ++ show musicIndex
  Mix.haltMusic
  case listToMaybe(drop musicIndex backgroundTracks) of
    Nothing -> do
      putStrLn $ "Error finding track #" ++ show musicIndex
    Just musicName -> do
      musicRaw <- ByteString.readFile ("sounds/"++ musicName ++".mp3")
      musicDecoded <- Mix.decode musicRaw
      _ <- try $Mix.playMusic Mix.Forever musicDecoded :: IO (Either SomeException ())
      return ()
# endif
  return ()

playAllSounds :: SoundList -> [String] -> IO SoundList
playAllSounds soundList [] = return soundList
playAllSounds soundList (soundName : others) = do

#   ifdef SoundEnabled
    newSoundList <- case lookup soundName soundList of
      Nothing-> do
          sound <- Mix.load ("sounds/"++soundName++".mp3")
          _ <- try $Mix.play sound :: IO (Either SomeException ())
          return $(soundName,sound) : soundList
      Just sound -> do
          _ <- try $Mix.play sound :: IO (Either SomeException ())
          return soundList
    playAllSounds newSoundList others
#   else
    return soundList
#   endif