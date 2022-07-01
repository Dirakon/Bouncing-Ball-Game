{-# HLINT ignore "Use foldM" #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Sounds where

#ifdef SoundEnabled
import qualified SDL
import qualified SDL.Mixer  as Mix
import           Data.Default.Class (def)
import SDL.Mixer (Chunk)
#endif

import Control.Exception (Exception, SomeException, try)
import GHC.Base (IO (IO))
import Types (MetaInfo (soundList, soundRequestList), SoundList)

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
  newSoundList <- playAllSounds (soundList metaInfo) (soundRequestList metaInfo)
  return (metaInfo {soundList = newSoundList} {soundRequestList = []})

playAllSounds :: SoundList -> [String] -> IO SoundList
playAllSounds soundList [] = return soundList
playAllSounds soundList (soundName : others) = do

#   ifdef SoundEnabled
    newSoundList <- case lookup soundName soundList of
      Nothing-> do
          sound <- Mix.load ("sounds/"++soundName++".mp3")
          try $Mix.play sound :: IO (Either SomeException ())
          return $(soundName,sound) : soundList
      Just sound -> do
          try $Mix.play sound :: IO (Either SomeException ())
          return soundList
    playAllSounds newSoundList others
#   else
    return soundList
#   endif