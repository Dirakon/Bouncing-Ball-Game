{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldM" #-}
{-# LANGUAGE CPP #-}
module Sounds where


#ifdef SoundEnabled
import qualified SDL
import qualified SDL.Mixer  as Mix
import           Data.Default.Class (def)
import SDL.Mixer (Chunk)
#endif
import Types(MetaInfo ( soundRequestList, soundList),SoundList)

initSounds :: IO()
initSounds = do
# ifdef SoundEnabled
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  -- open device
  Mix.openAudio def 256
# endif
  return ()

closeSounds :: IO()
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
  newSoundList <- playAllSounds (soundList metaInfo) (soundRequestList metaInfo )
  return (metaInfo {soundList = newSoundList}{soundRequestList  = []})
 

playAllSounds :: SoundList -> [String] -> IO SoundList
playAllSounds soundList [] = return soundList
playAllSounds soundList (soundName:others) = do
#   ifdef SoundEnabled
    newSoundList <- case lookup soundName soundList of
      Nothing-> do
          sound <- Mix.load ("sounds/"++soundName++".mp3") --- case lookup sound soundList of 
          Mix.play sound
          return $(soundName,sound) : soundList
      Just sound -> do
          Mix.play sound
          return soundList
    playAllSounds newSoundList others
#   else
    return soundList
#   endif