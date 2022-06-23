{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldM" #-}
module Sounds where



import qualified SDL
import qualified SDL.Mixer  as Mix
import           Data.Default.Class (def)
import SDL.Mixer (Chunk)
import Types(MetaInfo (soundList, soundRequestList))

initSounds :: IO()
initSounds = do
  SDL.initialize [SDL.InitAudio]
  Mix.initialize [Mix.InitMP3]

  -- open device
  Mix.openAudio def 256

playRequestedSounds :: MetaInfo -> IO MetaInfo
playRequestedSounds metaInfo = do
  newSoundList <- playAllSounds (soundList metaInfo) (soundRequestList metaInfo )
  return (metaInfo {soundList = newSoundList}{soundRequestList  = []})
 

playAllSounds :: [(String,Chunk)] -> [String] -> IO [(String,Chunk)]
playAllSounds soundList [] = return soundList
playAllSounds soundList (soundName:others) = do

    newSoundList <- case lookup soundName soundList of
      Nothing-> do
          sound <- Mix.load ("sounds/"++soundName++".mp3") --- case lookup sound soundList of 
          Mix.play sound
          return $(soundName,sound) : soundList
      Just sound -> do
          Mix.play sound
          return soundList
    playAllSounds newSoundList others