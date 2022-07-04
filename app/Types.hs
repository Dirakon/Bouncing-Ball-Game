{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}

module Types (Position, Velocity, Restitution, Speed, Coords, PlayerBall (..), EnemyBallType (..), EnemyPeg (..), MapInfo (..), Sprites (..), MetaInfo (..), SoundList) where

import Data.Binary
import GHC.Generics (Generic)
import Graphics.Gloss (Picture)

#ifdef SoundEnabled
import SDL.Mixer (Chunk)
type SoundList = [(String,Chunk)]
#else
type SoundList = [()]
#endif

-- | Position type.
type Position = (Float, Float)

-- | Velocity type.
type Velocity = (Float, Float)

-- | Restitution type.
type Restitution = Float

-- | Speed type.
type Speed = Float

-- | Coords type.
type Coords = (Float, Float)

-- | Game meta info.
data MetaInfo = MetaInfo
  { currentLevel :: Int, -- ^ Index of current level.
    soundList :: SoundList, -- ^ 'List' of sounds.
    soundRequestList :: [String], -- ^ 'List' of sounds that game requests to play.
    currentBackgroundTrackId :: Int, -- ^ Id of currently playing track.
    requestedBackgroundTrackId :: Int, -- ^ Id of track reuqested by game.
    sprites :: Sprites, -- ^ All sprites of game.
    preloadedLevels :: [MapInfo], -- ^ 'List' of loaded leveles from files.
    userMousePosition :: Coords -- ^ Player mouse coordinates.
  }
   deriving (Show)

-- | Player ball info.
data PlayerBall = PlayerBall
  { playerPosition :: Position, -- ^ Coordinates of player ball.
    velocity :: Velocity, -- ^ Velocity of player ball.
    restitution :: Restitution, -- ^ Resitution of player ball.
    speed :: Speed, -- ^ Speed of player ball.
    playerRadius :: Float -- ^ Radius of player ball.
  }
   deriving (
    Show -- ^ default 'Show' instance.
    ,Read -- ^ default 'Read' instance.
    )

-- | Enemy ball type.
data EnemyBallType = 
  Destructible Int -- ^ Destructible ball type with durability 'Int' value.
  | Indestructible -- ^ Indestructible ball type.
  deriving 
    (
    Eq -- ^ default 'Eq' instance.
    , Show -- ^ default 'Show' instance.
    , Read -- ^ default 'Read' instance.
    , Generic -- ^ default 'Generic' instance.
    ) 

data EnemyPeg = EnemyPeg
  { enemyPosition :: Position, -- ^ 'Coords' of 'EnemyPeg'.
    enemyRadius :: Float, -- ^ Radius of 'EnemyPeg'.
    ballType :: EnemyBallType -- ^ Type of 'EnemyPeg'.
  }
  deriving (
    Eq -- ^ default 'Eq' instance.
    , Show -- ^ default 'Show' instance.
    , Read -- ^ default 'Read' instance.
    , Generic -- ^ default 'Generic' instance.
    ) 

data Sprites = Sprites
  { cannonSprite :: Picture,
    backgrounds :: [Maybe Picture]
  }
   deriving (
    Show -- ^ default 'Show' instance.
   )

data MapInfo =MapInfo
  { enemyBalls :: [EnemyPeg], -- ^ List of enemy balls.
    cannonPosition :: Position, -- ^ Cannon position 'Coords'.
    leftWallX :: Float, -- ^ Left wall X coordinate. 
    rightWallX :: Float, -- ^ Right wall X coordinate.
    floorY :: Float, -- ^ Floor Y coordinate.
    ceilingY :: Float, -- ^ Ceiling Y coordinate.
    backgroundPictureId :: Int, -- ^ Id of background picture.
    backgroundTrackId :: Int -- ^ Id of track.
  }
  deriving (
  Show -- ^ default 'Show' instance.
  , Read -- ^ default 'Read' instance.
  , Generic -- ^ default 'Generic' instance.
  )

-- | Binary instance of 'MapInfo'.
instance Binary MapInfo

-- | Binary instance of 'EnemyPeg'.
instance Binary EnemyPeg

-- | Binary instance of 'EnemyBallType'.
instance Binary EnemyBallType