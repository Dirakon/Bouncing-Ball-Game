{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}
module Types (Position, Velocity, Restitution, Speed, Coords, PlayerBall (..), EnemyBallType (..), EnemyPeg (..), MapInfo (..), Sprites (..),MetaInfo(..),SoundList) where

import Data.Binary
import GHC.Generics (Generic)
import Graphics.Gloss (Picture)
#ifdef SoundEnabled
import SDL.Mixer (Chunk)
type SoundList = [(String,Chunk)]
#else
type SoundList = [()]
#endif

type Position = (Float, Float)

type Velocity = (Float, Float)

type Restitution = Float

type Speed = Float

type Coords = (Float, Float)


data MetaInfo = MetaInfo{
  currentLevel:: Int,
  soundList :: SoundList,
  soundRequestList :: [String],
  sprites :: Sprites
}

data PlayerBall = PlayerBall
  { playerPosition :: Position,
    velocity :: Velocity,
    restitution :: Restitution,
    speed :: Speed,
    playerRadius :: Float
  }

data EnemyBallType = Destructible Int | Indestructible deriving (Eq, Show, Read, Generic)

data EnemyPeg = EnemyPeg
  { enemyPosition :: Position,
    enemyRadius :: Float,
    ballType :: EnemyBallType
  }
  deriving (Eq, Show, Read, Generic)


data Sprites = Sprites
  { cannonSprite :: Picture
  }

data MapInfo = MapInfo
  { enemyBalls :: [EnemyPeg],
    cannonPosition :: Position,
    leftWallX :: Float,
    rightWallX :: Float,
    floorY :: Float,
    ceilingY :: Float
  }
  deriving (Show, Read, Generic)

instance Binary MapInfo

instance Binary EnemyPeg

instance Binary EnemyBallType