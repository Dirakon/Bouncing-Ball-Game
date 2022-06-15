module Types(Position, Velocity, Restitution, Speed,Coords, PlayerBall(..), EnemyBallType(..), EnemyPeg(..), MetaInfo(..)) where

type Position = (Float, Float)

type Velocity = (Float, Float)

type Restitution = Float

type Speed = Float

type Coords = (Float, Float)

data PlayerBall = PlayerBall
  { playerPosition :: Position,
    velocity :: Velocity,
    restitution :: Restitution,
    speed :: Speed,
    playerRadius :: Float
  }

data EnemyBallType = Destructible Int | Indestructible deriving (Eq)

data EnemyPeg = EnemyPeg
 { enemyPosition :: Position,
   enemyRadius :: Float,
   ballType :: EnemyBallType
 }
  deriving (Eq)

data MetaInfo = MetaInfo
  { ballsLeft :: Int,
    mousePosition :: Position,
    cannonPosition :: Position,
    leftWallX :: Float,
    rightWallX :: Float,
    floorY :: Float,
    ceilingY :: Float
  }