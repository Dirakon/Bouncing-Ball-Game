module Types where

type Position = (Float, Float)
type Velocity = (Float, Float)
type Restitution = Float
type Speed = Float
type Coords = (Float, Float)

data PlayerBall = PlayerBall Position Velocity Restitution Speed

data EnemyBall = EnemyBall Position

data MetaInfo = MetaInfo {
  ballsLeft :: Int,
  mousePosition :: Position,
  cannonPosition :: Position,
  leftWallX :: Float,
  rightWallX :: Float,
  floorY :: Float,
  ceilingY :: Float
}

-- | A data structure to hold the state of the game.
data GameState
  = Game
  {
    mainBall :: Maybe PlayerBall,
    enemyBalls :: [EnemyBall],
    metaInfo :: MetaInfo
  }
