module Consts where

import Graphics.Gloss
import Types

-- | Screen settings | --
width, height, offset :: Int
width = 700
height = 700
offset = 100

background :: Color
background = black

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | RenderingConsts
wallColor = dark green
enemyBallColor  = dark red
playerBallColor  =  dark white

-- | Initial player ball simulation settings | --
startPlayerRestitution :: Restitution
startPlayerRestitution = 6

startPlayerSpeed :: Speed
startPlayerSpeed = 450

startPlayerRadius :: Float
startPlayerRadius = 10

-- | Simulation settings | --
gravity :: Vector
gravity = (0, -6) --(0.-6)

playerOtsckokCoefficient :: Float
playerOtsckokCoefficient = 1

playerVelocityLoseCoefficient :: Float
playerVelocityLoseCoefficient = 0.05


nonMovingPlayerThreshold :: Float
nonMovingPlayerThreshold = 1