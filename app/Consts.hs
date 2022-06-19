module Consts (width, height, offset, fps, background, startPlayerRadius, startPlayerSpeed, startPlayerRestitution, gravity, wallColor) where

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

-- | Walls color
wallColor = dark green

-- | Initial player ball simulation settings | --
startPlayerRestitution :: Restitution
startPlayerRestitution = 6

startPlayerSpeed :: Speed
startPlayerSpeed = 300

startPlayerRadius :: Float
startPlayerRadius = 10

-- | Global simulation settings | --
gravity :: Vector
gravity = (0, -6)