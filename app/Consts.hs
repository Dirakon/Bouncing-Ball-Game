{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module Consts where

import Graphics.Gloss
import Types

-- | Screen settings | --
width, height, offset :: Int

-- | Width of player screen.
width = 700 

-- | Height of player screen.
height = 700 

-- | Offset of player screen.
offset = 100 

-- | 'List' of backgrounds for levels.
pngBackgrounds :: [String]
pngBackgrounds = ["sprites/level1.png", "sprites/level2.png", "sprites/level3.png"]

-- | 'List' of music for levels.
backgroundTracks :: [String]
backgroundTracks = ["582065__lagmusics__8bit-looping", "JeffSpeed68_-_The_Vendetta", "septahelix_-_Tocharian_C", "sparky_-_Toi_encore_(encore)"]

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | RenderingConsts | --

-- | Color of walls in game.
wallColor :: Color 
wallColor = dark $ dark $ dark green

-- | Color of enemy balls.
enemyBallColor :: Color 
enemyBallColor  = dark red

-- | Color of player ball.
playerBallColor :: Color 
playerBallColor  =  dark white

-- | Initial player ball simulation settings | --
startPlayerRestitution :: Restitution
startPlayerRestitution = 6

-- | Start speed of player.
startPlayerSpeed :: Speed
startPlayerSpeed = 450

-- | Start radius of player.
startPlayerRadius :: Float
startPlayerRadius = 10

-- | Simulation settings | --
-- | Vector of gravity.
gravity :: Vector
gravity = (0, -6) --(0.-6)

-- | Coefficient for teleportaion of player after rebound.
playerOtsckokCoefficient :: Float
playerOtsckokCoefficient = 0.05

-- | Coefficient for  player vector after rebound.
playerVelocityLoseCoefficient :: Float
playerVelocityLoseCoefficient = 0.05

-- | Minimal speed to despawn player.
nonMovingPlayerThreshold :: Float
nonMovingPlayerThreshold = 1.5