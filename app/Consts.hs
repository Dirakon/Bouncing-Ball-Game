{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module Consts where

import Graphics.Gloss
import Types

-- | Screen settings | --
width, height, offset :: Int
width = 700
height = 700
offset = 100

pngBackgrounds :: [String]
pngBackgrounds = ["sprites/level1.png", "sprites/level2.png", "sprites/level3.png"]

backgroundTracks :: [String]
backgroundTracks = ["582065__lagmusics__8bit-looping", "JeffSpeed68_-_The_Vendetta", "septahelix_-_Tocharian_C", "sparky_-_Toi_encore_(encore)"]

-- | Number of frames to show per second.
fps :: Int
fps = 60

-- | RenderingConsts
wallColor :: Color
wallColor = dark green
enemyBallColor :: Color
enemyBallColor  = dark red
playerBallColor :: Color
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
playerOtsckokCoefficient = 0.5

playerVelocityLoseCoefficient :: Float
playerVelocityLoseCoefficient = 0.05


nonMovingPlayerThreshold :: Float
nonMovingPlayerThreshold = 1.5