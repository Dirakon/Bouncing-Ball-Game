module Render where

import Consts
import Graphics.Gloss
import TextSizeAnalysis
import Types

renderMap :: MapInfo -> Picture
renderMap currentMapInfo = leftWall <> rightWall <> ceiling
  where
    leftWall =
      translate (curLeftX - wallWidth / 2) (curFloorY + wallHeight / 2) $
        color wallColor $ rectangleSolid wallWidth wallHeight
    rightWall =
      translate (curRightX + wallWidth / 2) (curFloorY + wallHeight / 2) $
        color wallColor $ rectangleSolid wallWidth wallHeight
    ceiling =
      translate 0 (curCeilingY + ceilWidth / 2) $
        color wallColor $ rectangleSolid ceilLength ceilWidth
    curLeftX = leftWallX currentMapInfo
    curRightX = rightWallX currentMapInfo
    curFloorY = floorY currentMapInfo
    curCeilingY = ceilingY currentMapInfo
    wallWidth = 45
    ceilWidth = 45
    wallHeight = curCeilingY - curFloorY + ceilWidth
    ceilLength = curRightX - curLeftX + 3 * wallWidth

renderEnemies :: [EnemyPeg] -> Picture
renderEnemies pegs = pictures (map drawEnemyBall pegs)
  where
    drawEnemyBall :: EnemyPeg -> Picture
    drawEnemyBall (EnemyPeg (x, y) radius enemyType) = translate x y (color enemyBallColor $ circleSolid radius) <> texTT
      where
        texTT = translate x (y + radius / 5) $ scale mult mult (color green (alignedCenterText textToPrint))
          where
            textToPrint = case enemyType of
              Indestructible -> "Inf"
              Destructible t -> show t
            mult = radius / 100 / sqrt (fromIntegral (length textToPrint))

renderPlayer :: PlayerBall -> Picture
renderPlayer (PlayerBall (x, y) _ _ _ radius) = translate x y $ color playerBallColor $ circleSolid radius
