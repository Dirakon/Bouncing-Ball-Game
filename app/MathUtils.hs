{-# OPTIONS_GHC -Wall -fno-warn-type-defaults #-}
module MathUtils (distanceBetween,vectorSum, vectorDiff, segmentCircleFirstIntersection,minimumByTotal,maximumByTotal) where

import Graphics.Gloss
import Types
import Data.List (minimumBy, maximumBy)

-- | Get distance between two points
distanceBetween :: 
  Point -- ^ Point 1
  -> Point -- ^ Point 2
  -> Float -- ^ Distance
distanceBetween (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

-- | Get sum of vector list
vectorSum :: [Vector] -> Vector
vectorSum vectors = (sum (map getI vectors), sum (map getJ vectors))
  where
    getI :: Coords -> Float
    getI (i, _) = i
    getJ :: Coords -> Float
    getJ (_, j) = j

-- | Get vector difference
vectorDiff :: Vector -> Vector -> Vector
vectorDiff vec1 vec2 = (fst vec1 - fst vec2, snd vec1 - snd vec2)

-- | Total version of minimumBy function 
minimumByTotal :: (a -> a -> Ordering) -> [a] ->  Maybe a
minimumByTotal  f [] = Nothing
minimumByTotal  f list = Just (minimumBy f list)

-- | Total version of maximumBy function 
maximumByTotal :: (a -> a -> Ordering) -> [a] ->  Maybe a
maximumByTotal f [] = Nothing
maximumByTotal f list = Just (maximumBy f list)


-- Segment-circle intersection algorithm is based on https://rosettacode.org/wiki/Line_circle_intersection#Haskell
-- | Get the first intersection of segment with circle
segmentCircleFirstIntersection ::
  Coords  -- ^ Segment start
  -> Coords -- ^ Segment end
  -> (Coords, Float) -- ^ Circle center and radius
  -> Maybe Coords -- ^ Maybe intersection closest to segment start
segmentCircleFirstIntersection pt1 pt2 circle =
  minimumByTotal compareByDistanceToPt1 (filter (go p1 p2) (lineCircleIntersection pt1 pt2 circle))
  where
    compareByDistanceToPt1 p1 p2
      | distanceBetween pt1 p1 > distanceBetween pt1 p2 = GT
      | otherwise = LT
    [p1, p2]
      | pt1 < pt2 = [pt1, pt2]
      | otherwise = [pt2, pt1]
    go (x, y) (u, v) (i, j)
      | x == u = y <= j && j <= v
      | otherwise = x <= i && i <= u

-- | Get all intersection of line with circle
lineCircleIntersection ::
  Coords -- ^ First point on the line
  -> Coords -- ^ Second point on the line
  -> (Coords, Float) -- ^ Circle center and radius
  -> [Coords] -- ^ List of intersection points
lineCircleIntersection (a1, b1) (a2, b2) ((a3, b3), r) = go delta
  where
    (x1, x2) = (a1 - a3, a2 - a3)
    (y1, y2) = (b1 - b3, b2 - b3)
    (dx, dy) = (x2 - x1, y2 - y1)
    drdr = dx * dx + dy * dy
    d = x1 * y2 - x2 * y1
    delta = r * r * drdr - d * d
    sqrtDelta = sqrt delta
    (sgnDy, absDy) = (sgn dy, abs dy)
    u1 = (d * dy + sgnDy * dx * sqrtDelta) / drdr
    u2 = (d * dy - sgnDy * dx * sqrtDelta) / drdr
    v1 = (- d * dx + absDy * sqrtDelta) / drdr
    v2 = (- d * dx - absDy * sqrtDelta) / drdr
    go x
      | 0 > x = []
      | 0 == x = [(u1 + a3, v1 + b3)]
      | otherwise = [(u1 + a3, v1 + b3), (u2 + a3, v2 + b3)]

-- | Get sign of float
sgn :: Float -> Float
sgn x
  | 0 > x = -1
  | otherwise = 1