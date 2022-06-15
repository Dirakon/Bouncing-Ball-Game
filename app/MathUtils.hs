module MathUtils(vectorSum,vectorDiff) where
import Graphics.Gloss
import Types



vectorSum :: [Vector] -> Vector
vectorSum vectors = (sum (map getI vectors), sum (map getJ vectors))
  where
    getI :: Coords -> Float
    getI (i, _) = i
    getJ :: Coords -> Float
    getJ (_, j) = j

vectorDiff :: Vector -> Vector -> Vector
vectorDiff vec1 vec2 = (fst vec1 - fst vec2, snd vec1 - snd vec2)
