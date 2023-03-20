{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Math utils
-}

module Math (
    distance  
) where

distance :: (Int, Int, Int) -> (Int, Int, Int) -> Float
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2))
