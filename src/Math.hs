{-
-- EPITECH PROJECT, 2023
-- Math
-- File description:
-- All math functions
-}

module Math
    ( closest
    , distance
    ) where

import Types

distance :: Color -> Color -> Float
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral (((x2 - x1) ^ (2::Int)) +
                        ((y2 - y1) ^ (2::Int)) +
                        ((z2 - z1) ^ (2::Int))))

closest :: [Color] -> Color -> Color
closest [] _ = (0, 0, 0)
closest (x:xs) y =
    foldl (\acc f -> if distance f y < distance acc y then f else acc) x xs

-- initColors :: Int -> [Color]
-- initColors 0 = []
-- initColors n = (0, 0, 0) : initColors (n - 1)
