{-
-- EPITECH PROJECT, 2023
-- Math
-- File description:
-- All math functions
-}

module Math
    ( closest
    , distance
    , distanceBetweenPixels
    , randomInt
    , Pixel (..)
    , Color
    ) where

import System.Random

import Types

distance :: Color -> Color -> Float
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral (((x2 - x1) ^ (2::Int)) +
                        ((y2 - y1) ^ (2::Int)) +
                        ((z2 - z1) ^ (2::Int))))

distanceBetweenPixels :: Pixel -> Pixel -> Float
distanceBetweenPixels p1 p2 = distance (color p1) (color p2)

closest :: [Pixel] -> Pixel -> Pixel
closest [] _ = Pixel { pos = (0, 0), color = (0, 0, 0) }
closest (x:xs) y =
    foldl (\acc f -> 
        if distanceBetweenPixels f y < distanceBetweenPixels acc y 
            then f 
        else acc
    ) x xs

randomInt :: Int -> IO Int
randomInt n = getStdRandom (randomR (0, n-1))
