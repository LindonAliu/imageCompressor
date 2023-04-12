{-
-- EPITECH PROJECT, 2023
-- Math
-- File description:
-- All math functions
-}

module Math
    ( closest
    , distance
    , getRandomColorInList
    , randomInt
    , getRandomPixel
    , Pixel (..)
    ) where

import System.Random

data Pixel = Pixel {
    pos :: (Int, Int),
    color :: (Int, Int, Int)
} deriving (Show, Read)

type Color = (Int, Int, Int)

distance :: Color -> Color -> Float
distance (x1, y1, z1) (x2, y2, z2) =
    sqrt (fromIntegral (((x2 - x1) ^ (2::Int)) +
                        ((y2 - y1) ^ (2::Int)) +
                        ((z2 - z1) ^ (2::Int))))

closest :: [Color] -> Color -> Color
closest [] _ = (0, 0, 0)
closest (x:xs) y =
    foldl (\acc f -> if distance f y < distance acc y then f else acc) x xs

getRandomColorInList :: [Color] -> StdGen -> Color
getRandomColorInList [] _ = (0, 0, 0)
getRandomColorInList list gen = list !! (randomInt (0, (length list) - 1) gen)

randomInt :: (Int, Int) -> StdGen -> Int
randomInt range gen = head $ randomRs range gen

getRandomPixel :: [Pixel] -> StdGen -> Pixel
getRandomPixel [] _ = Pixel { pos = (0, 0), color = (0, 0, 0) }
getRandomPixel list gen = list !! (randomInt (0, (length list) - 1) gen)
