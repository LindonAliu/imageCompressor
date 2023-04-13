{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Types
-}

module Types
    ( Color
    , Pixel (..)
    , Position
    ) where

type Color = (Int, Int, Int)
type Position = (Int, Int)

data Pixel = Pixel {
    pos :: Position,
    color :: Color
}
