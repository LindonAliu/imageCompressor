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
} deriving (Show, Read)

instance Eq Pixel where
  (Pixel pos1 color1) == (Pixel pos2 color2) = pos1 == pos2 && color1 == color2
