{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Types
-}

module Types
    ( Color
    , getRGB
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

getRGB :: Color -> Int -> Int
getRGB (r, _, _) 0 = r
getRGB (_, g, _) 1 = g
getRGB (_, _, b) 2 = b
getRGB _ _ = 0