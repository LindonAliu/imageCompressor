{-
-- EPITECH PROJECT, 2023
-- Algorithm
-- File description:
-- k-means++ algorithm for image compression
-}

module Algorithm
    ( parseFile
    , Pixel (..)
    ) where

import Data.Maybe

data Pixel = Pixel {
    pos :: (Int, Int),
    color :: (Int, Int, Int)
}

parseFile :: [String] -> ([Pixel], Bool)
parseFile [] = ([], True)
parseFile (x:xs) | isJust pixel = (fromJust pixel : (fst (parseFile xs)), True)
                 | otherwise = ([], False)
    where pixel = parseLine (words x)

parseLine :: [String] -> Maybe Pixel
parseLine [xy, rgb] = Just Pixel { pos = read xy, color = read rgb }
parseLine _ = Nothing
