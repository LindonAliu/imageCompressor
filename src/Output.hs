{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Output
-}

module Output
    ( printImage
    ) where

import Types

printElem :: Show a => a -> IO ()
printElem x = putStr . show $ x

printImage :: [(Pixel, [Pixel])] -> IO ()
printImage [] = pure ()
printImage ((c, arr):xs) = printCluster (color c) arr <* printImage xs 

printCluster :: Color -> [Pixel] -> IO ()
printCluster center pixels = putStrLn "--" <* print center
    <* putStrLn "-" <* printPixels pixels

printPixels :: [Pixel] -> IO ()
printPixels [] = pure ()
printPixels (x:xs) = printElem (pos x) <* putStr " "
    <* print (color x) <* (printPixels xs)
