{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Output
-}

module Output
    ( printCluster
    ) where

import Types

printElem :: Show a => a -> IO ()
printElem x = putStr . show $ x

printCluster :: Color -> [Pixel] -> IO ()
printCluster center pixels = putStrLn "--" <* print center
    <* putStrLn "-" <* printPixels pixels

printPixels :: [Pixel] -> IO ()
printPixels [] = pure ()
printPixels (x:xs) = printElem (pos x) <* putStr " "
    <* print (color x) <* (printPixels xs)
