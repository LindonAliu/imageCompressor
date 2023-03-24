{-
-- EPITECH PROJECT, 2023
-- Lib
-- File description:
-- Lib of project
-}

module Lib
    ( Color
    , imageCompressor
    , Position
    ) where

import Args
import Options.Applicative
import System.Exit

type Color = (Int, Int, Int)
type Position = (Int, Int)

imageCompressor :: IO ()
imageCompressor = checkArguments =<< execParser opts
    where opts = (info (getSample <**> helper)
            ( fullDesc
            <> progDesc "Compress an image"
            <> header "ImageCompressor" )) { infoFailureCode = 84 }

checkArguments :: Sample -> IO ()
checkArguments (Sample c l _) | c < 1 =
    errorMessage "Invalid number of colors"
                              | l <= 0 =
    errorMessage "Invalid convergence limit"
                              | otherwise = putStrLn "GOOD JOB"

errorMessage :: String -> IO ()
errorMessage s = putStrLn s >> exitWith (ExitFailure 84)