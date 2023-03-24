{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Core
-}

module Lib (
    imageCompressor
) where

import Args
import Options.Applicative
import System.Exit

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