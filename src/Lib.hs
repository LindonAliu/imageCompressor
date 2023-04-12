{-
-- EPITECH PROJECT, 2023
-- Lib
-- File description:
-- Lib of project
-}

module Lib
    ( imageCompressor
    ) where

import Args
import Algorithm (parseFile, algorithm)
import Error (errorMessage)
import Options.Applicative

imageCompressor :: IO ()
imageCompressor = checkArguments =<< execParser opts
    where opts = (info (getSample <**> helper)
            ( fullDesc
            <> progDesc "Compress an image"
            <> header "ImageCompressor" )) { infoFailureCode = 84 }

checkArguments :: Sample -> IO ()
checkArguments (Sample c l f) | c < 1 =
    errorMessage "Invalid number of colors"
                              | l <= 0 =
    errorMessage "Invalid convergence limit"
                              | otherwise = do
    content <- getFileContent f
    let pixels = parseFile (lines content)
    if length pixels == 0 || not (snd pixels)
        then errorMessage "Invalid file"
        else algorithm (fst pixels) c l
