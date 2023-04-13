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

import Algorithm (parseFile)
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
        else putStrLn "File successfully parsed"
