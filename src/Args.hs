{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Arguments
-}

module Args
    ( Sample (..)
    , getFileContent
    , getSample
    ) where

import Control.Exception (catch, IOException)
import Error (errorMessage)
import Options.Applicative

data Sample = Sample {
    colors :: Int,
    limit :: Float,
    file :: String
}

getSample :: Parser Sample
getSample = Sample
    <$> option auto ( short 'n' <> metavar "N"
    <> help "Number of colors in the final image" )
    <*> option auto
    ( short 'l' <> metavar "L" <> help "Convergence limit" )
    <*> strOption ( short 'f' <> metavar "F"
    <> help "Path to the file containing the colors of the pixels" )

getFileContent :: FilePath -> IO String
getFileContent filename = do
    content <- catch (Just <$> readFile filename) fileHandler
    case content of
        Nothing -> errorMessage("Error: file " ++ filename ++ " not found") >> return ""
        Just c -> return c

fileHandler :: IOException -> IO (Maybe String)
fileHandler _ = return Nothing