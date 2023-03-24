{-
-- EPITECH PROJECT, 2022
-- ImageCompressor
-- File description:
-- Arguments
-}

module Args
    ( Sample (..)
    , getSample
    ) where

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
