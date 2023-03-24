{-
-- EPITECH PROJECT, 2023
-- Lib
-- File description:
-- Error management
-}

module Error
    ( errorMessage
    ) where

import System.Exit

errorMessage :: String -> IO ()
errorMessage s = putStrLn s >> exitWith (ExitFailure 84)
