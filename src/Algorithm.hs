{-
-- EPITECH PROJECT, 2023
-- Algorithm
-- File description:
-- k-means++ algorithm for image compression
-}

module Algorithm
    ( parseFile
    , algorithm
    ) where

import Data.List (delete)
import Data.Maybe
import System.Random (getStdRandom, randomR)

import Math ( closest
            , distanceBetweenPixels
            , randomInt
            )

import Types

parseFile :: [String] -> ([Pixel], Bool)
parseFile [] = ([], True)
parseFile (x:xs) | isJust pixel = (fromJust pixel : (fst (parseFile xs)), True)
                 | otherwise = ([], False)
    where pixel = parseLine (words x)

parseLine :: [String] -> Maybe Pixel
parseLine [xy, rgb] = Just Pixel { pos = read xy, color = read rgb }
parseLine _ = Nothing

getRandomPixelInList :: [Pixel] -> IO Pixel
getRandomPixelInList pixels = do
  randomIndex <- randomInt (length pixels)
  let randomPixel = pixels !! randomIndex
  let remainingPixels = delete randomPixel pixels
  return randomPixel

sampleFromDistribution :: [Float] -> IO Int
sampleFromDistribution distribution = do
  num <- getStdRandom (randomR (0, 1))
  return (loopIndex num 0 (head distribution) (tail distribution))

loopIndex :: Int -> Int -> Float -> [Float] -> Int
loopIndex _ index _ [] = index
loopIndex num index acc (d:ds)
  | acc > (fromIntegral num) = index
  | otherwise = loopIndex num (index + 1) (acc + d) ds

chooseInitialCentersSecond :: Int -> [Pixel] -> [Pixel] -> [Float] -> IO [Pixel]
chooseInitialCentersSecond k centers pixels squaredDistances
  | length centers == k = return centers
  | otherwise = computeInitialCentersSecond k centers pixels squaredDistances

computeInitialCentersSecond :: Int -> [Pixel] -> [Pixel] -> [Float] -> IO [Pixel]
computeInitialCentersSecond k centers pixels squaredDistances = do
      let distribution = map (\x -> x / sum squaredDistances) squaredDistances
      newCenterIndex <- sampleFromDistribution distribution
      let newCenter = pixels !! newCenterIndex
      let newCenters = centers ++ [newCenter]
      let remaining = delete newCenter pixels
      let newDistances = map (distanceBetweenPixels newCenter) remaining
      let newSquaredDistances = map (^ 2) newDistances
      chooseInitialCentersSecond k newCenters remaining newSquaredDistances

chooseInitialCenters :: Int -> [Pixel] -> IO [Pixel]
chooseInitialCenters k pixels = do
  firstCenter <- getRandomPixelInList pixels
  let initialCenters = [firstCenter]
  let remainingPixels = delete firstCenter pixels
  let distances = map (distanceBetweenPixels firstCenter) remainingPixels
  let squaredDistances = map (^ (2::Int)) distances
  chooseInitialCentersSecond k initialCenters remainingPixels squaredDistances

assignPixelsToCenters :: [Pixel] -> [Pixel] -> [(Pixel, Pixel)]
assignPixelsToCenters pixels centers = map assignPixel pixels
  where assignPixel pixel = (pixel, closest centers pixel)

algorithm :: [Pixel] -> Int -> Float -> IO ()
algorithm pixels k cLimit = do
    centers <- chooseInitialCenters k pixels
    let newCenters = assignPixelsToCenters pixels centers
    print newCenters
