{-
-- EPITECH PROJECT, 2023
-- Algorithm
-- File description:
-- k-means++ algorithm for image compression
-}

module Algorithm
    ( parseFile
    , Pixel (..)
    , algorithm
    ) where

import Data.Maybe
import Data.List (delete)
import System.Random (getStdRandom, randomR)

import System.IO.Unsafe (unsafePerformIO)
import Math (distanceBetweenPixels
            , randomInt
            , Pixel (..)
            , Color
            , closest
            )

instance Eq Pixel where
  (Pixel pos1 color1) == (Pixel pos2 color2) = pos1 == pos2 && color1 == color2

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

sampleFromDistribution :: [Float] -> Int
sampleFromDistribution distribution = loop 0 (head distribution) (tail distribution)
  where
    loop index _ [] = index
    loop index acc (d:ds)
      | acc > r = index
      | otherwise = loop (index + 1) (acc + d) ds
      where r = unsafePerformIO (getStdRandom (randomR (0, 1)))

chooseInitialCentersSecond :: Int -> [Pixel] -> [Pixel] -> [Float] -> IO [Pixel]
chooseInitialCentersSecond k centers pixels squaredDistances
  | length centers == k = return centers
  | otherwise = do
      let distribution = map (\x -> x / sum squaredDistances) squaredDistances
      let newCenterIndex = sampleFromDistribution distribution
      let newCenter = pixels !! newCenterIndex
      let newCenters = centers ++ [newCenter]
      let remainingPixels = delete newCenter pixels
      let newDistances = map (distanceBetweenPixels newCenter) remainingPixels
      let newSquaredDistances = map (^2) newDistances
      chooseInitialCentersSecond k newCenters remainingPixels newSquaredDistances

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
