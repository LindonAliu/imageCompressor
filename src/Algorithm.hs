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
            , distanceSqrBetweenPixels
            , distanceBetweenPixels
            , randomInt
            )
import Output (printImage)
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
      let newCenters = newCenter:centers
      let remaining = delete newCenter pixels
      let newDistances = map (distanceSqrBetweenPixels newCenter) remaining
      chooseInitialCentersSecond k newCenters remaining newDistances

chooseInitialCenters :: Int -> [Pixel] -> IO [Pixel]
chooseInitialCenters k pixels = do
  firstCenter <- getRandomPixelInList pixels
  let initialCenters = [firstCenter]
  let remainingPixels = delete firstCenter pixels
  let distances = map (distanceSqrBetweenPixels firstCenter) remainingPixels
  chooseInitialCentersSecond k initialCenters remainingPixels distances

assignPixelsToCenters :: [Pixel] -> [Pixel] -> [(Pixel, [Pixel])]
assignPixelsToCenters pixels centers = map assignCenter centers
  where assignCenter c = (c, filter (\px -> closest centers px == c) pixels)

getNewCenters :: [(Pixel, [Pixel])] -> [Pixel]
getNewCenters assignations = map getNewCenter assignations
  where getNewCenter (center, pixels) = center { color = averageColor pixels }

highestDistance :: [Pixel] -> [Pixel] -> Float
highestDistance [] _ = 0
highestDistance _ [] = 0
highestDistance (x:xs) (y:ys)
  = max (distanceBetweenPixels x y) (highestDistance xs ys)

averageColor :: [Pixel] -> Color
averageColor pixels | size == 0 = (0, 0, 0)
                    | otherwise = (averageR, averageG, averageB)
  where size = length pixels
        averageR = div (sum (map (\px -> getRGB (color px) 0) pixels)) size
        averageG = div (sum (map (\px -> getRGB (color px) 1) pixels)) size
        averageB = div (sum (map (\px -> getRGB (color px) 2) pixels)) size

kmeansAlgorithm :: [Pixel] -> [Pixel] -> Int -> Float -> [(Pixel, [Pixel])]
kmeansAlgorithm pixels centers k cLimit
  | dist > cLimit = kmeansAlgorithm pixels newCenters k cLimit
  | otherwise = assignations
  where assignations = assignPixelsToCenters pixels centers
        newCenters = getNewCenters assignations
        dist = highestDistance centers newCenters

algorithm :: [Pixel] -> Int -> Float -> IO ()
algorithm pixels k cLimit = do
    centers <- chooseInitialCenters k pixels
    printImage (kmeansAlgorithm pixels centers k cLimit)
