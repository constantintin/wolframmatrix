{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( makeWolfram
    , makeWolframQuadratic
    ) where

import Text.Parsec
import Data.List.Extra

ifMaybe predicate value =
  if predicate then
    Just value
  else
    Nothing

sqrtMaybe :: Int -> Maybe Int
sqrtMaybe i =
  let square :: Float = sqrt $ fromIntegral i
      roundSquare = round square
  in  ifMaybe (fromIntegral roundSquare == square) roundSquare

inputParser = do c <- entries `sepBy1` (char ' ')
                 return c
  where entries = many $ noneOf " "

wolfram :: Int -> [String] -> String
wolfram n = concatB "" . map (concatB "") . chunksOf n
  where
    concatB (x:xs) [] = "{" ++ xs ++ "}"
    concatB str (x:xs) = concatB (str ++ "," ++ x) xs

matrixShow :: Int -> [String] -> String
matrixShow n matrix = (concat "\n" $ map (concat " " . map (\(offset, x) -> addSpace x offset) . zip offsets) chunks)
  where
    addSpace s n = (take (n - (length s)) $ repeat ' ') ++ s
    chunks = chunksOf n matrix
    transpose = map (\i -> map (!! i) chunks) [0..n-1]
    offsets = map (length . maximumBy (\a b -> compare (length a) (length b))) transpose
    concat s (x:xs) = x ++ s ++ (concat s xs)
    concat s [] = ""

makeWolframQuadratic :: String -> IO ()
makeWolframQuadratic input =
  let handle entries =
        case sqrtMaybe (length entries) of
          Just n -> printMatrices n entries
          Nothing -> putStrLn "Not a quadratic matrix"
  in  case parse inputParser "" input of
        Right s -> handle s
        Left  e -> print e

makeWolfram:: Int -> String -> IO ()
makeWolfram n input =
  case parse inputParser "" input of
    Right s -> printMatrices n s
    Left  e -> print e

printMatrices :: Int -> [String] -> IO ()
printMatrices n entries =
  do putStrLn $ wolfram n entries
     putStr $ "\n" ++ matrixShow n entries
