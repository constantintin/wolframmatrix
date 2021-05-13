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
wolfram n =  entriesToWolfram . chunksOf n

entriesToWolfram :: [[String]] -> String
entriesToWolfram = concatB "" . map (concatB "")
  where
    concatB (x:xs) [] = "{" ++ xs ++ "}"
    concatB str (x:xs) = concatB (str ++ "," ++ x) xs

makeWolframQuadratic :: String -> IO ()
makeWolframQuadratic input =
  let handle entries =
        case sqrtMaybe (length entries) of
          Just n -> putStrLn $ wolfram n entries
          Nothing -> putStrLn "Not a quadratic matrix"
  in  case parse inputParser "" input of
        Right s -> handle s
        Left  e -> print e

makeWolfram:: Int -> String -> IO ()
makeWolfram n input =
  case parse inputParser "" input of
    Right s -> putStrLn $ wolfram n s
    Left  e -> print e
