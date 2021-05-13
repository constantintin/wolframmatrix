module Main where

import Lib

import System.Environment

main :: IO ()
main = getArgs >>= processArgs
  where processArgs [input, n] = makeWolfram (read n) input
        processArgs [input] = makeWolframQuadratic input
        processArgs [] = putStrLn $ "Missing arguments"
