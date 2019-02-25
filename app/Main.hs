module Main where

import System.IO (hFlush, stdout)

import Parser
import SAT

main :: IO ()
main = do
  putStr ">> "
  hFlush stdout
  l <- getLine
  if l == "quit"
     then return ()
     else do
       print $ checkSAT $ parseFormula l
       main
