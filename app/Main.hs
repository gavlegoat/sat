module Main where

import System.IO (hFlush, stdout)

import Parser
import Transformation
import DPLL

main :: IO ()
main = do
  putStr ">> "
  hFlush stdout
  l <- getLine
  if l == "quit"
     then return ()
     else do
       let f = parseFormula l
       print f
       print $ tseitin f
       print $ checkSAT f
       main
