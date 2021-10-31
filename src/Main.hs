module Main where

import System.Directory
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  currentDir <- getCurrentDirectory
  putStrLn currentDir
