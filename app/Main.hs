module Main where

import Tldr.App (appMain)
import System.Environment (getArgs)
import System.Exit (exitWith)

main :: IO ()
main = do
  args <- getArgs
  exitCode <- appMain args
  exitWith exitCode
