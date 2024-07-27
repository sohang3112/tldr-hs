module Main where

import Tldr.App (appMain)
import System.Environment (getArgs)
import System.Exit (exitWith)

-- for normal executable, pass args normally: tldr --version
-- while running with stack exec, pass args after -- : stack exec tldr -- --version
main :: IO ()
main = do
  args <- getArgs
  exitCode <- appMain args
  exitWith exitCode
