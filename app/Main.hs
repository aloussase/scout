module Main where

import           Options

main :: IO ()
main = do
    options <- getOptions
    print options
