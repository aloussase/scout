module Main where

import           Display
import           Options
import           Requests

import           Control.Lens ((^.))

run :: Options -> IO ()
run opts = case opts^.optCommand of
            SearchCommand searchOptions -> do
                searchPackagesWithInfo (searchOptions^.searchQuery) >>= displayPackages

main :: IO ()
main = getOptions >>= run
