module Main where

import           Scout.Display
import           Scout.Options
import           Scout.Requests

import           Control.Lens   ((^.))

run :: Options -> IO ()
run opts = case opts^.optCommand of
            SearchCommand searchOptions -> do
                packages <- searchPackagesWithInfo
                                (searchOptions^.searchSortDirection)
                                (searchOptions^.searchQuery)
                displayPackages (searchOptions^.searchOutputLimit) packages

main :: IO ()
main = getOptions >>= run
