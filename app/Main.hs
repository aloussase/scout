module Main where

import           Scout.Display
import           Scout.Options
import           Scout.Requests

import           Control.Lens   ((^.))
import qualified Data.Text.IO   as TIO

run :: Options -> IO ()
run opts = case opts^.optCommand of
            SearchCommand searchOptions -> do
                TIO.putStrLn $ "Searching for \"" <> (searchOptions^.searchQuery) <> "\"..."
                packages <- searchPackagesWithInfo
                                (searchOptions^.searchSortDirection)
                                (searchOptions^.searchQuery)
                displayPackages (searchOptions^.searchOutputLimit) packages

main :: IO ()
main = getOptions >>= run
