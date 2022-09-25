module Main where

import           Display
import           Options
import           Requests
import           Types        (pageContents)

import           Control.Lens (view, (^.))

run :: Options -> IO ()
run opts = case opts^.optCommand of
            SearchCommand searchOptions ->
                searchPackages (searchOptions^.searchQuery) >>= displayPackages . view pageContents

main :: IO ()
main = getOptions >>= run
