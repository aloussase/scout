{- | Find packages in Hackage.

This package is meant to be used as an executable rather than a library, but it is modularized
in order to be able to test it.
-}
module Scout
  ( run
    -- * Option parsing
  , Options
  , defaultOptions
  , getOptions
  , toOptions
  ) where

import           Control.Lens   ((^.))

import qualified Data.Text.IO   as TIO

import           Scout.Display
import           Scout.Options
import           Scout.Requests

import           System.IO      (stderr)

runCmd :: Command -> Options -> IO ()
runCmd (SearchCommand searchOptions) opts = do
  TIO.hPutStrLn stderr $ "Searching for \"" <> (searchOptions^.searchQuery) <> "\"..."
  searchPackagesWithInfo searchOptions >>= displayPackages (opts^.formatOptions)

-- | 'run' executes the program according to the provided 'Options'.
run :: Options -> IO ()
run opts = runCmd (opts^.optCommand) opts

