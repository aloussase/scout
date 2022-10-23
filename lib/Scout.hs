module Scout
(
      run
    -- Option parsing
    , getOptions
    , toOptions
    , defaultOptions
    -- Lenses
    , searchQuery
    , searchSortDirection
    , fmtDisplayFormat
    , fmtOutputLimit
    , formatOptions
    , optCommand
) where

import           Scout.Display
import           Scout.Options
import           Scout.Requests

import           Control.Lens   ((^.))
import qualified Data.Text.IO   as TIO
import           System.IO      (stderr)

runCmd :: Command -> Options -> IO ()
runCmd (SearchCommand searchOptions) opts = do
  TIO.hPutStrLn stderr $ "Searching for \"" <> (searchOptions^.searchQuery) <> "\"..."
  searchPackagesWithInfo searchOptions >>= displayPackages (opts^.formatOptions)

run :: Options -> IO ()
run opts = runCmd (opts^.optCommand) opts

