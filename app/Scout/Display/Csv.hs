module Scout.Display.Csv ( displayPackages ) where

import           Scout.Options.Format (FormatOptions, fmtDisplayFields)
import           Scout.Types
import qualified Scout.Util           as U

import           Control.Lens         ((^.))
import           Control.Monad        (forM_)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

-- | Display a list of packages as CSV.
displayPackages :: FormatOptions -> [(Revision, PackageSearchResultInfo)] -> IO ()
displayPackages opts packages =
    let texts = map (flip U.projectFields (opts^.fmtDisplayFields) . snd) packages
     in forM_ texts (TIO.putStrLn . T.intercalate ",")
