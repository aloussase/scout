module Scout.Display
(
    displayPackages
)
where

import qualified Scout.Display.Apt    as Apt
import qualified Scout.Display.Csv    as Csv
import           Scout.Options.Format
import           Scout.Types

import           Control.Lens

displayPackages :: FormatOptions -> [(Revision, PackageSearchResultInfo)] -> IO ()
displayPackages opts packages =
    let format = opts^.fmtDisplayFormat
        limit = opts^.fmtOutputLimit
        packages' = take limit packages
    in case format of
        Apt -> Apt.displayPackages opts packages'
        Csv -> Csv.displayPackages opts packages'
