module Scout.Display
(
    displayPackages
)
where

import qualified Scout.Display.Apt    as Apt
import qualified Scout.Display.Csv    as Csv
import           Scout.Options.Format (DisplayFormat (..))
import           Scout.Types

displayPackages :: DisplayFormat -> Int -> [(Revision, PackageSearchResultInfo)] -> IO ()
displayPackages Apt limit = Apt.displayPackages . take limit
displayPackages Csv limit = Csv.displayPackages . take limit
