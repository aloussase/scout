module Scout.Display.Csv ( displayPackages ) where

import           Scout.Types
import qualified Scout.Util    as U

import           Control.Lens  ((^.))
import           Control.Monad (forM_)
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO

-- | Display a list of packages as CSV.
displayPackages :: [(Revision, PackageSearchResultInfo)] -> IO ()
displayPackages packages = do
    TIO.putStrLn $ T.intercalate "," ["description","downloads","votes","lastUpload","uri"]
    forM_ packages $ \(_, package) -> TIO.putStrLn $ T.intercalate ","
            [ package^.description
            , U.tshow $ package^.downloads
            , U.tshow $ package^.votes
            , U.tshow $ package^.lastUpload
            , U.renderPackageUri package
            ]
