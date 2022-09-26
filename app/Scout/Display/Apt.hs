module Scout.Display.Apt ( displayPackages ) where

import           Scout.Types
import           Scout.Util          (renderPackageUri, tshow)

import           Control.Lens        ((^.))
import           Control.Monad       (forM_)
import           Data.Text           (Text)
import qualified Data.Text.IO        as TIO
import           System.Console.ANSI

-- | Display a list of packages in `apt search` output style.
displayPackages :: [(Revision, PackageSearchResultInfo)] -> IO ()
displayPackages packages = forM_ packages $ \(revision, package) -> do
    TIO.putStrLn $ package ^. name . display <> "/" <> revision
    putInfos [ ("description", package^.description)
        , ("downloads", tshow $ package^.downloads)
        , ("votes", tshow $ package^.votes)
        , ("last upload", tshow $ package^.lastUpload)
        , ("uri", renderPackageUri package)
        ]
    putStrLn ""

putInfos :: [(Text, Text)] -> IO ()
putInfos = mapM_ (uncurry putInfo)

putInfo :: Text -> Text -> IO ()
putInfo key value = do
    setSGR [ SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Green ]
    TIO.putStr $ "  " <> key <> ": "
    setSGR [ Reset ]
    TIO.putStrLn value


