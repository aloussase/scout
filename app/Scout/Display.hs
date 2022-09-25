module Scout.Display
(
    displayPackages
)
where

import           Scout.Types

import           Control.Lens        ((^.))
import           Control.Monad       (forM_)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           System.Console.ANSI

putInfos :: [(Text, Text)] -> IO ()
putInfos = mapM_ (uncurry putInfo)

putInfo :: Text -> Text -> IO ()
putInfo key value = do
    setSGR [ SetConsoleIntensity BoldIntensity , SetColor Foreground Vivid Green ]
    TIO.putStr $ "  " <> key <> ": "
    setSGR [ Reset ]
    TIO.putStrLn value

displayPackages_ :: [(Revision, PackageSearchResultInfo)] -> IO ()
displayPackages_ packages = forM_ packages $ \(revision, package) -> do
    TIO.putStrLn $ package ^. name . display <> "/" <> revision
    putInfos [ ("description", package^.description)
             , ("downloads", tshow $ package^.downloads)
             , ("votes", tshow $ package^.votes)
             , ("last upload", tshow $ package^.lastUpload)
             ]
    putStrLn ""

displayPackages :: Int -> [(Revision, PackageSearchResultInfo)] -> IO ()
displayPackages limit = displayPackages_ . take limit

tshow :: (Show a) => a -> Text
tshow = T.pack . show
