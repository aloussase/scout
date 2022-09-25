module Display where

import           Types

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

displayPackages :: [PackageSearchResultInfo] -> IO ()
displayPackages packages = forM_ packages $ \package -> do
    TIO.putStrLn $ package ^. name . display
    putInfos [ ("description", package^.description)
             , ("downloads", tshow $ package^.downloads)
             , ("votes", tshow $ package^.votes)
             ]
    putStrLn ""

tshow :: (Show a) => a -> Text
tshow = T.pack . show
