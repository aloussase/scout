module Options
(
      getOptions
    , Command (..)
    , optCommand
    , searchQuery
    , Options
) where

import           Control.Lens        (makeLenses)
import           Data.Text           (Text)
import           Options.Applicative

newtype Options = MkOptions { _optCommand :: Command }
    deriving (Show)

newtype SearchOptions = MkSearchOptions { _searchQuery :: Text }
    deriving (Show)

newtype Command = SearchCommand SearchOptions
    deriving (Show)

makeLenses ''Options
makeLenses ''SearchOptions

searchCommand :: Parser Command
searchCommand = SearchCommand . MkSearchOptions
                 <$> argument str (metavar "QUERY" <> help "Search in package descriptions")

parseCommand :: Parser Command
parseCommand =
    hsubparser
        ( command "search" ( info searchCommand (progDesc "Search packages in Hackage")  ) )

parseOptions :: Parser Options
parseOptions = MkOptions <$> parseCommand

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
        (fullDesc
        <> progDesc "Scout Hackage packages"
        <> header   "scout - CLI tool for scouting packages in Hackage")

getOptions :: IO Options
getOptions = execParser opts
