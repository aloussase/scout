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
                 <$> argument str (metavar "QUERY" <> help "The name of a package to look for")

parseCommand :: Parser Command
parseCommand =
    hsubparser
        ( command "search" ( info searchCommand (progDesc "Search packages in hackage")  ) )

parseOptions :: Parser Options
parseOptions = MkOptions <$> parseCommand

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
        (fullDesc
        <> progDesc "Scout Hackage packages"
        <> header   "scout - CLI tool for scouting packages in Hackage")

getOptions :: IO Options
getOptions = execParser opts
