module Scout.Options
(
      getOptions
    , Command (..)
    , optCommand
    , searchQuery
    , searchOutputLimit
    , searchSortDirection
    , Options
) where

import           Scout.Options.Search

import           Control.Lens         (makeLenses)
import           Options.Applicative

newtype Options = MkOptions
    { _optCommand :: Command
    }
    deriving (Show)

newtype Command = SearchCommand SearchOptions
    deriving (Show)

makeLenses ''Options

parseCommand :: Parser Command
parseCommand =
    hsubparser
        ( command "search" ( info (SearchCommand <$> searchCommand) (progDesc "Search packages in Hackage")  ) )

parseOptions :: Parser Options
parseOptions = MkOptions <$> parseCommand

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
        (fullDesc
        <> progDesc "Scout Hackage packages"
        <> header   "scout - CLI tool for scouting packages in Hackage")

getOptions :: IO Options
getOptions = execParser opts
