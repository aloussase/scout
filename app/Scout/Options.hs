module Scout.Options
(
      getOptions
    , Command (..)
    , fmtDisplayFormat
    , fmtOutputLimit
    , formatOptions
    , optCommand
    , searchQuery
    , searchSortDirection
    , Options
) where

import           Scout.Options.Format
import           Scout.Options.Search

import           Control.Lens         (makeLenses)
import           Options.Applicative

data Options = MkOptions
    { _optCommand    :: !Command
    , _formatOptions :: !FormatOptions
    }
    deriving (Show)

newtype Command = SearchCommand SearchOptions
    deriving (Show)

makeLenses ''Options

parseCommand :: Parser Command
parseCommand =
    hsubparser
        ( command "search" ( info (SearchCommand <$> parseSearchOptions) (progDesc "Search packages in Hackage")  ) )

parseOptions :: Parser Options
parseOptions = MkOptions <$> parseCommand <*> parseFormatOptions

opts :: ParserInfo Options
opts = info (parseOptions <**> helper)
        (fullDesc
        <> progDesc "Scout Hackage packages"
        <> header   "scout - CLI tool for scouting packages in Hackage")

getOptions :: IO Options
getOptions = execParser opts
