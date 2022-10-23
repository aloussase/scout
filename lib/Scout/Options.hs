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
    , toOptions
    , Options
    , defaultOptions
) where

import           Scout.Options.Format
import           Scout.Options.Search

import           Control.Lens         (makeLenses)
import           Data.Default.Class
import           Options.Applicative
import           System.Environment   (getArgs)

data Options = MkOptions
    { _optCommand    :: !Command
    , _formatOptions :: !FormatOptions
    }
    deriving (Show, Eq)

instance Default Options where def = MkOptions (SearchCommand def) def

defaultOptions :: Options
defaultOptions = def

newtype Command = SearchCommand SearchOptions
    deriving (Show, Eq)

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

toOptions :: [String] -> Either String Options
toOptions args =
    case execParserPure defaultPrefs opts args of
        Success o       -> Right o
        Failure failure -> Left $ show failure
        _               -> error "unexpected result"

getOptions :: IO (Either String Options)
getOptions = toOptions <$> getArgs
