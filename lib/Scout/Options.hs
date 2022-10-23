module Scout.Options
  ( Command (..)
  , Options
  , defaultOptions
  , fmtDisplayFormat
  , fmtOutputLimit
  , formatOptions
  , getOptions
  , optCommand
  , searchQuery
  , searchSortDirection
  , toOptions
  ) where

import           Control.Lens         (makeLenses)

import           Data.Default.Class

import           Options.Applicative

import           Scout.Options.Format
import           Scout.Options.Search

import           System.Environment   (getArgs)

-- | The program options.
data Options
  = MkOptions
      { _optCommand    :: !Command
      , _formatOptions :: !FormatOptions
      }
  deriving (Eq, Show)

instance Default Options where def = MkOptions (SearchCommand def) def

-- | 'defaultOptions' are the default options.
defaultOptions :: Options
defaultOptions = def

newtype Command
  = SearchCommand SearchOptions
  deriving (Eq, Show)

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

-- | 'toOptions' parses the provided list of command line arguments into an 'Options'.
toOptions :: [String] -> Either String Options
toOptions args =
    case execParserPure defaultPrefs opts args of
        Success o       -> Right o
        Failure failure -> Left $ show failure
        _               -> error "unexpected result"

-- | 'getOptions' parses the list of command line arguments into an 'Options'.
getOptions :: IO (Either String Options)
getOptions = toOptions <$> getArgs
