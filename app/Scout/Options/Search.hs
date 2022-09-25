module Scout.Options.Search
(
      SearchOptions
    , SortDirection
    , searchQuery
    , searchOutputLimit
    , searchSortDirection
    , searchCommand
)
where

import           Control.Lens        (makeLenses)
import           Data.Text           (Text)
import           Options.Applicative

data SortDirection = Asc | Desc deriving Show

data SearchOptions = MkSearchOptions
    { _searchQuery         :: !Text
    , _searchOutputLimit   :: !Int
    , _searchSortDirection :: !SortDirection
    }
    deriving (Show)

parseOutputLimit :: Parser Int
parseOutputLimit = option auto
                    (  long "limit"
                    <> short 'l'
                    <> help "How many packages to show in the output"
                    <> showDefault
                    <> value 16
                    <> metavar "INT" )

parseSortDirection :: Parser SortDirection
parseSortDirection = option (eitherReader parseSortDirection')
                    ( long "sort"
                    <> short 's'
                    <> help "Whether to sort packages in ascending or descending order"
                    <> showDefault
                    <> value Desc
                    )
    where
        parseSortDirection' "ascending" = Right Asc
        parseSortDirection' "descending" = Right Desc
        parseSortDirection' _ = Left "expected either 'ascending' or 'descending'"

searchCommand :: Parser SearchOptions
searchCommand = MkSearchOptions
      <$> argument str (metavar "QUERY" <> help "Search in package descriptions")
      <*> parseOutputLimit
      <*> parseSortDirection

makeLenses ''SearchOptions

