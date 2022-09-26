module Scout.Options.Search
(
      SearchOptions
    , SortDirection
    , searchQuery
    , searchSortDirection
    , parseSearchOptions
)
where

import           Control.Lens        (makeLenses)
import           Data.Text           (Text)
import           Options.Applicative

data SortDirection = Asc | Desc

instance Show SortDirection where
    show Asc  = "ascending"
    show Desc = "descending"

data SearchOptions = MkSearchOptions
    { _searchQuery         :: !Text
    , _searchSortDirection :: !SortDirection
    }
    deriving (Show)

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

parseSearchOptions :: Parser SearchOptions
parseSearchOptions = MkSearchOptions
      <$> argument str (metavar "QUERY" <> help "Search in package descriptions")
      <*> parseSortDirection

makeLenses ''SearchOptions

