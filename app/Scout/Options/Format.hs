module Scout.Options.Format
(
      parseFormatOptions
    , DisplayFormat (..)
    , FormatOptions
    , fmtDisplayFormat
    , fmtOutputLimit
)
where

import           Control.Lens        (makeLenses)
import           Options.Applicative


data DisplayFormat = Apt | Csv deriving Show

data FormatOptions = MkFormatOptions
    { _fmtOutputLimit   :: !Int
    , _fmtDisplayFormat :: !DisplayFormat
    }
    deriving Show

makeLenses ''FormatOptions

parseOutputLimit :: Parser Int
parseOutputLimit = option auto
                    (  long "limit"
                    <> short 'l'
                    <> help "How many packages to show in the output"
                    <> showDefault
                    <> value 16
                    <> metavar "INT" )

parseDisplayFormat :: Parser DisplayFormat
parseDisplayFormat = option (eitherReader parseDisplayFormat')
                    ( long "format"
                    <> short 'f'
                    <> help "Format in which to display search results"
                    <> showDefault
                    <> value Apt
                    )
    where
        parseDisplayFormat' "apt" = Right Apt
        parseDisplayFormat' "csv" = Right Csv
        parseDisplayFormat' _     = Left "expected one of 'apt' or 'csv'"


parseFormatOptions :: Parser FormatOptions
parseFormatOptions = MkFormatOptions <$> parseOutputLimit <*> parseDisplayFormat
