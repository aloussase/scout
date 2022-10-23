module Scout.Options.Format
  ( DisplayField (..)
  , DisplayFormat (..)
  , FormatOptions
  , fmtDisplayFields
  , fmtDisplayFormat
  , fmtOutputLimit
  , parseFormatOptions
  ) where

import           Control.Lens        (makeLenses)

import           Data.Default.Class  (Default (def))
import           Data.List           (foldl')
import qualified Data.Text           as T

import           Options.Applicative


data DisplayFormat = Apt | Csv deriving (Eq, Show)
data DisplayField = Description | Downloads | LastUpload | Name | Uri | Votes deriving
    ( Eq
    , Show
    )

data FormatOptions
  = MkFormatOptions
      { _fmtOutputLimit   :: !Int
      , _fmtDisplayFormat :: !DisplayFormat
      , _fmtDisplayFields :: ![DisplayField]
      }
  deriving (Eq, Show)

instance Default FormatOptions where def = MkFormatOptions 10 Apt []

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
                    <> metavar "apt, csv"
                    )
    where
        parseDisplayFormat' "apt" = Right Apt
        parseDisplayFormat' "csv" = Right Csv
        parseDisplayFormat' _     = Left "expected one of 'apt' or 'csv'"


parseDisplayFields :: Parser [DisplayField]
parseDisplayFields = option (eitherReader parseDisplayFields')
                    ( long "select"
                    <> short 's'
                    <> help "Fields from package info to output"
                    <> showDefault
                    <> value [Description,Downloads,LastUpload,Name,Uri,Votes]
                    )
    where
        parseDisplayFields' :: String -> Either String [DisplayField]
        parseDisplayFields' s = sequence (foldl' (\xs x -> f x : xs) [] (T.splitOn "," $ T.pack s))

        f "description" = Right Description
        f "downloads"   = Right Downloads
        f "lastUpload"  = Right LastUpload
        f "name"        = Right Name
        f "uri"         = Right Uri
        f "votes"       = Right Votes
        f field         = Left $ "not a valid field " <> T.unpack field

parseFormatOptions :: Parser FormatOptions
parseFormatOptions = MkFormatOptions
                <$> parseOutputLimit
                <*> parseDisplayFormat
                <*> parseDisplayFields
