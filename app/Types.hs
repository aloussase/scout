module Types where

import           Control.Lens    (makeLenses)
import           Data.Aeson
import           Data.Aeson.TH   (deriveJSON)
import           Data.Text       (Text)
import           Data.Time.Clock (UTCTime)
import           GHC.Generics


data DisplayAndUri = MkDisplayAndUri
                    { _display :: !Text
                    , _uri     :: !Text
                    }
                    deriving (Show, Generic)


makeLenses ''DisplayAndUri
deriveJSON defaultOptions{fieldLabelModifier  = drop 1} ''DisplayAndUri

data PackageSearchPayload = MkPackageSearchPayload
                            { _description :: !Text
                            , _downloads   :: !Int
                            , _lastUpload  :: !UTCTime
                            , _maintainers :: ![DisplayAndUri]
                            , _name        :: !DisplayAndUri
                            , _tags        :: !DisplayAndUri
                            , _votes       :: !Int
                            }
                            deriving (Show, Generic)

makeLenses ''PackageSearchPayload
deriveJSON defaultOptions{fieldLabelModifier  = drop 1} ''PackageSearchPayload
