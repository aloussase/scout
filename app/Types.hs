module Types where

import           Control.Lens       (makeLenses)
import           Data.Aeson
import           Data.Aeson.TH      (deriveJSON)
import           Data.Default.Class
import           Data.Text          (Text)
import           Data.Time.Clock    (UTCTime)
import           GHC.Generics


data DisplayAndUri = MkDisplayAndUri
                    { _display :: !Text
                    , _uri     :: !Text
                    }
                    deriving (Show, Generic)

makeLenses ''DisplayAndUri
deriveJSON defaultOptions{fieldLabelModifier  = drop 1} ''DisplayAndUri

-- | Payload returned from hackage.haskell.org/packages/search
data PackageSearchResultInfo = MkPackageSearchResultInfo
                             { _description :: !Text
                             , _downloads   :: !Int
                             , _lastUpload  :: !UTCTime
                             , _maintainers :: ![DisplayAndUri]
                             , _name        :: !DisplayAndUri
                             , _tags        :: ![DisplayAndUri]
                             , _votes       :: !Float
                             }
                             deriving (Show, Generic)

makeLenses ''PackageSearchResultInfo
deriveJSON defaultOptions{fieldLabelModifier  = drop 1} ''PackageSearchResultInfo

-- | A single package info entry contained in the "pageContents" key of @PackageSearchResponsePayload@.
data PackageSearchResponsePayload =
    MkPackageSearchResponsePayload
    { _numberOfResults :: !Int
    , _pageContents    :: ![PackageSearchResultInfo]
    }
    deriving (Show, Generic)

makeLenses ''PackageSearchResponsePayload
deriveJSON defaultOptions{fieldLabelModifier  = drop 1} ''PackageSearchResponsePayload

-- | Payload sent to hackage.haskell.org/packages/search.
data PackageSearchRequestPayload = MkPackageSearchRequestPayload
    { _page          :: !Int
    , _sortColumn    :: !Text
    , _sortDirection :: !Text
    , _searchQuery   :: !Text
    }

makeLenses ''PackageSearchRequestPayload
deriveJSON defaultOptions{fieldLabelModifier  = drop 1} ''PackageSearchRequestPayload

instance Default PackageSearchRequestPayload where
    def = MkPackageSearchRequestPayload
            { _page = 0
            , _sortColumn = "downloads"
            , _sortDirection = "descending"
            , _searchQuery = ""
            }
