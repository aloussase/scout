module Requests
(
    searchPackages
) where

import qualified Constants          as C
import           Types

import           Control.Lens       ((&), (.~))
import           Data.Default.Class (def)
import           Data.Text          (Text)
import           Network.HTTP.Req


-- | Search for packages matching a given query.
searchPackages :: Text -> IO PackageSearchResponsePayload
searchPackages query = runReq defaultHttpConfig $ do
    responseBody
      <$> req
            POST
            C.packageSearchEndpoint
            (ReqBodyJson $ def & searchQuery .~ query)
            jsonResponse
            (header "Content-Type" "application/json")
