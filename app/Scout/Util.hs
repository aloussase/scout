module Scout.Util where

import           Scout.Types

import           Control.Lens
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Network.HTTP.Req (Scheme (..), Url, https, renderUrl, (/:))

tshow :: Show a => a -> Text
tshow = T.pack . show

hackage :: Url 'Https
hackage = https "hackage.haskell.org"

renderPackageUri :: PackageSearchResultInfo -> Text
renderPackageUri package = renderUrl $ hackage /: package ^. name . uri
