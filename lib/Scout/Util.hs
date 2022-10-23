module Scout.Util where

import           Scout.Options.Format
import           Scout.Types

import           Control.Lens
import           Data.List            (foldl')
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Req     (Scheme (..), Url, https, renderUrl, (/:))

-- | Like show but return 'Text' instead of 'String'.
tshow :: Show a => a -> Text
tshow = T.pack . show

-- | The hackage base url.
hackage :: Url 'Https
hackage = https "hackage.haskell.org"

renderPackageUri :: PackageSearchResultInfo -> Text
renderPackageUri package = renderUrl $ hackage /: T.tail (package ^. name . uri)

-- | 'projectFields' takes a list of 'DisplayField' and returns a list with the
-- corresponding textual information.
projectFields :: PackageSearchResultInfo -> [DisplayField] -> [Text]
projectFields package = foldl' f []
    where
        f :: [Text] -> DisplayField -> [Text]
        f xs Description = package ^. description : xs
        f xs Downloads   = tshow (package ^. downloads) : xs
        f xs LastUpload  = tshow (package ^. lastUpload) : xs
        f xs Name        = package ^. name . display : xs
        f xs Uri         = renderPackageUri package : xs
        f xs Votes       = tshow (package ^. votes) : xs
