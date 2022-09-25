module Scout.Requests
(
      searchPackages
    , searchPackageInfo
    , searchPackagesWithInfo
) where

import           Scout.Options.Search (SortDirection)
import           Scout.Types

import           Control.Lens         ((&), (.~), (^.))
import           Control.Monad        (forM)
import           Data.Default.Class   (def)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Network.HTTP.Req

hackage :: Url 'Https
hackage = https "hackage.haskell.org"

packageSearchEndpoint :: Url 'Https
packageSearchEndpoint = hackage /: "packages" /: "search"

packageInfoEndpoint :: PackageName -> Url 'Https
packageInfoEndpoint package = hackage /: "package" /: package

-- | Search for information for a given package.
-- The api currently just return an object mapping revisions to "normal".
searchPackageInfo :: PackageName -> IO [Text]
searchPackageInfo package = runReq defaultHttpConfig $ do
      M.keys
    . responseBody @(JsonResponse (M.Map Text Text))
    <$> req GET (packageInfoEndpoint package) NoReqBody jsonResponse mempty

searchPackages_ ::
    SortDirection
    -> PageNumber
    -> SearchQuery
    -> IO PackageSearchResponsePayload
searchPackages_ sortDirection_ pageNumber query = runReq defaultHttpConfig $ do
    responseBody
      <$> req
            POST
            packageSearchEndpoint
            ( ReqBodyJson $ def
                            & searchQuery .~ query
                            & page .~ pageNumber
                            & sortDirection .~ (T.pack . show $ sortDirection_) )
            jsonResponse
            ( header "Content-Type" "application/json" )

-- | Search for packages matching a given query.
searchPackages :: SortDirection -> SearchQuery -> IO PackageSearchResponsePayload
searchPackages sortDirection_ = searchPackages_ sortDirection_ 0

-- | Return a list of @PackageSearchResultInfo@ and their latest revision number.
searchPackagesWithInfo :: SortDirection -> SearchQuery -> IO [(Revision, PackageSearchResultInfo)]
searchPackagesWithInfo sortDirection_ query = do
    packages <- searchPackages sortDirection_ query
    forM (packages ^. pageContents) $ \package -> do
        packageInfo <- searchPackageInfo $ package ^. name . display
        pure $ case packageInfo of
                (x:_) -> (x, package)
                []    ->  ("", package)
