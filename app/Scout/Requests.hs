module Scout.Requests
(
      searchPackages
    , searchPackageInfo
    , searchPackagesWithInfo
) where

import qualified Scout.Options.Search     as S
import           Scout.Types
import           Scout.Util               (hackage)

import           Control.Concurrent.Async (forConcurrently)
import           Control.Lens             ((&), (.~), (^.))
import           Data.Default.Class       (def)
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Network.HTTP.Req

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
    S.SortDirection
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
searchPackages :: S.SearchOptions -> IO PackageSearchResponsePayload
searchPackages opts =
    let sortDirection_ = opts ^. S.searchSortDirection
        query = opts ^. S.searchQuery
     in searchPackages_ sortDirection_ 0 query

-- | Return a list of @PackageSearchResultInfo@ and their latest revision number.
searchPackagesWithInfo :: S.SearchOptions -> IO [(Revision, PackageSearchResultInfo)]
searchPackagesWithInfo opts = do
    packages <- searchPackages opts
    forConcurrently (packages ^. pageContents) $ \package -> do
        packageInfo <- searchPackageInfo $ package ^. name . display
        pure $ case packageInfo of
                (x:_) -> (x, package)
                []    ->  ("", package)
