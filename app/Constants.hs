{-# LANGUAGE DataKinds #-}
module Constants where

import           Network.HTTP.Req (Scheme (..), Url, https, (/:))

hackage :: Url 'Https
hackage = https "hackage.haskell.org"

packageSearchEndpoint :: Url 'Https
packageSearchEndpoint = hackage /: "packages" /: "search"
