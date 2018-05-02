{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import LocalCooking.Types (AppM)

import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (SparrowServerT, matchGroup, match, unpackServer)
import Web.Dependencies.Sparrow.Types (Topic (..))
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
dependencies =
  pure ()
