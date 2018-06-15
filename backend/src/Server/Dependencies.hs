{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import LocalCooking.Function.System (SystemM)
import LocalCooking.Dependencies.Admin (adminDependencies)

import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (SparrowServerT, matchGroup, match, unpackServer)
import Web.Dependencies.Sparrow.Types (Topic (..))
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT SystemM) [] SystemM ()
dependencies =
  adminDependencies
