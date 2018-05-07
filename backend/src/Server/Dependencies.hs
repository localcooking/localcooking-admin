{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import Server.Dependencies.Users (usersServer)
import LocalCooking.Types (AppM)

import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (SparrowServerT, matchGroup, match, unpackServer)
import Web.Dependencies.Sparrow.Types (Topic (..))
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) AppM ()
dependencies =
  match (l_ "users" </> o_)
    =<< unpackServer (Topic ["users"]) usersServer

