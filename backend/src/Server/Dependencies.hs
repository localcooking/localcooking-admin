{-# LANGUAGE
    OverloadedStrings
  #-}

module Server.Dependencies where

import Server.Dependencies.Users.Get (getUsersServer)
import Server.Dependencies.Users.Set (setUserServer)
import LocalCooking.Types (AppM)

import Web.Routes.Nested (l_, o_, (</>))
import Web.Dependencies.Sparrow (SparrowServerT, matchGroup, match, unpackServer)
import Web.Dependencies.Sparrow.Types (Topic (..))
import Network.Wai.Trans (MiddlewareT)


dependencies :: SparrowServerT (MiddlewareT AppM) [] AppM ()
dependencies =
  matchGroup (l_ "users" </> o_) $ do
    match (l_ "get" </> o_)
      =<< unpackServer (Topic ["users","get"]) getUsersServer
    match (l_ "set" </> o_)
      =<< unpackServer (Topic ["users","set"]) setUserServer
