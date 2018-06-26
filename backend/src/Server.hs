{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , ScopedTypeVariables
  , QuasiQuotes
  , DataKinds
  #-}

module Server where

import Server.HTTP (httpServer)
import Server.Assets (favicons, frontend, frontendMin)
import Links (SiteLinks)

import LocalCooking.Server (LocalCookingArgs (..))
import LocalCooking.Colors (LocalCookingColors (..))
import LocalCooking.Dependencies.Admin (adminDependencies)
import LocalCooking.Dependencies.Tag (tagDependencies)

import Text.Lucius (Color (..))


server :: LocalCookingArgs SiteLinks sec -- []
server = LocalCookingArgs
  { localCookingArgsFrontend = frontend
  , localCookingArgsFrontendMin = frontendMin
  , localCookingArgsFavicons = favicons
  , localCookingArgsHTTP = httpServer
  , localCookingArgsDeps = do
      adminDependencies
      tagDependencies
  , localCookingArgsColors = LocalCookingColors
    { localCookingColorsMain = Color 41 67 78
    , localCookingColorsActive = Color 84 110 122
    , localCookingColorsHover = Color 129 156 169
    }
  }
