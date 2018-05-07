module Spec.Content where

import Spec.Content.Root (root)
import Spec.Content.Users (users)
import Spec.Content.UserDetails (userDetails)
import Client.Dependencies.Users (UsersSparrowClientQueues)
import Links (SiteLinks (..))
import LocalCooking.Window (WindowSize)
import LocalCooking.Common.AccessToken.Auth (AuthToken)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.Maybe (Maybe)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Partial.Unsafe (unsafePartial)



type State =
  { page :: SiteLinks
  , windowSize :: WindowSize
  }

initialState :: {initSiteLinks :: SiteLinks, initWindowSize :: WindowSize} -> State
initialState {initSiteLinks,initWindowSize} =
  { page: initSiteLinks
  , windowSize: initWindowSize
  }

data Action
  = ChangedCurrentPage SiteLinks
  | ChangedWindowSize WindowSize


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , console   :: CONSOLE
  , scrypt    :: SCRYPT
  | eff)


spec :: forall eff
      . { windowSizeSignal  :: IxSignal (Effects eff) WindowSize
        , currentPageSignal :: IxSignal (Effects eff) SiteLinks
        , authTokenSignal   :: IxSignal (Effects eff) (Maybe AuthToken)
        , siteLinks         :: SiteLinks -> Eff (Effects eff) Unit
        , toURI             :: Location -> URI
        , usersQueues       :: UsersSparrowClientQueues (Effects eff)
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  { windowSizeSignal
  , currentPageSignal
  , authTokenSignal
  , siteLinks
  , toURI
  , usersQueues
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage p ->
        void $ T.cotransform _ { page = p }
      ChangedWindowSize w ->
        void $ T.cotransform _ { windowSize = w }

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ case state.page of
          RootLink ->
            root
              { windowSizeSignal
              , toURI
              }
          UsersLink ->
            users
              { usersQueues
              , authTokenSignal
              }
          _ -> R.text ""
      ]



content :: forall eff
         . { currentPageSignal :: IxSignal (Effects eff) SiteLinks
           , windowSizeSignal  :: IxSignal (Effects eff) WindowSize
           , authTokenSignal   :: IxSignal (Effects eff) (Maybe AuthToken)
           , siteLinks         :: SiteLinks -> Eff (Effects eff) Unit
           , toURI             :: Location -> URI
           , usersQueues       :: UsersSparrowClientQueues (Effects eff)
           } -> R.ReactElement
content
  { currentPageSignal
  , windowSizeSignal
  , authTokenSignal
  , siteLinks
  , toURI
  , usersQueues
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initWindowSize: unsafePerformEff $ IxSignal.get windowSizeSignal
        }
      {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { windowSizeSignal
            , currentPageSignal
            , authTokenSignal
            , siteLinks
            , toURI
            , usersQueues
            }
          )
          (initialState init)
      reactSpec' = Signal.whileMountedIxUUID
                     currentPageSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
                 $ Signal.whileMountedIxUUID
                     windowSizeSignal
                     (\this x -> unsafeCoerceEff $ dispatcher this (ChangedWindowSize x))
                   reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
