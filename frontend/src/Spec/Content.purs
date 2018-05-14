module Spec.Content where

import Spec.Content.Root (root)
import Spec.Content.Users (users)
import Spec.Content.UserDetails (userDetails)
import Spec.Dialogs.User (userDialog)
import Client.Dependencies.Users.Get (GetUsersSparrowClientQueues)
import Links (SiteLinks (..))
import LocalCooking.Window (WindowSize)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)
import LocalCooking.Spec.Snackbar (SnackbarMessage (..))
import User (UserDetails)
import Types.Env (env)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.Location (Location)
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)

import Crypto.Scrypt (SCRYPT)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Queue.Types (WRITE, writeOnly)
import Queue.One as One
import Queue.One.Aff as OneIO
import Partial.Unsafe (unsafePartial)
import DOM (DOM)



type State = LocalCookingState SiteLinks UserDetails

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState = id

type Action = LocalCookingAction SiteLinks UserDetails


type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  , console   :: CONSOLE
  , scrypt    :: SCRYPT
  , dom       :: DOM
  , timer     :: TIMER
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens id (\_ x -> x)

getLCAction :: Prism' Action (LocalCookingAction SiteLinks UserDetails)
getLCAction = prism' id Just



spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { getUsersQueues :: GetUsersSparrowClientQueues (Effects eff)
        , env            :: Env
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  params@{windowSizeSignal,currentPageSignal,authTokenSignal,siteLinks,toURI}
  { getUsersQueues
  , env
  } = T.simpleSpec (performAction <> performActionLocalCooking getLCState getLCAction) render
  where
    performAction action props state = pure unit

    render :: T.Render State Unit Action
    render dispatch props state children = case state.currentPage of
      RootLink ->
        [ root
          { windowSizeSignal
          , toURI
          }
        ]
      UsersLink ->
        [ users
          { getUsersQueues
          , userDialogQueue
          , userCloseQueue
          , authTokenSignal
          }
        , userDialog
          params
          { userDialogQueue
          , userCloseQueue
          , env
          }
        ]
      _ -> [R.text ""]

    userDialogQueue = unsafePerformEff OneIO.newIOQueues
    userCloseQueue = unsafePerformEff $ writeOnly <$> One.newQueue



content :: forall eff
         . LocalCookingParams SiteLinks UserDetails (Effects eff)
        -> { getUsersQueues :: GetUsersSparrowClientQueues (Effects eff)
           , env            :: Env
           } -> R.ReactElement
content
  params
  { getUsersQueues
  , env
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { getUsersQueues
            , env
            }
          )
          (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            getLCAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
