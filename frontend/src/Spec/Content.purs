module Spec.Content where

import Spec.Content.Root (root)
import Spec.Content.Users (users)
import Spec.Dialogs.User (userDialog)
import Links (SiteLinks (..))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Dependencies.Admin (GetUsersSparrowClientQueues, SetUserSparrowClientQueues)

import Prelude

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R

import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)

import Crypto.Scrypt (SCRYPT)

import Queue.Types (writeOnly)
import Queue.One as One
import Queue.One.Aff as OneIO
import DOM (DOM)



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)


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
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> { getUsersQueues :: GetUsersSparrowClientQueues (Effects eff)
        , setUserQueues  :: SetUserSparrowClientQueues (Effects eff)
        , env            :: Env
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  params@{windowSizeSignal,currentPageSignal,authTokenSignal,siteLinks,toURI}
  { getUsersQueues
  , setUserQueues
  , env
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children = case state.localCooking.currentPage of
      RootLink ->
        [ root
          params
        ]
      UsersLink ->
        [ users
          { getUsersQueues
          , setUserQueues
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
           , setUserQueues  :: SetUserSparrowClientQueues (Effects eff)
           , env            :: Env
           } -> R.ReactElement
content
  params
  { getUsersQueues
  , setUserQueues
  , env
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            params
            { getUsersQueues
            , setUserQueues
            , env
            }
          )
          (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
