module Spec.Content.Root where

import Links (SiteLinks)
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, initLocalCookingState, LocalCookingState, LocalCookingAction, whileMountedLocalCooking, performActionLocalCooking)

import Prelude
import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import React (ReactElement, createClass, createElement) as R
import React.DOM (text) as R
import DOM.HTML.Window.Extra (WindowSize (Laptop))

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)





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
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec params@{toURI} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.localCooking.windowSize < Laptop
                      then Typography.headline
                      else Typography.display1
        , align: Typography.left
        , color: Typography.primary
        , style: createStyles {marginBottom: "1em"}
        } [R.text "Admin"]
      , divider {}
      , typography
        { variant: Typography.body1
        } [R.text "Welcome to the Local Cooking administration console - if you don't know why you're here, skedaddle!"]
      ]


root :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> R.ReactElement
root params =
  let {spec: reactSpec, dispatcher} = T.createReactSpec
        ( spec params
        ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "Spec.Content.Root"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
