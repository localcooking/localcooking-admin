module Spec.Content.UserDetails where

import Links (SiteLinks (UserDetailsLink), UserDetailsLinks (..))
import User (UserDetails)
import Spec.Content.UserDetails.General (general)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, performActionLocalCooking, whileMountedLocalCooking, initLocalCookingState)

import Prelude

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Drawer (drawer)
import MaterialUI.Drawer as Drawer
import MaterialUI.Divider (divider)
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)

import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Exception (EXCEPTION)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal
import Partial.Unsafe (unsafePartial)


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
  ( ref :: REF
  , uuid :: GENUUID
  , exception :: EXCEPTION
  | eff)


getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec params = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ case state.localCooking.currentPage of -- TODO pack currentPageSignal listener to this level, so
                            -- side buttons aren't redrawn
          UserDetailsLink mUserDetails -> case mUserDetails of
            Nothing -> general
            Just UserDetailsGeneralLink -> general
            _ -> R.text ""
          _ -> R.text ""
      ]

userDetails :: forall eff
             . LocalCookingParams SiteLinks UserDetails (Effects eff)
            -> R.ReactElement
userDetails params =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec params
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "Spec.Content.UserDetails"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
        reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
