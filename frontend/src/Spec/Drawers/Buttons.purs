module Spec.Drawers.Buttons where

import Links (SiteLinks (UsersLink))
import User (UserDetails)
import LocalCooking.Thermite.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)

import Prelude
import Data.UUID (GENUUID)
import Data.Lens (Lens', lens)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React (ReactElement, createClass, createElement) as R

import MaterialUI.Divider (divider)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.Icons.People (peopleIcon)



type State =
  { localCooking :: LocalCookingState SiteLinks UserDetails
  }

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState localCooking =
  { localCooking
  }

data Action
  = LocalCookingAction (LocalCookingAction SiteLinks UserDetails)
  | Clicked SiteLinks

type Effects eff =
  ( ref :: REF
  , exception :: EXCEPTION
  , uuid :: GENUUID
  | eff)


getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens (_.localCooking) (_ { localCooking = _ })


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> R.ReactElement
     -> T.Spec (Effects eff) State Unit Action
spec params@{siteLinks} prefix = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      LocalCookingAction a -> performActionLocalCooking getLCState a props state
      Clicked x -> liftEff (siteLinks x)

    -- FIXME generate href links via params.toURI
    render :: T.Render State Unit Action
    render dispatch props state children =
      [ prefix
      , divider {}
      , listItem
        { button: true
        , onClick: mkEffFn1 \_ -> unsafeCoerceEff $ siteLinks UsersLink
        }
        [ listItemIcon {} peopleIcon
        , listItemText
          { primary: "Users"
          }
        ]
      ]


drawersButtons :: forall eff
                . LocalCookingParams SiteLinks UserDetails (Effects eff)
               -> R.ReactElement
               -> R.ReactElement
drawersButtons params prefix =
  let {spec:reactSpec,dispatcher} =
        T.createReactSpec
          ( spec params prefix
          ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
        whileMountedLocalCooking
          params
          "Spec.Content"
          LocalCookingAction
          (\this -> unsafeCoerceEff <<< dispatcher this)
          reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
