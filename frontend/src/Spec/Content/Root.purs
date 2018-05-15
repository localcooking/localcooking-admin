module Spec.Content.Root where

import LocalCooking.Window (WindowSize (Laptop))
import Links (AboutPageLinks (..), SiteLinks)
import User (UserDetails)
import LocalCooking.Links.Class (toLocation)
import LocalCooking.Types.Params (LocalCookingParams, initLocalCookingState, LocalCookingState, LocalCookingAction, whileMountedLocalCooking, performActionLocalCooking)

import Prelude
import Data.UUID (GENUUID)
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location)
import Data.Lens (lens)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Signal.WhileMounted as Signal

import MaterialUI.Types (createStyles)
import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)
import MaterialUI.Grid (grid)
import MaterialUI.Grid as Grid
import MaterialUI.List (list)
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.Icons.Search (searchIcon)
import MaterialUI.Icons.PictureInPicture (pictureInPictureIcon)
import MaterialUI.Icons.ShoppingCart (shoppingCartIcon)
import MaterialUI.Icons.Timelapse (timelapseIcon)
import MaterialUI.Icons.LocalShipping (localShippingIcon)
import MaterialUI.Icons.RestaurantMenu (restaurantMenuIcon)

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type State = LocalCookingState SiteLinks UserDetails

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState = id

type Action = LocalCookingAction SiteLinks UserDetails

type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  | eff)


spec :: forall eff
      . { toURI :: Location -> URI
        }
     -> T.Spec eff State Unit Action
spec {toURI} = T.simpleSpec performAction render
  where
    performAction = performActionLocalCooking (lens id (\_ x -> x))

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: if state.windowSize < Laptop then Typography.headline else Typography.display1
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
        ( spec {toURI: params.toURI}
        ) (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "Spec.Content.Root"
            id
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
