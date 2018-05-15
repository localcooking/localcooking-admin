module Spec.Topbar.Buttons where

import Links (SiteLinks (UsersLink))
import LocalCooking.Links.Class (toLocation)
import LocalCooking.Common.User.Role (UserRole (Admin))
import LocalCooking.Types.Params (LocalCookingParams, LocalCookingState, LocalCookingAction, initLocalCookingState, performActionLocalCooking, whileMountedLocalCooking)
import User (UserDetails (..))

import Prelude
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Array as Array
import Data.Maybe (Maybe (..))
import Data.Lens (Lens', Prism', lens, prism')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafePerformEff, unsafeCoerceEff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Exception (EXCEPTION)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.SVG as RS
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import React.Signal.WhileMounted as Signal

import MaterialUI.Button (button)
import MaterialUI.Button as Button

import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type Effects eff =
  ( ref       :: REF
  , exception :: EXCEPTION
  , uuid      :: GENUUID
  | eff)


type State = LocalCookingState SiteLinks UserDetails

initialState :: LocalCookingState SiteLinks UserDetails -> State
initialState = id

data Action
  = Clicked SiteLinks
  | LocalCookingAction (LocalCookingAction SiteLinks UserDetails)

getLCState :: Lens' State (LocalCookingState SiteLinks UserDetails)
getLCState = lens id (\_ x -> x)

getLCAction :: Prism' Action (LocalCookingAction SiteLinks UserDetails)
getLCAction = prism' LocalCookingAction $ case _ of
  LocalCookingAction x -> Just x
  _ -> Nothing


spec :: forall eff
      . LocalCookingParams SiteLinks UserDetails (Effects eff)
     -> T.Spec (Effects eff) State Unit Action
spec {siteLinks,toURI} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      Clicked x -> liftEff (siteLinks x)
      LocalCookingAction a -> performActionLocalCooking getLCState a props state

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ button
        { color: Button.primary
        , disabled: state.currentPage == UsersLink
          || ( case state.userDetails of
                  Just (UserDetails {roles})
                    | Array.elem Admin roles -> false
                    | otherwise -> true
                  _ -> true
             )
        , onClick: mkEffFn1 preventDefault
        , onTouchTap: mkEffFn1 \e -> do
            preventDefault e
            dispatch (Clicked UsersLink)
        , href: URI.print $ toURI $ toLocation UsersLink
        , variant: Button.raised
        } [R.text "Users"]
      ]


topbarButtons :: forall eff
               . LocalCookingParams SiteLinks UserDetails (Effects eff)
              -> R.ReactElement
topbarButtons params =
  let {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec params
        )
        (initialState (unsafePerformEff (initLocalCookingState params)))
      reactSpec' =
          whileMountedLocalCooking
            params
            "Spec.Topbar.Buttons"
            LocalCookingAction
            (\this -> unsafeCoerceEff <<< dispatcher this)
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
