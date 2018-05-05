module Spec.Topbar.Buttons where

import Links (SiteLinks (UsersLink))
import LocalCooking.Links.Class (toLocation)
import LocalCooking.Common.User.Role (UserRole (Admin))
import User (UserDetails (..))

import Prelude
import Data.URI (URI)
import Data.URI.URI as URI
import Data.URI.Location (Location)
import Data.UUID (GENUUID)
import Data.Array as Array
import Data.Maybe (Maybe (..))
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


type State =
  { currentPage :: SiteLinks
  , userDetails :: Maybe UserDetails
  }

initialState :: { initSiteLinks :: SiteLinks
                , initUserDetails :: Maybe UserDetails
                } -> State
initialState {initSiteLinks,initUserDetails} =
  { currentPage: initSiteLinks
  , userDetails: initUserDetails
  }

data Action
  = ChangedCurrentPage SiteLinks
  | ChangedUserDetails (Maybe UserDetails)
  | Clicked SiteLinks


spec :: forall eff
      . { siteLinks :: SiteLinks -> Eff (Effects eff) Unit
        , toURI :: Location -> URI
        }
     -> T.Spec (Effects eff) State Unit Action
spec {siteLinks,toURI} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      ChangedCurrentPage p -> void $ T.cotransform _ { currentPage = p }
      ChangedUserDetails u -> void $ T.cotransform _ { userDetails = u }
      Clicked x -> liftEff (siteLinks x)

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
               . { currentPageSignal :: IxSignal (Effects eff) SiteLinks
                 , siteLinks :: SiteLinks -> Eff (Effects eff) Unit
                 , toURI :: Location -> URI
                 , userDetailsSignal :: IxSignal (Effects eff) (Maybe UserDetails)
                 } -> R.ReactElement
topbarButtons
  { toURI
  , siteLinks
  , currentPageSignal
  , userDetailsSignal
  } =
  let init =
        { initSiteLinks: unsafePerformEff $ IxSignal.get currentPageSignal
        , initUserDetails: unsafePerformEff $ IxSignal.get userDetailsSignal
        }
      {spec:reactSpec,dispatcher} = T.createReactSpec
        ( spec
          { toURI
          , siteLinks
          }
        )
        (initialState init)
      reactSpec' =
          Signal.whileMountedIxUUID
            currentPageSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedCurrentPage x))
        $ Signal.whileMountedIxUUID
            userDetailsSignal
            (\this x -> unsafeCoerceEff $ dispatcher this (ChangedUserDetails x))
            reactSpec
  in  R.createElement (R.createClass reactSpec') unit []
