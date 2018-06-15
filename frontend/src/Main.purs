module Main where

import Links (SiteLinks (..), ImageLinks (Logo40Png), initSiteLinks)
import Colors (palette)
import User (UserDetails (..), PreUserDetails (..))
import Spec.Topbar.Buttons (topbarButtons)
import Spec.Content (content)
import Spec.Content.UserDetails (userDetails)
import LocalCooking.Spec.Misc.Branding (mainBrand)
import LocalCooking.Spec.Misc.Icons.ChefHat (chefHatViewBox, chefHat)
import LocalCooking.Main (defaultMain)
import LocalCooking.Common.User.Role (UserRole (Admin))
import LocalCooking.Types.ServerToClient (env)
import LocalCooking.Dependencies.Admin (newAdminQueues, adminDependencies, GetUsersSparrowClientQueues, SetUserSparrowClientQueues)
import LocalCooking.Semantics.Common (User (..))

import Sparrow.Client (unpackClient)
import Sparrow.Client.Queue (newSparrowClientQueues, newSparrowStaticClientQueues, sparrowClientQueues, sparrowStaticClientQueues)
import Sparrow.Types (Topic (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.UUID (GENUUID)
import Data.URI.Location (toLocation)
import Data.Array as Array
import Control.Monad.Aff (sequential)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Timer (TIMER)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Execution.Immediate (SET_IMMEDIATE_SHIM)

import React as R
import React.DOM as R
import React.DOM.SVG as RS
import React.DOM.Props as RP
import MaterialUI.InjectTapEvent (INJECT_TAP_EVENT)
import MaterialUI.Divider (divider)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.SvgIcon (svgIcon)
import MaterialUI.SvgIcon as SvgIcon
import MaterialUI.ListItem (listItem)
import MaterialUI.ListItemIcon (listItemIcon)
import MaterialUI.ListItemText (listItemText)
import MaterialUI.Types (createStyles)
import MaterialUI.Icons.People (peopleIcon)
import DOM (DOM)
import DOM.HTML.Types (HISTORY)
import WebSocket (WEBSOCKET)
import Network.HTTP.Affjax (AJAX)
import Browser.WebStorage (WEB_STORAGE)
import Crypto.Scrypt (SCRYPT)


-- | All top-level effects
type Effects =
  ( console            :: CONSOLE
  , injectTapEvent     :: INJECT_TAP_EVENT
  , set_immediate_shim :: SET_IMMEDIATE_SHIM
  , ref                :: REF
  , dom                :: DOM
  , timer              :: TIMER
  , uuid               :: GENUUID
  , exception          :: EXCEPTION
  , history            :: HISTORY
  , now                :: NOW
  , ws                 :: WEBSOCKET
  , ajax               :: AJAX
  , webStorage         :: WEB_STORAGE
  , scrypt             :: SCRYPT
  )


main :: Eff Effects Unit
main = do
  log "Starting Local Cooking Admin frontend..."

  initSiteLink <- initSiteLinks

  adminQueues <- newAdminQueues

  defaultMain
    { env
    , initSiteLinks: initSiteLink
    , palette
    , siteQueues: adminQueues
    , deps: adminDependencies
    , leftDrawer:
      { buttons: \{siteLinks} ->
        [ divider {}
        , listItem
          { button: true
          , onClick: mkEffFn1 \_ -> unsafeCoerceEff (siteLinks UsersLink)
          }
          [ listItemIcon {} peopleIcon
          , listItemText
            { primary: "Meals"
            }
          ]
        ]
      }
    , topbar:
      { imageSrc: toLocation Logo40Png
      , buttons: \params ->
        [ topbarButtons
          params
        ]
      }
    , content: \params ->
      [ content
        params
        { getUsersQueues: adminQueues.getUsersQueues
        , setUserQueues: adminQueues.setUserQueues
        , env
        }
      ]
    , userDetails:
      { buttons: \_ -> []
      , content: \{currentPageSignal,siteLinks} ->
        [ userDetails {currentPageSignal,siteLinks}
        ]
      , obtain: \{user} -> do
        PreUserDetails mUser <- sequential $ PreUserDetails <$> user
        case mUser of
          Just user -> pure $ Just $ UserDetails {user}
          _ -> pure Nothing
      }
    , extraRedirect: \link mUserDetails -> case link of
        UsersLink -> case mUserDetails of
          Just (UserDetails {user: User {roles}})
            | Admin `Array.elem` roles -> Nothing
            | otherwise -> Just RootLink
          Nothing -> Just RootLink
        _ -> Nothing
    , extendedNetwork:
      [ Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#c62828"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#ff5f52"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Customers"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#1b5e20"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#4c8c4a"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://farm.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Farms"
          ]
      , R.text " "
      , Button.withStyles
        (\_ ->
          { root: createStyles
            { background: "#1565c0"
            , color: "#fff"
            , textTransform: "none"
            , "&:hover":
              { background: "#5e92f3"
              }
            }
          }
        )
        \{classes} ->
          button
          { href: "https://chef.localcooking.com/"
          , classes: Button.createClasses classes
          , variant: Button.raised
          }
          [ svgIcon
            { viewBox: "0 0 279 279"
            , color: SvgIcon.inherit
            }
            [ mainBrand
            ]
          , R.text " Chefs"
          ]

      ]
    }
