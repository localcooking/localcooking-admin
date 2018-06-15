module Spec.Dialogs.User where

import Links (SiteLinks)
import User (UserDetails)
import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Common.Form.Email as Email
import LocalCooking.Spec.Common.Form.Password as Password
import LocalCooking.Spec.Common.Form.Checkbox as Checkbox
import LocalCooking.Spec.Types.Env (Env)
import LocalCooking.Thermite.Params (LocalCookingParams)
import LocalCooking.Global.Links.Class (registerLink, class LocalCookingSiteLinks)
import LocalCooking.Common.User.Password (HashedPassword, hashPassword)
import LocalCooking.Common.User.Role (UserRole (..))
import LocalCooking.Semantics.Common (User (..))
import LocalCooking.Semantics.Admin (SetUser (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location, toLocation, class ToLocation)
import Data.UUID (genUUID, GENUUID)
import Data.Array as Array
import Data.Generic (class Generic, gEq, gCompare)
import Text.Email.Validate (EmailAddress)
import Text.Email.Validate as Email
import Control.Monad.Base (liftBase)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff, unsafePerformEff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)

import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.DOM.Props.PreventDefault (preventDefault)
import DOM (DOM)
import DOM.HTML.Window.Extra (WindowSize)

import MaterialUI.Types (createStyles)
import MaterialUI.Button (button)
import MaterialUI.Button as Button
import MaterialUI.Checkbox (checkbox)
import MaterialUI.Form (formControl, formGroup, formLabel, formControlLabel)
import Crypto.Scrypt (SCRYPT)

import Queue.Types (readOnly, writeOnly)
import Queue (WRITE)
import Queue.One as One
import Queue.One.Aff as OneIO
import IxQueue (IxQueue)
import IxQueue as IxQueue
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal




type Effects eff =
  ( ref       :: REF
  , uuid      :: GENUUID
  , exception :: EXCEPTION
  , scrypt    :: SCRYPT
  , console   :: CONSOLE
  , dom       :: DOM
  , timer     :: TIMER
  | eff)


data EmailConfirmed = EmailConfirmed

derive instance gnenericEmailConfirmed :: Generic EmailConfirmed

instance eqEmailConfirmed :: Eq EmailConfirmed where
  eq = gEq

instance ordEmailConfirmed :: Ord EmailConfirmed where
  compare = gCompare

instance showEmailConfirmed :: Show EmailConfirmed where
  show EmailConfirmed = "Has confirmed email"


userDialog :: forall eff
            . LocalCookingParams SiteLinks UserDetails (Effects eff)
           -> { userDialogQueue :: OneIO.IOQueues (Effects eff) User (Maybe SetUser)
              , userCloseQueue  :: One.Queue (write :: WRITE) (Effects eff) Unit
              , env             :: Env
              }
           -> R.ReactElement
userDialog
  params
  { userDialogQueue: userDialogQueue@OneIO.IOQueues {output: userDialogOutputQueue}
  , userCloseQueue
  , env
  } =
  genericDialog
  params
  { dialogQueue: userDialogQueue
  , closeQueue: Just userCloseQueue
  , buttons: \{close,input: u} ->
    [ button
      { color: Button.secondary
      , onTouchTap: mkEffFn1 \_ ->
          unsafeCoerceEff $ One.putQueue userDialogOutputQueue $ Just $ SetUserDelete u
      } [R.text "Delete"]
    ]
  , title: "User"
  , submitValue: "Save"
  , pends: true
  , content:
    { component: \{submitDisabled,input: User {email,roles,emailConfirmed}} ->
      let _ = unsafePerformEff $ do
            k <- show <$> genUUID
            let submitValue = do
                  mEmail <- IxSignal.get emailSignal
                  x <- case mEmail of
                    Email.EmailGood _ -> pure false
                    _ -> pure true
                  submitDisabled x
            IxQueue.onIxQueue emailQueue k \_ -> submitValue
            IxQueue.onIxQueue passwordQueue k \_ -> submitValue
            IxSignal.subscribe (\_ -> submitValue) emailSignal
            IxSignal.subscribe (\_ -> submitValue) passwordSignal
            IxSignal.set roles rolesSignal
            IxSignal.set (if emailConfirmed then [EmailConfirmed] else []) confirmedSignal
            void $ setTimeout 200 $
              One.putQueue setQueue (Email.EmailGood email)
      in  [ Email.email
            { label: R.text "Email"
            , fullWidth: true
            , name: "login-email"
            , id: "login-email"
            , emailSignal: emailSignal
            , parentSignal: Nothing
            , updatedQueue: emailQueue
            , setQueue
            }
          , Password.password
            { label: R.text "Password"
            , fullWidth: true
            , name: "login-password"
            , id: "login-password"
            , passwordSignal: passwordSignal
            , parentSignal: Nothing
            , updatedQueue: passwordQueue
            , errorQueue: passwordErrorQueue
            }
          , Checkbox.checkboxes
            { entriesSignal: rolesSignal
            , label: "User Roles"
            , entries: [Customer, Chef, Farmer, Editor, Manager, Admin]
            }
          , Checkbox.checkboxes
            { entriesSignal: confirmedSignal
            , label: "Email Confirmed"
            , entries: [EmailConfirmed]
            }
          ]
    , obtain: \(User {id,created,socialLogin}) -> do
      mEmail <- liftEff (IxSignal.get emailSignal)
      case mEmail of
        Email.EmailGood email -> do
          newPassword <- do
            pw <- liftEff (IxSignal.get passwordSignal)
            if pw == ""
              then pure Nothing
              else Just <$> liftBase (hashPassword {salt: env.salt, password: pw})
          roles <- liftEff (IxSignal.get rolesSignal)
          emailConfirmed <- not <<< Array.null <$> liftEff (IxSignal.get confirmedSignal)
          pure $ Just $ SetUserUpdate
            { user: User {id,created,email,socialLogin,emailConfirmed,roles}
            , newPassword
            }
        _ -> do
          liftEff $ log "bad email!" -- FIXME bug out somehow?
          pure Nothing
    , reset: do
      IxSignal.set "" passwordSignal
    }
  }
  where
    emailSignal = unsafePerformEff $ IxSignal.make $ Email.EmailPartial ""
    passwordSignal = unsafePerformEff $ IxSignal.make ""
    emailQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordQueue = unsafePerformEff $ readOnly <$> IxQueue.newIxQueue
    passwordErrorQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    setQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    rolesSignal = unsafePerformEff $ IxSignal.make []
    confirmedSignal = unsafePerformEff $ IxSignal.make []
