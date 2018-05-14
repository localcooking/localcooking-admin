module Spec.Dialogs.User where

import Spec.Dialogs.User.Roles as Roles
import Links (SiteLinks)
import User (UserDetails)
import LocalCooking.Spec.Dialogs.Generic (genericDialog)
import LocalCooking.Spec.Form.Email as Email
import LocalCooking.Spec.Form.Password as Password
import LocalCooking.Types.Env (Env)
import LocalCooking.Types.Params (LocalCookingParams)
import LocalCooking.Window (WindowSize)
import LocalCooking.Client.Dependencies.PasswordVerify (PasswordVerifySparrowClientQueues, PasswordVerifyInitIn (PasswordVerifyInitInUnauth), PasswordVerifyInitOut (PasswordVerifyInitOutSuccess))
import LocalCooking.Links.Class (registerLink, toLocation, class LocalCookingSiteLinks, class ToLocation)
import LocalCooking.Common.Password (HashedPassword, hashPassword)
import LocalCooking.Common.User.Role (UserRole (..))

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.URI (URI)
import Data.URI.URI (print) as URI
import Data.URI.Location (Location)
import Data.UUID (genUUID, GENUUID)
import Data.Array as Array
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
-- import React.Icons (facebookIcon, twitterIcon, googleIcon)
import DOM (DOM)

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


userDialog :: forall eff
            . LocalCookingParams SiteLinks UserDetails (Effects eff)
           -> { userDialogQueue :: OneIO.IOQueues (Effects eff)
                                   {email :: EmailAddress, roles :: Array UserRole}
                                   (Maybe (Maybe {email :: EmailAddress, password :: Maybe HashedPassword, roles :: Array UserRole}))
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
  , buttons: \{close} ->
    [ button
      { color: Button.secondary
      , onTouchTap: mkEffFn1 \_ ->
          unsafeCoerceEff $ One.putQueue userDialogOutputQueue (Just Nothing)
      } [R.text "Delete"]
    ]
  , title: "User"
  , submitValue: "Save"
  , pends: true
  , content:
    { component: \{submitDisabled,input: {email,roles}} ->
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
            void $ setTimeout 200 $
              One.putQueue setPartialQueue (Email.toString email)
      in  [ Email.email
            { label: R.text "Email"
            , fullWidth: true
            , name: "login-email"
            , id: "login-email"
            , emailSignal: emailSignal
            , parentSignal: Nothing
            , updatedQueue: emailQueue
            , setPartialQueue
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
          , Roles.roles
            { rolesSignal
            }
          ]
    , obtain: do
      mEmail <- liftEff (IxSignal.get emailSignal)
      case mEmail of
        Email.EmailGood email -> do
          pw <- liftEff (IxSignal.get passwordSignal)
          password <-
            if pw == ""
              then pure Nothing
              else Just <$> liftBase (hashPassword {salt: env.salt, password: pw})
          roles <- liftEff (IxSignal.get rolesSignal)
          pure (Just (Just {email,password,roles}))
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
    setPartialQueue = unsafePerformEff $ writeOnly <$> One.newQueue
    rolesSignal = unsafePerformEff $ IxSignal.make []
