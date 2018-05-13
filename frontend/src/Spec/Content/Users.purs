module Spec.Content.Users where

import Client.Dependencies.Users.Get (UserListing (..), GetUsersSparrowClientQueues)
import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitOut (..), AuthInitIn (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.Password (HashedPassword)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.Foldable (intercalate)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Uncurried (mkEffFn1)
import Text.Email.Validate as Email
import Text.Email.Validate (EmailAddress)

import Thermite as T
import React as R
import React.DOM as R
import React.DOM.Props as RP
import React.Queue.WhileMounted as Queue

import MaterialUI.Typography (typography)
import MaterialUI.Typography as Typography
import MaterialUI.Divider (divider)
import MaterialUI.CircularProgress (circularProgress)
import MaterialUI.CircularProgress as CircularProgress
import MaterialUI.Table (table, tableBody, tableCell, tableHead, tableRow)

import Queue.Types (WRITE, allowReading, allowWriting)
import Queue.One.Aff as OneIO
import Queue.One as One
import IxSignal.Internal (IxSignal)
import IxSignal.Internal as IxSignal



type Effects eff =
  ( ref :: REF
  | eff)


type State =
  { users :: Maybe (Array UserListing)
  }

initialState :: State
initialState =
  { users: Nothing
  }

data Action
  = GotUsers (Array UserListing)
  | ClickedUser {email :: EmailAddress, roles :: Array UserRole}


spec :: forall eff
      . { userDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe {email :: EmailAddress, password :: HashedPassword})
        , userCloseQueue  :: One.Queue (write :: WRITE) (Effects eff) Unit
        }
     -> T.Spec (Effects eff) State Unit Action
spec {userDialogQueue,userCloseQueue} = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotUsers xs -> void $ T.cotransform _ { users = Just xs }
      ClickedUser {email,roles} -> do
        pure unit

    render :: T.Render State Unit Action
    render dispatch props state children =
      [ typography
        { variant: Typography.display1
        , align: Typography.center
        , color: Typography.primary
        } [R.text "Users"]
      , R.div [RP.style {marginBotton: "1em"}] []
      , divider {}
      , case state.users of
          Nothing -> circularProgress {mode: CircularProgress.indeterminate}
          Just xs -> table {}
            [ tableHead {}
              [ tableRow {}
                [ tableCell {} $ R.text "Email"
                , tableCell {} $ R.text "Roles"
                ]
              ]
            , tableBody {} $
                map (\(UserListing x@{email,roles}) ->
                      tableRow {hover: true} -- , onClick: mkEffFn1 \_ -> dispatch (ClickedUser x)
                      [ tableCell {} $ R.text $ Email.toString email
                      , tableCell {} $ R.text $ intercalate ", " $ show <$> roles
                      ]
                    ) xs
            ]
      ]


users :: forall eff
       . { getUsersQueues :: GetUsersSparrowClientQueues (Effects eff)
         , userDialogQueue :: OneIO.IOQueues (Effects eff) Unit (Maybe {email :: EmailAddress, password :: HashedPassword})
         , userCloseQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
         , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
         } -> R.ReactElement
users
  { authTokenSignal
  , getUsersQueues: OneIO.IOQueues {input: getUsersInput, output: getUsersOutput}
  , userDialogQueue
  , userCloseQueue
  } =
  let {spec: reactSpec, dispatcher} = T.createReactSpec (spec {userDialogQueue, userCloseQueue}) initialState
      reactSpec' =
          Queue.whileMountedOne
            (allowReading getUsersOutput)
            (\this mxs -> case mxs of
                Just (AuthInitOut {subj: xs}) -> unsafeCoerceEff $ dispatcher this $ GotUsers xs
                Just AuthInitOutNoAuth -> unsafeCoerceEff $ log "no auth"
                Nothing -> unsafeCoerceEff $ log "bad users queue response"
            )
        $ reactSpec
          { componentDidMount = \_ -> do
            mAuthToken <- unsafeCoerceEff (IxSignal.get authTokenSignal)
            case mAuthToken of
              Nothing -> unsafeCoerceEff $ log "no auth token"
              Just token ->
                unsafeCoerceEff (One.putQueue (allowWriting getUsersInput) (AuthInitIn {token, subj: JSONUnit}))
          }
  in  R.createElement (R.createClass reactSpec') unit []
