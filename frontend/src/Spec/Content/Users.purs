module Spec.Content.Users where

-- import Client.Dependencies.Users.Get (UserListing (..), GetUsersSparrowClientQueues)
-- import Client.Dependencies.Users.Set (SetUserInitIn' (..), SetUserSparrowClientQueues)
import LocalCooking.Dependencies.AccessToken.Generic (AccessInitIn (..))
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Common.User.Password (HashedPassword)
import LocalCooking.Semantics.Common (User (..))
import LocalCooking.Semantics.Admin (SetUser (..), NewUser (..))
import LocalCooking.Dependencies.Admin (GetUsersSparrowClientQueues, SetUserSparrowClientQueues)

import Prelude
import Data.Maybe (Maybe (..))
import Data.Argonaut.JSONUnit (JSONUnit (..))
import Data.Foldable (intercalate)
import Control.Monad.Base (liftBase)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Eff.Console (CONSOLE, log)
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
  , console :: CONSOLE
  | eff)


type State =
  { users :: Maybe (Array User)
  }

initialState :: State
initialState =
  { users: Nothing
  }

data Action
  = GotUsers (Array User)
  | ClickedUser User



spec :: forall eff
      . { userDialogQueue :: OneIO.IOQueues (Effects eff) User (Maybe SetUser)
        , userCloseQueue  :: One.Queue (write :: WRITE) (Effects eff) Unit
        , setUserQueues   :: SetUserSparrowClientQueues (Effects eff)
        , getUsersQueues  :: GetUsersSparrowClientQueues (Effects eff)
        , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
        }
     -> T.Spec (Effects eff) State Unit Action
spec
  { userDialogQueue
  , userCloseQueue
  , setUserQueues
  , getUsersQueues
  , authTokenSignal
  } = T.simpleSpec performAction render
  where
    performAction action props state = case action of
      GotUsers xs -> void $ T.cotransform _ { users = Just xs }
      ClickedUser u@(User {email,roles}) -> do
        mAuthToken <- liftEff (IxSignal.get authTokenSignal)
        case mAuthToken of
          Nothing -> liftEff $ log "no auth token"
          Just token -> do
            mSetUser <- liftBase (OneIO.callAsync userDialogQueue u)
            case mSetUser of
              Just xs -> do
                mResult <- liftBase (OneIO.callAsync setUserQueues (AccessInitIn {token, subj: xs}))
                case mResult of
                  Nothing -> pure unit
                  Just JSONUnit -> do
                    liftEff (One.putQueue userCloseQueue unit)
                    mXs <- liftBase (OneIO.callAsync getUsersQueues (AccessInitIn {token, subj: JSONUnit}))
                    case mXs of
                      Just xs -> performAction (GotUsers xs) props state
                      Nothing -> pure unit
              Nothing -> pure unit

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
                map (\u@(User {email,roles}) ->
                      tableRow {hover: true, onClick: mkEffFn1 \_ -> dispatch (ClickedUser u)}
                      [ tableCell {} $ R.text $ Email.toString email
                      , tableCell {} $ R.text $ intercalate ", " $ show <$> roles
                      ]
                    ) xs
            ]
      ]


users :: forall eff
       . { getUsersQueues :: GetUsersSparrowClientQueues (Effects eff)
         , setUserQueues :: SetUserSparrowClientQueues (Effects eff)
         , userDialogQueue :: OneIO.IOQueues (Effects eff) User (Maybe SetUser)
         , userCloseQueue :: One.Queue (write :: WRITE) (Effects eff) Unit
         , authTokenSignal :: IxSignal (Effects eff) (Maybe AuthToken)
         } -> R.ReactElement
users
  { authTokenSignal
  , getUsersQueues: getUsersQueues@OneIO.IOQueues {input: getUsersInput, output: getUsersOutput}
  , setUserQueues
  , userDialogQueue
  , userCloseQueue
  } =
  let {spec: reactSpec, dispatcher} =
        T.createReactSpec
          ( spec
            { userDialogQueue
            , userCloseQueue
            , getUsersQueues
            , setUserQueues
            , authTokenSignal
            }
          ) initialState
      reactSpec' =
          Queue.whileMountedOne
            (allowReading getUsersOutput)
            (\this mxs -> case mxs of
                Just xs -> unsafeCoerceEff $ dispatcher this $ GotUsers xs
                Nothing -> unsafeCoerceEff $ log "bad users queue response"
            )
        $ reactSpec
          { componentDidMount = \_ -> unsafeCoerceEff $ do
            -- FIXME onAvailable
            mAuthToken <- IxSignal.get authTokenSignal
            case mAuthToken of
              Nothing -> log "no auth token"
              Just token ->
                One.putQueue (allowWriting getUsersInput) (AccessInitIn {token, subj: JSONUnit})
          }
  in  R.createElement (R.createClass reactSpec') unit []
