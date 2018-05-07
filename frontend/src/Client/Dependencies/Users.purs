module Client.Dependencies.Users where

import Prelude
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn, AuthInitOut)

import Data.Argonaut (class DecodeJson, decodeJson, (.?))
import Data.Argonaut.JSONUnit (JSONUnit)
import Text.Email.Validate (EmailAddress)
import Sparrow.Client.Queue (SparrowStaticClientQueues)



newtype UserListing = UserListing
  { email :: EmailAddress
  , roles :: Array UserRole
  }

instance decodeJsonUserListing :: DecodeJson UserListing where
  decodeJson json = do
    o <- decodeJson json
    email <- o .? "email"
    roles <- o .? "roles"
    pure (UserListing {email,roles})


type UsersInitIn = AuthInitIn AuthToken JSONUnit

type UsersInitOut = AuthInitOut (Array UserListing)

type UsersSparrowClientQueues eff =
  SparrowStaticClientQueues eff UsersInitIn UsersInitOut
