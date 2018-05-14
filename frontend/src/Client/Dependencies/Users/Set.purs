module Client.Dependencies.Users.Set where

import Prelude
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Common.User.Role (UserRole)
import LocalCooking.Client.Dependencies.AccessToken.Generic (AuthInitIn, AuthInitOut)

import Data.Argonaut (class DecodeJson, decodeJson, (.?), class EncodeJson, encodeJson, (:=), (~>), jsonEmptyObject)
import Data.Argonaut.JSONUnit (JSONUnit)
import Data.Maybe (Maybe)
import Text.Email.Validate (EmailAddress)
import Sparrow.Client.Queue (SparrowStaticClientQueues)




data SetUserInitIn'
  = SetUserDelete EmailAddress
  | SetUserUpdate
    { email :: EmailAddress
    , roles :: Array UserRole
    , password :: Maybe HashedPassword
    }

instance encodeJsonSetUserInitIn' :: EncodeJson SetUserInitIn' where
  encodeJson x = case x of
    SetUserDelete e -> "delete" := e ~> jsonEmptyObject
    SetUserUpdate {email,roles,password}
      ->  "update" :=
          (  "email" := email
          ~> "roles" := roles
          ~> "password" := password
          ~> jsonEmptyObject
          )
      ~> jsonEmptyObject

type SetUserInitIn = AuthInitIn AuthToken SetUserInitIn'

type SetUserInitOut = AuthInitOut JSONUnit

type SetUserSparrowClientQueues eff =
  SparrowStaticClientQueues eff SetUserInitIn SetUserInitOut
