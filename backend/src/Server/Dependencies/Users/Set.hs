{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  #-}

module Server.Dependencies.Users.Set where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Admin))
import LocalCooking.Common.Password (HashedPassword)
import LocalCooking.Database.Query.User (updateUser, deleteUser, hasRole)
import LocalCooking.Server.Dependencies.AccessToken.Generic (AuthInitIn (..), AuthInitOut (..))
import LocalCooking.Server.Dependencies.Pagination (PaginationInitIn, PaginationInitOut, PaginationDeltaIn, PaginationDeltaOut)

import Web.Dependencies.Sparrow.Types (Server, JSONVoid, staticServer)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.JSONUnit (JSONUnit (..))
import Text.EmailAddress (EmailAddress)
import Control.Applicative ((<|>))
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Logging (warn')



data SetUserInitIn'
  = SetUserDelete EmailAddress
  | SetUserUpdate
    { setUserEmail :: EmailAddress
    , setUserRoles :: [UserRole]
    , setUserPassword :: Maybe HashedPassword
    }

instance FromJSON SetUserInitIn' where
  parseJSON json = case json of
    Object o -> do
      let delete = SetUserDelete <$> o .: "delete"
          update = do
            o' <- o .: "update"
            SetUserUpdate <$> o' .: "email" <*> o' .: "roles" <*> o' .: "password"
      delete <|> update
    _ -> fail'
    where
      fail' = typeMismatch "SetUserInitIn'" json


type SetUserInitIn = AuthInitIn AuthToken SetUserInitIn'

type SetUserInitOut = AuthInitOut JSONUnit


setUserServer :: Server AppM []  SetUserInitIn
                                 SetUserInitOut
                                 JSONVoid
                                 JSONVoid
setUserServer = staticServer $ \(AuthInitIn authToken mUpdate) -> do
  Env{envDatabase} <- ask

  mUserId <- usersAuthToken authToken
  case mUserId of
    Nothing -> do
      warn' "auth token not recognized"
      pure Nothing
    Just userId -> do
      isAdmin <- liftIO (hasRole envDatabase userId Admin)
      if isAdmin
        then do liftIO $ case mUpdate of
                  SetUserDelete e -> deleteUser envDatabase e
                  SetUserUpdate e r p -> updateUser envDatabase e r p
                pure $ Just $ AuthInitOut JSONUnit
        else pure Nothing
