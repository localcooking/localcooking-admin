{-# LANGUAGE
    OverloadedStrings
  , NamedFieldPuns
  , RecordWildCards
  #-}

module Server.Dependencies.Users.Get where

import LocalCooking.Types (AppM)
import LocalCooking.Types.Env (Env (..))
import LocalCooking.Auth (usersAuthToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.User.Role (UserRole (Admin))
import LocalCooking.Database.Query.User (getUsers, hasRole)
import LocalCooking.Server.Dependencies.AccessToken.Generic (AuthInitIn (..), AuthInitOut (..))
import LocalCooking.Server.Dependencies.Pagination (PaginationInitIn, PaginationInitOut, PaginationDeltaIn, PaginationDeltaOut)

import Web.Dependencies.Sparrow.Types (Server, JSONVoid, staticServer)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), object, (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.JSONUnit (JSONUnit (..))
import Text.EmailAddress (EmailAddress)
import Control.Monad.Reader (ask)
import Control.Monad.IO.Class (liftIO)
import Control.Logging (warn')



data UserListing = UserListing
  { userListingEmail :: EmailAddress
  , userListingRoles :: [UserRole]
  }

instance ToJSON UserListing where
  toJSON UserListing{..} = object
    [ "email" .= userListingEmail
    , "roles" .= userListingRoles
    ]


type GetUsersInitIn = AuthInitIn AuthToken JSONUnit

type GetUsersInitOut = AuthInitOut [UserListing]


getUsersServer :: Server AppM []  GetUsersInitIn
                                  GetUsersInitOut
                                  JSONVoid
                                  JSONVoid
getUsersServer = staticServer $ \(AuthInitIn authToken JSONUnit) -> do
  Env{envDatabase} <- ask

  mRole <- do
    mUserId <- usersAuthToken authToken
    case mUserId of
      Nothing -> do
        warn' "auth token not recognized"
        pure Nothing
      Just userId -> do
        isAdmin <- liftIO (hasRole envDatabase userId Admin)
        if isAdmin
          then Just <$> liftIO (getUsers envDatabase)
          else pure Nothing

  case mRole of
    Nothing -> pure (Just AuthInitOutNoAuth)
    Just roles -> pure $ Just $ AuthInitOut $ map (uncurry UserListing) roles
