{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  , MultiParamTypeClasses
  , OverloadedStrings
  #-}

module Server.Dependencies.PermissionToken.Admin where

import LocalCooking.Common.AccessToken (AccessToken)
import LocalCooking.Common.AccessToken.Auth (AuthToken)
import LocalCooking.Common.AccessToken.Permission (PermissionToken)

import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=), object, Value (Object))
import Data.Aeson.Types (typeMismatch)
import Data.Hashable (Hashable)
import Control.Newtype (Newtype (..))
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)



newtype AdminToken = AdminToken
  { getAdminToken :: PermissionToken
  } deriving (Eq, Show, PersistField, PersistFieldSql, Hashable, Generic, Arbitrary, FromJSON, ToJSON)

instance Newtype AdminToken AccessToken where
  pack = AdminToken . pack
  unpack = unpack . getAdminToken


data AdminTokenInitIn
  = AdminTokenInitIn
    { adminTokenInitInOwner :: AuthToken
    }

instance FromJSON AdminTokenInitIn where
  parseJSON json = case json of
    Object o -> AdminTokenInitIn <$> o .: "owner"
    _ -> fail
    where
      fail = typeMismatch "AdminTokenInitIn" json
