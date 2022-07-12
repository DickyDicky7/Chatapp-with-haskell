{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Model
  ( WebSocketResponse(..)
  , WebSocketRequest(..)
  , EnvVariableError(..)
  , RestResponse(..)
  , AuthClaims(..)
  , Role(..)
  ) where

import qualified Data.Aeson                              as JSON
import qualified Libjwt.Classes                          as JWT
import           Universum
import qualified Universum.Unsafe                        as Unsafe
import qualified Web.Libjwt                              as JWT

-- * WebSocket *
data WebSocketResponse = WebSocketResponse
  { time    :: Text
  , name    :: Text
  , message :: Text
  }
  deriving (Eq, Generic, Show, JSON.ToJSON)

data WebSocketRequest = WebSocketRequest
  { name    :: Text
  , message :: Text
  }
  deriving (Eq, Generic, Show, JSON.FromJSON)
-- *

-- * Rest *
data RestResponse result = RestResponse
  { code    :: Int
  , message :: Text
  , result  :: Maybe result
  }
  deriving (Eq, Generic, Show, JSON.ToJSON)
-- *

-- * Auth *
data AuthClaims = AuthClaims
  { username :: Text
  , password :: Text
  , role     :: Role
  }
  deriving (Eq, Generic, Show, JWT.FromPrivateClaims, JWT.ToPrivateClaims)

data Role = Admin | User | NonUser
  deriving (Eq, Generic, Show, JWT.FromPrivateClaims, JWT.ToPrivateClaims)

instance JWT.JwtRep Text Role where
  rep   = show
  unRep = \case
    "NonUser" -> Just NonUser
    "Admin"   -> Just Admin
    "User"    -> Just User
    _         -> Nothing
-- *

-- * Environment *
data EnvVariableError = EnvVariableNotFound String
  deriving (Eq, Show)

instance Exception EnvVariableError where
  displayException (EnvVariableNotFound name) = "Environment variable not found: " <> name
-- *
