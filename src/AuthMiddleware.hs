{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AuthMiddleware
  ( jwtAuthMiddleware
  ) where

import qualified Data.ByteString.Char8                   as Strict
import qualified Data.List                               as List
import qualified JWTConfig                               as JWT
import qualified Model
import qualified Network.Wai                             as Server
import           Universum
import qualified Universum.Unsafe                        as Unsafe
import qualified Web.Twain                               as Server

-- * JWT Authorization *
type AuthorizationHeader = ByteString
getAuthorizationHeader :: Server.Request -> IO (Maybe AuthorizationHeader)
getAuthorizationHeader = pure . List.lookup "Authorization" . Server.requestHeaders

type Message = Text
response401Unauthorized :: Message -> Model.RestResponse ()
response401Unauthorized message = Model.RestResponse 401 message Nothing

type Respond = (Server.Response -> IO Server.ResponseReceived)
respond401Unauthorized :: Respond -> Message -> IO Server.ResponseReceived
respond401Unauthorized respond message =
  respond (Server.status Server.status401 (Server.json (response401Unauthorized message)))

type Token = ByteString
extractBearerAuth :: AuthorizationHeader -> IO Token
extractBearerAuth = pure . Strict.drop 7

handleAuthorizationHeader
  :: Server.Application -> Server.Request -> Respond -> AuthorizationHeader -> IO Server.ResponseReceived
handleAuthorizationHeader application request respond header = extractBearerAuth header >>= \token ->
  JWT.validationConfig >>= \config ->
    JWT.additionalValidationConfig
      >>= (   JWT.decodeAndValidateFull token config
          >=> either (respond `respond401Unauthorized`) (const (application request respond))
          )

jwtAuthMiddleware :: Server.Middleware
jwtAuthMiddleware application request respond = getAuthorizationHeader request >>= maybe
  (respond `respond401Unauthorized` "Missing Authorization header")
  (handleAuthorizationHeader application request respond)
-- *
