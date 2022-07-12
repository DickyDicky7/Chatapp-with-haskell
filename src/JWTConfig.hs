{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module JWTConfig
  ( payloadConfig
  , validationConfig
  , makeJWTAuthClaims
  , makeAuthClaimsPayload
  , decodeAndValidateFull
  , additionalValidationConfig
  ) where

import qualified Data.Either.Validation                  as Validation
import qualified Data.Time                               as Time
import qualified LoadEnv                                 as Env
import qualified Model
import qualified System.Environment                      as Env
import           Universum
import qualified Universum.Unsafe                        as Unsafe
import qualified Web.Libjwt                              as JWT
import           Web.Libjwt                               ( type (->>) )

-- * Environment Variable Secret *
type SecretName = String
loadHMAC512Secret :: IO (SecretName, Maybe JWT.Secret)
loadHMAC512Secret =
  Env.loadEnvFrom "AuthEnv.env"
    >> let envVariableName = "HMAC512_SECRET"
       in  (envVariableName, ) <$> (Env.lookupEnv envVariableName <&> map (JWT.MkSecret . fromString))

hmac512Secret :: IO (JWT.Algorithm JWT.Secret)
hmac512Secret = loadHMAC512Secret >>= \(secretName, maybeSecret) ->
  maybe (throwM (Model.EnvVariableNotFound secretName)) (pure . JWT.HMAC512) maybeSecret
-- *

-- * Payload *
type JWTAuthClaimsBuilder = JWT.JwtBuilder (JWT.Claims Model.AuthClaims) (JWT.OutNs Model.AuthClaims)
payloadConfig :: IO JWTAuthClaimsBuilder
payloadConfig =
  Time.getCurrentTime
    <&> Time.addUTCTime 120 {- 120 seconds ~ 2 minutes -}
    >>= \expiredTime -> pure (JWT.expiresAt expiredTime)

type JWTAuthClaimsPayload = JWT.Payload (JWT.Claims Model.AuthClaims) (JWT.OutNs Model.AuthClaims)
makeAuthClaimsPayload :: Model.AuthClaims -> JWTAuthClaimsBuilder -> IO JWTAuthClaimsPayload
makeAuthClaimsPayload claims config = JWT.jwtPayload config claims
-- *

-- * JWT Token *
type Token = ByteString
makeJWTAuthClaims :: JWTAuthClaimsPayload -> IO Token
makeJWTAuthClaims payload = JWT.getToken <$> (hmac512Secret <&> (`JWT.sign` payload))
-- *

-- * Decode And Validate *
validationConfig :: IO JWT.ValidationSettings
validationConfig = pure JWT.defaultValidationSettings

type JWTAdditionalValidationSettings
  = JWT.JwtValidation '["username" ->> Text , "password" ->> Text , "role" ->> Model.Role] 'JWT.NoNs
additionalValidationConfig :: IO JWTAdditionalValidationSettings
additionalValidationConfig = pure (JWT.checkClaim (== Model.Admin) #role)

type JWTAuthClaims = JWT.Jwt '["username" ->> Text , "password" ->> Text , "role" ->> Model.Role] 'JWT.NoNs
type JWTAuthClaimsValidation = JWT.ValidationNEL JWT.ValidationFailure (JWT.Validated JWTAuthClaims)
decodeAndValidate :: Token -> JWT.ValidationSettings -> JWTAdditionalValidationSettings -> IO JWTAuthClaimsValidation
decodeAndValidate token config additionalConfig =
  hmac512Secret >>= \algorithmSecret -> JWT.jwtFromByteString config additionalConfig algorithmSecret token

getAuthClaims :: JWT.Validated JWTAuthClaims -> Model.AuthClaims
getAuthClaims = JWT.fromPrivateClaims . JWT.privateClaims . JWT.payload . JWT.getValid

handleJWTAuthClaimsValidation :: JWTAuthClaimsValidation -> IO EitherErrorAuthClaims
handleJWTAuthClaimsValidation = pure . either (Left . show) (Right . getAuthClaims) . Validation.validationToEither

onAuthClaimsValidationError :: JWT.SomeDecodeException -> IO EitherErrorAuthClaims
onAuthClaimsValidationError = pure . Left . toText . displayException

type EitherErrorAuthClaims = Either Text Model.AuthClaims
decodeAndValidateFull :: Token -> JWT.ValidationSettings -> JWTAdditionalValidationSettings -> IO EitherErrorAuthClaims
decodeAndValidateFull token config additionalConfig =
  (decodeAndValidate token config additionalConfig >>= handleJWTAuthClaimsValidation)
    `catch` onAuthClaimsValidationError
-- *
