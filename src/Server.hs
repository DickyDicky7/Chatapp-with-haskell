{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
-- {-# LANGUAGE QuasiQuotes           #-}
-- {-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE OverloadedLabels      #-}

module Server
  ( start
  ) where

import qualified AuthMiddleware                          as Middleware
import qualified Data.Aeson                              as JSON
import qualified Data.Map                                as Map
import qualified Data.Time                               as Time
import           Data.UUID                                ( UUID(..) )
import qualified Data.UUID                               as UUID
import qualified Data.UUID.V4                            as UUID
import qualified JWTConfig                               as JWT
import qualified Libjwt.Classes                          as JWT
import qualified Model
import qualified Network.Wai                             as Server
import qualified Network.Wai.Handler.Warp                as Server
import qualified Network.Wai.Handler.WebSockets          as Socket
import qualified Network.Wai.Middleware.Cors             as Middleware
import qualified Network.Wai.Middleware.HttpAuth         as Middleware
import qualified Network.Wai.Middleware.RequestLogger    as Middleware
import qualified Network.Wai.Middleware.Routed           as Middleware
import qualified Network.Wai.Middleware.Static           as Middleware
import qualified Network.Wai.Middleware.Timeout          as Middleware
import qualified Network.WebSockets                      as Socket
import qualified Network.WebSockets.Connection           as Socket
import qualified Text.Pretty.Simple                      as Simple
import           Universum
import qualified Universum.Unsafe                        as Unsafe
import qualified View
import qualified Web.Cookie                              as Cookie
import qualified Web.Twain                               as Server

type Clients = Map UUID Socket.Connection
initClients :: Clients
initClients = Map.empty

start :: IO ()
start = newTVarIO initClients >>= \clientsTVar -> Simple.pPrintString "Listening on localhost:8080"
  >> Server.run 8080 (foldr ($) (application clientsTVar) middlewares)

middlewares :: [Server.Middleware]
middlewares =
  [ Middleware.static
  , Middleware.simpleCors
  , Middleware.logStdoutDev
  , Middleware.timeoutAs requestTimeoutPageHandler 30
  , Middleware.routedMiddleware ("auth" `elem`) Middleware.jwtAuthMiddleware
  ]

requestTimeoutPageHandler :: Server.Response
requestTimeoutPageHandler = Server.status Server.status408 (Server.html (View.page View.requestTimeout))

homePageHandler :: Server.ResponderM ()
homePageHandler = Server.send (Server.html (View.page View.home))

chatPageHandler :: Server.ResponderM ()
chatPageHandler = Server.send (Server.html (View.page View.chat))

notFound404PageHandler :: Server.ResponderM ()
notFound404PageHandler = Server.send (Server.html (View.page View.notFound404))

type Time = Text
getCurrentLocalTime :: IO Time
getCurrentLocalTime = Time.getCurrentTime >>= \utcTime ->
  Time.getCurrentTimeZone >>= \timeZone -> pure (formatLocalTime (Time.utcToLocalTime timeZone utcTime))

formatLocalTime :: Time.LocalTime -> Time
formatLocalTime = toText . Time.formatTime Time.defaultTimeLocale "%X"

application :: TVar Clients -> Server.Application
application clientsTVar = Socket.websocketsOr Socket.defaultConnectionOptions (socketServer clientsTVar) restServer

addConnection :: TVar Clients -> Socket.Connection -> IO UUID
addConnection clientsTVar connection = UUID.nextRandom >>= \uuid -> readTVarIO clientsTVar
  >>= \clients -> atomically (writeTVar clientsTVar (Map.insert uuid connection clients)) >> pure uuid

removeConnection :: TVar Clients -> UUID -> IO ()
removeConnection clientsTVar uuid =
  readTVarIO clientsTVar >>= \clients -> atomically (writeTVar clientsTVar (Map.delete uuid clients))

forkConnection :: Socket.Connection -> IO () -> IO ()
forkConnection connection = Socket.withPingThread connection 30 (pure ())

socketServer :: TVar Clients -> Socket.ServerApp
socketServer clientsTVar pendingConnection = Socket.acceptRequest pendingConnection >>= \connection -> forkConnection
  connection
  (   addConnection clientsTVar connection
  >>= \uuid -> talk clientsTVar connection `onException` removeConnection clientsTVar uuid
  )

getWebSocketRequest :: Socket.Connection -> IO (Maybe Model.WebSocketRequest)
getWebSocketRequest connection = Socket.receiveData connection <&> JSON.decode

informError :: Socket.Connection -> Time -> IO ()
informError connection localTime =
  connection `Socket.sendTextData` JSON.encode (Model.WebSocketResponse localTime "system" "Something went wrong 😢")

talk :: TVar Clients -> Socket.Connection -> IO ()
talk clientsTVar connection = forever
  (getCurrentLocalTime >>= \localTime ->
    getWebSocketRequest connection >>= maybe (informError connection localTime) (broadcast clientsTVar localTime)
  )

broadcast :: TVar Clients -> Time -> Model.WebSocketRequest -> IO ()
broadcast clientsTVar time Model.WebSocketRequest {..} = readTVarIO clientsTVar
  >>= \clients -> for_ clients (`Socket.sendTextData` JSON.encode (Model.WebSocketResponse time name message))

restServer :: Server.Application
restServer = foldr
  ($)
  (Server.notFound notFound404PageHandler)
  [ Server.get "/" homePageHandler
  , Server.get "/chat" chatPageHandler
  , Server.get "/cookies" sendCookies
  , Server.get "/view-cookies" viewCookies
  , Server.get "/delete-cookies" expireCookies
  , Server.get "/jwt-key" sendJWTKey
  , Server.post "/auth/somepath" somepathHandler
  ]

sendJWTKey :: Server.ResponderM ()
sendJWTKey = do
  let username = "SirDicky"
  let password = "12345678"
  let role     = Model.Admin
  payloadConfig <- liftIO JWT.payloadConfig
  payload       <- liftIO (JWT.makeAuthClaimsPayload Model.AuthClaims { .. } payloadConfig)
  jwt           <- liftIO (JWT.makeJWTAuthClaims payload)
  Server.send (Server.withHeader ("jwt", jwt) (Server.text "A JWT was sent"))

somepathHandler :: Server.ResponderM ()
somepathHandler = Server.send (Server.text "You are passed")

sendCookies :: Server.ResponderM ()
sendCookies = do
  let cookie = Cookie.defaultSetCookie { Cookie.setCookieName  = "I love cookies"
                                       , Cookie.setCookieValue = "They are so delicious"
                                       --, Cookie.setCookieMaxAge = Just (Time.secondsToDiffTime 300)
                                       }
  Server.send (Server.withCookie' cookie (Server.text "I send you a cookie"))

expireCookies :: Server.ResponderM ()
expireCookies = Server.send (Server.expireCookie "I love cookies" (Server.text "Oops! I have just deleted the cookie"))

viewCookies :: Server.ResponderM ()
viewCookies = do
  h <- Server.headers
  Simple.pPrint h
  Server.send (Server.text "Did i see it?")
