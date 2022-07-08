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
-- {-# LANGUAGE LambdaCase            #-}

module Server
  ( start
  ) where

import qualified Data.Aeson                              as JSON
import qualified Data.Map                                as Map
import qualified Data.Time                               as Time
import           Data.UUID                                ( UUID(..) )
import qualified Data.UUID                               as UUID
import qualified Data.UUID.V4                            as UUID
import qualified Network.Wai                             as Server
import qualified Network.Wai.Handler.Warp                as Server
import qualified Network.Wai.Handler.WebSockets          as Socket
import qualified Network.Wai.Middleware.Cors             as Middleware
import qualified Network.Wai.Middleware.RequestLogger    as Middleware
import qualified Network.Wai.Middleware.Static           as Middleware
import qualified Network.Wai.Middleware.Timeout          as Middleware
import qualified Network.WebSockets                      as Socket
import qualified Network.WebSockets.Connection           as Socket
import           Universum
import qualified Universum.Unsafe                        as Unsafe
import           View
import qualified Web.Twain                               as Server

type Clients = Map UUID Socket.Connection

initClients :: Clients
initClients = Map.empty

start :: IO ()
start = do
  clientsTVar <- newTVarIO initClients
  putStrLn @Text "Listening on localhost:8080"
  Server.run 8080 (foldr ($) (application clientsTVar) middlewares)

middlewares :: [Server.Middleware]
middlewares =
  [ Middleware.static
  , Middleware.logStdout
  , Middleware.simpleCors
  , Middleware.timeoutAs requestTimeoutPageHandler 30
  ]

requestTimeoutPageHandler :: Server.Response
requestTimeoutPageHandler = Server.status Server.status408 (Server.html (page requestTimeout))

chatPageHandler :: Server.ResponderM ()
chatPageHandler = Server.send (Server.html (page chat))

homePageHandler :: Server.ResponderM ()
homePageHandler = Server.send (Server.html (page home))

notFound404PageHandler :: Server.ResponderM ()
notFound404PageHandler = Server.send (Server.html (page notFound404))

getCurrentLocalTime :: IO Text
getCurrentLocalTime = do
  utcTime  <- Time.getCurrentTime
  timeZone <- Time.getCurrentTimeZone
  pure (formatLocalTime (Time.utcToLocalTime timeZone utcTime))

formatLocalTime :: Time.LocalTime -> Text
formatLocalTime = toText . Time.formatTime Time.defaultTimeLocale "%X"

application :: TVar Clients -> Server.Application
application clientsTVar =
  Socket.websocketsOr Socket.defaultConnectionOptions (socketServer clientsTVar) restServer

addConnection :: TVar Clients -> Socket.Connection -> IO UUID
addConnection clientsTVar connection = do
  uuid    <- UUID.nextRandom
  clients <- readTVarIO clientsTVar
  atomically (writeTVar clientsTVar (Map.insert uuid connection clients))
  pure uuid

removeConnection :: TVar Clients -> UUID -> IO ()
removeConnection clientsTVar uuid = do
  clients <- readTVarIO clientsTVar
  atomically (writeTVar clientsTVar (Map.delete uuid clients))

forkConnection :: Socket.Connection -> IO () -> IO ()
forkConnection connection = Socket.withPingThread connection 30 (pure ())

socketServer :: TVar Clients -> Socket.ServerApp
socketServer clientsTVar pendingConnection = do
  connection <- Socket.acceptRequest pendingConnection
  forkConnection
    connection
    (   addConnection clientsTVar connection
    >>= \uuid -> talk clientsTVar connection `onException` removeConnection clientsTVar uuid
    )

talk :: TVar Clients -> Socket.Connection -> IO ()
talk clientsTVar connection = forever do
  localTime    <- getCurrentLocalTime
  maybeRequest <- Socket.receiveData connection <&> JSON.decode @WebSocketRequest
  if isNothing maybeRequest
    then connection `Socket.sendTextData` JSON.encode
      (WebSocketResponse localTime "system" "Something went wrong 😢")
    else do
      let request@WebSocketRequest {..} = Unsafe.fromJust maybeRequest
      broadcast clientsTVar localTime name message

broadcast :: TVar Clients -> Text -> Text -> Text -> IO ()
broadcast clientsTVar time name message = do
  clients <- readTVarIO clientsTVar
  for_ clients (`Socket.sendTextData` JSON.encode (WebSocketResponse time name message))

restServer :: Server.Application
restServer = foldr ($)
                   (Server.notFound notFound404PageHandler)
                   [Server.get "/" homePageHandler, Server.get "/chat" chatPageHandler]

data WebSocketResponse = WebSocketResponse
  { time    :: Text
  , name    :: Text
  , message :: Text
  }
  deriving (Generic, Show, JSON.ToJSON)

data WebSocketRequest = WebSocketRequest
  { name    :: Text
  , message :: Text
  }
  deriving (Generic, Show, JSON.FromJSON)

