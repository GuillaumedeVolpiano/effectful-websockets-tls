-- |
-- Module      : Effectful.WebSocket.Client.TLS
-- Description : Opening a secure WebSocket to a server
-- License     : BSD-3-Clause
--
-- Interpreters for the effect of "Effectful.WebSocket" that open the
-- connection over TLS, or as a URL says.
--
-- > import Effectful
-- > import Effectful.Error.Static (runErrorNoCallStack)
-- > import Effectful.WebSocket
-- > import Effectful.WebSocket.Client
-- > import Effectful.WebSocket.Client.TLS
-- >
-- > hello :: IO (Either WebSocketError (Maybe Message))
-- > hello = runEff . runErrorNoCallStack . runWebSocketSecure SystemCertificates (clientConfig "example.org" 443 "/chat") $ do
-- >   _ <- sendText "hello"
-- >   receiveMessage
--
-- They behave as 'Effectful.WebSocket.Client.runWebSocketClient' does:
-- the action runs in the calling thread, the server is pinged, and a
-- connection that the server closes, or that breaks, is not an error.
module Effectful.WebSocket.Client.TLS
  ( runWebSocketSecure
  , runWebSocketAt
  , urlClientConfig
  , Scheme (..)
  , Certificates (..)
  ) where

import Control.Exception (catch, throwIO, try)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.IORef (newIORef, readIORef, writeIORef)
import Effectful
import Effectful.Error.Static (Error)
import Effectful.WebSocket (WebSocket)
import Effectful.WebSocket.Client (ClientConfig (..), WebSocketError, clientConfig, connectionConfig, runWebSocketClient, runWebSocketOver)
import Network.TLS (TLSException)
import Streamly.Data.Stream qualified as Stream
import Streamly.Data.StreamK qualified as StreamK
import Streamly.External.Network.HTTP (Transport (..))
import Streamly.External.Network.TLS qualified as Tls
import Streamly.External.Network.WebSocket qualified as WS
import Streamly.External.Network.WebSocket.TLS (Certificates (..), Scheme (..), tlsTransport, urlConfig)

-- | Open a WebSocket over TLS and run an action over it, as
-- 'runWebSocketClient' does in clear. The server must present a
-- certificate valid for the host of the configuration and signed by one
-- of the authorities trusted.
--
-- Throws 'Effectful.WebSocket.Client.ConnectFailed' if the connection
-- cannot be opened, among others when the certificate is not accepted,
-- and 'Effectful.WebSocket.Client.NoPong' if the server stops answering
-- pings.
runWebSocketSecure
  :: (IOE :> es, Error WebSocketError :> es)
  => Certificates
  -> ClientConfig
  -> Eff (WebSocket : es) a
  -> Eff es a
runWebSocketSecure trusted config = runWebSocketOver open config.pingPong
  where
    settings = connectionConfig config
    secure = (Tls.config config.host config.port) {Tls.certificates = trusted, Tls.connectTimeout = settings.openTimeout}
    open :: (WS.Connection -> IO x) -> IO x
    open use = do
      -- What the action gave, kept in case the end of the TLS connection
      -- fails after it.
      result <- newIORef Nothing
      attempt <- try . try . Tls.withConnection secure $ \connection ->
        WS.withConnectionOver (guarded (tlsTransport connection)) settings use >>= writeIORef result . Just
      done <- readIORef result
      case (done, attempt) of
        (Just value, _) -> pure value
        (Nothing, Left Tls.ConnectTimedOut) -> throwIO WS.OpenTimedOut
        (Nothing, Right (Left (problem :: TLSException))) -> throwIO (WS.HandshakeFailed (show problem))
        (Nothing, Right (Right ())) -> throwIO (WS.HandshakeFailed "the connection gave nothing")
    -- Once the connection is open, a failure of TLS is the loss of the
    -- connection, as a failure of TCP is in clear.
    guarded transport =
      Transport
        { send = \bytes -> transport.send bytes `catch` lost
        , receive = Stream.unfoldrM (\from -> StreamK.uncons from `catch` lost) (StreamK.fromStream transport.receive)
        }
    lost :: TLSException -> IO b
    lost problem = throwIO (userError (show problem))

-- | Open a WebSocket in clear ('Http') or over TLS ('Https').
runWebSocketAt
  :: (IOE :> es, Error WebSocketError :> es)
  => Certificates
  -> Scheme
  -> ClientConfig
  -> Eff (WebSocket : es) a
  -> Eff es a
runWebSocketAt trusted = \case
  Http -> runWebSocketClient
  Https -> runWebSocketSecure trusted

-- | The scheme and the configuration that a @ws@ or @wss@ URL stands for,
-- to give to 'runWebSocketAt': 'Http' for @ws@, 'Https' for @wss@.
urlClientConfig :: ByteString -> Either String (Scheme, ClientConfig)
urlClientConfig text = do
  (scheme, settings) <- urlConfig text
  pure (scheme, clientConfig settings.host settings.port (Char8.unpack settings.path))
