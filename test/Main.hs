-- | The interpreters against servers of streamly-websockets on the
-- loopback interface, over TLS and in clear.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Exception (bracket)
import Control.Monad (forever, replicateM)
import Data.ByteString qualified as BS (replicate)
import Data.ByteString.Char8 (pack)
import Effectful (Eff, IOE, MonadIO (liftIO), runEff)
import Effectful.Error.Static (Error, runErrorNoCallStack)
import Effectful.WebSocket (Message (BinaryMessage, TextMessage), messages,
                            receiveMessage, sendBinary, sendText)
import Effectful.WebSocket.Client (ClientConfig (host, path, pingPong, port),
                                   PingPong (PingPong, interval, timeout),
                                   WebSocketError (..), clientConfig)
import Effectful.WebSocket.Client.TLS (Certificates (..), Scheme (Http, Https),
                                       runWebSocketAt, runWebSocketSecure,
                                       urlClientConfig)
import Network.Socket (socketPort)
import Network.Socket qualified as N (close)
import Streamly.Data.Fold (drain, drainMapM, toList)
import Streamly.Data.Stream (fold)
import Streamly.External.Network.WebSocket.Server (Connection,
                                                   defaultServerOptions, listen,
                                                   send, serveSocket)
import Streamly.External.Network.WebSocket.Server qualified as WSS (Message (TextMessage),
                                                                    close,
                                                                    messages)
import Streamly.External.Network.WebSocket.TLS (loadCredentials,
                                                serveSecureSocket)
import Test.Tasty (TestTree, defaultMain, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

main :: IO ()
main = defaultMain tests

certificateFile, keyFile :: FilePath
certificateFile = "test/fixtures/localhost-cert.pem"
keyFile = "test/fixtures/localhost-key.pem"

-- | The certificate of the fixtures, trusted for itself.
local :: Certificates
local = CertificatesFrom certificateFile

run :: Eff '[Error WebSocketError, IOE] a -> IO (Either WebSocketError a)
run = runEff . runErrorNoCallStack

-- | Serve over TLS with a handler on a free loopback port, and give the
-- configuration that reaches the server by the name of its certificate.
withSecureServer :: (Connection -> IO ()) -> (ClientConfig -> IO a) -> IO a
withSecureServer handler use = do
  credentials <- loadCredentials certificateFile keyFile
  bracket (listen "127.0.0.1" 0) N.close $ \listener -> do
    port <- socketPort listener
    withAsync (serveSecureSocket credentials listener defaultServerOptions (const handler)) $ \_ ->
      use (clientConfig "localhost" (fromIntegral port) "/")

-- | The same, in clear.
withClearServer :: (Connection -> IO ()) -> (ClientConfig -> IO a) -> IO a
withClearServer handler use =
  bracket (listen "127.0.0.1" 0) N.close $ \listener -> do
    port <- socketPort listener
    withAsync (serveSocket listener defaultServerOptions (const handler)) $ \_ ->
      use (clientConfig "127.0.0.1" (fromIntegral port) "/")

echo :: Connection -> IO ()
echo connection = fold (drainMapM (send connection)) (WSS.messages connection)

tests :: TestTree
tests =
  localOption (mkTimeout 20_000_000) $
    testGroup
      "effectful-websockets-tls"
      [ testCase "echoes text and binary messages over TLS" $ do
          let large = BS.replicate 300_000 42
          outcome <- withSecureServer echo $ \config -> run . runWebSocketSecure local config $ do
            _ <- sendText "héllo"
            _ <- sendBinary large
            replicateM 2 receiveMessage
          outcome @?= Right [Just (TextMessage "héllo"), Just (BinaryMessage large)]
      , testCase "streams messages until the server closes, then sending fails" $ do
          let server connection = mapM_ (send connection . WSS.TextMessage) ["a", "b"] >> WSS.close connection >> fold drain (WSS.messages connection)
          outcome <- withSecureServer server $ \config -> run . runWebSocketSecure local config $ do
            received <- fold toList messages
            liftIO (threadDelay 100_000)
            late <- sendText "late"
            pure (received, late)
          outcome @?= Right ([TextMessage "a", TextMessage "b"], False)
      , testCase "a certificate the system does not know is a failure to connect" $ do
          outcome <- withSecureServer echo $ \config -> run (runWebSocketSecure SystemCertificates config (pure ()))
          case outcome of
            Left (ConnectFailed _) -> pure ()
            other -> assertFailure ("expected ConnectFailed, got " <> show other)
      , testCase "a server in clear is a failure to connect" $ do
          outcome <- withClearServer echo $ \config -> run (runWebSocketSecure local config {host = "localhost"} (pure ()))
          case outcome of
            Left (ConnectFailed _) -> pure ()
            other -> assertFailure ("expected ConnectFailed, got " <> show other)
      , testCase "reports a server that stops answering pings" $ do
          -- This server never reads, so it never answers a ping.
          outcome <- withSecureServer (\_ -> forever (threadDelay 1_000_000)) $ \config ->
            run (runWebSocketSecure local config {pingPong = Just PingPong {interval = 1, timeout = 2}} receiveMessage)
          outcome @?= Left NoPong
      , testCase "the scheme chooses between clear and TLS" $ do
          inClear <- withClearServer echo $ \config -> run (runWebSocketAt local Http config (sendText "clear" >> receiveMessage))
          overTls <- withSecureServer echo $ \config -> run (runWebSocketAt local Https config (sendText "secure" >> receiveMessage))
          (inClear, overTls) @?= (Right (Just (TextMessage "clear")), Right (Just (TextMessage "secure")))
      , testCase "a URL gives the scheme and the configuration" $ do
          let summary = fmap (\(scheme, config) -> (scheme, config.host, config.port, config.path)) . urlClientConfig
          summary "wss://example.org/chat?room=1" @?= Right (Https, "example.org", 443, "/chat?room=1")
          summary "ws://127.0.0.1:9330/api" @?= Right (Http, "127.0.0.1", 9330, "/api")
          summary "https://example.org/" @?= Left "not a ws or wss URL"
      , testCase "a URL reaches the server it names" $
          withSecureServer echo $ \config -> do
            let url = "wss://localhost:" <> pack (show config.port) <> "/"
            outcome <- either (fail . show) (\(scheme, found) -> run (runWebSocketAt local scheme found (sendText "by URL" >> receiveMessage))) (urlClientConfig url)
            outcome @?= Right (Just (TextMessage "by URL"))
      ]
