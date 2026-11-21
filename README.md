# effectful-websockets-tls

Interpreters for the WebSocket effect of
[effectful-websockets](https://github.com/GuillaumedeVolpiano/effectful-websockets)
that open secure WebSockets, through
[streamly-websockets-tls](https://github.com/GuillaumedeVolpiano/streamly-websockets-tls).
A package apart, so that programs that only speak in clear need not build
a TLS stack.

```haskell
import Effectful
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.WebSocket
import Effectful.WebSocket.Client
import Effectful.WebSocket.Client.TLS

hello :: IO (Either WebSocketError (Maybe Message))
hello = runEff . runErrorNoCallStack . runWebSocketSecure SystemCertificates (clientConfig "example.org" 443 "/chat") $ do
  _ <- sendText "hello"
  receiveMessage
```

## What it gives

- `runWebSocketSecure`: the connection over TLS, the server checked
  against the certificates of the system (`SystemCertificates`) or of a
  file in PEM (`CertificatesFrom`).
- `runWebSocketAt`: in clear or over TLS, as a scheme says.
- `urlClientConfig`: the scheme and the configuration of a `ws` or `wss`
  URL, to give to `runWebSocketAt`.

They behave as `runWebSocketClient` does in clear: the action runs in the
calling thread, the server is pinged, and a connection that the server
closes, or that breaks, is not an error. A certificate that is not accepted
is a `ConnectFailed`.

## Building

The libraries this package is built on are not on Hackage yet:
`cabal.project` fetches them from GitHub, at their releases. To build
against copies checked out next to this repository instead, pass
`--project-file=cabal.project.siblings`.

Tested with GHC 9.12.4 and effectful 2.7.1.
