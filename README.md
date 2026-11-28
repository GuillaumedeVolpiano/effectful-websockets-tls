# effectful-websockets-tls

Interpreters for the WebSocket effect of
[effectful-websockets](https://hackage.haskell.org/package/effectful-websockets)
that open secure WebSockets, through
[streamly-websockets-tls](https://hackage.haskell.org/package/streamly-websockets-tls).
A package apart, so that programs that only speak in clear need not build
a TLS stack.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Effectful (runEff)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.WebSocket (Message, receiveMessage, sendText)
import Effectful.WebSocket.Client (WebSocketError, clientConfig)
import Effectful.WebSocket.Client.TLS (Certificates (SystemCertificates),
                                       runWebSocketSecure)

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

To build against copies of the libraries this package is built on
([effectful-websockets](https://hackage.haskell.org/package/effectful-websockets),
[streamly-websockets-tls](https://hackage.haskell.org/package/streamly-websockets-tls)
and those under them) checked out next to this repository, pass
`--project-file=cabal.project.siblings`.

Tested with GHC 9.8.4, 9.10.3 and 9.12.4, and effectful 2.7.1.
