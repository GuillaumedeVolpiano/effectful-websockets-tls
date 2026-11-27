# Revision history for effectful-websockets-tls

## 0.1.0.3 -- 2026-11-27

* No change of interface. The dependency bounds are stated once in the
  cabal file, and the imports list the names they use.

## 0.1.0.2 -- 2026-11-27

* Builds with GHC 9.8 and 9.10 as well as 9.12: the language is GHC2021
  with the extensions GHC2024 adds, and the bounds on `base` and on the
  libraries that come with the compiler are wider.

## 0.1.0.1 -- 2026-11-21

* Built on streamly-http 0.3 and streamly-websockets 0.3.

## 0.1.0.0 -- 2026-11-21

* `Effectful.WebSocket.Client.TLS`: `runWebSocketSecure`, `runWebSocketAt`
  and `urlClientConfig`.
