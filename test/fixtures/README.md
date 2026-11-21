# Test fixtures

`localhost-cert.pem` and `localhost-key.pem` are a self-signed certificate
for `localhost` and `127.0.0.1` and its private key, made for the tests
only. The key is public: never trust this certificate outside the tests.

    openssl req -x509 -newkey ec -pkeyopt ec_paramgen_curve:prime256v1 -nodes \
      -keyout localhost-key.pem -out localhost-cert.pem -days 36500 \
      -subj "/CN=localhost" -addext "subjectAltName=DNS:localhost,IP:127.0.0.1"
