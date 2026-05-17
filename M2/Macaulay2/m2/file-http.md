# `http.m2` — `getWWW` (basic HTTP client)

`http.m2` provides M2's **basic HTTP client** — `getWWW`. It
implements a simple GET request without external dependencies
(no curl, no libcurl), suitable for fetching HTTP resources from M2
scripts.

Part of the [`m2/` Core layer](README.md).

[← back to m2 overview](README.md) · [← top-level engine TOC](../../../README.md)

## What's declared

```m2
--		Copyright 1996 by Daniel R. Grayson

needs "methods.m2"

getWWW = method()
httpProduct := concatenate("Macaulay2/", version#"VERSION")

crlf := "\r\n";

GET := (host, url, connection) -> (
    ...
)
```

The `httpProduct` string is what M2 sends in the `User-Agent`
header. The `GET` local function constructs and writes the raw
request bytes to a `connection` socket.

## What `getWWW` does

```m2
getWWW "http://www.example.com/"
```

Returns the response body as a string. The implementation:

1. Parses the URL to extract host, port, path.
2. Opens a socket to the host on port 80 (or 443 with TLS — if
   linked with OpenSSL).
3. Sends an HTTP/1.0 GET request.
4. Reads the response.
5. Strips headers, returns the body.

The implementation is **basic**: HTTP/1.0 only, no follow-redirects,
no chunked encoding, no compression. For anything beyond simple
fetches users should use `run "curl …"` instead.

## When it's used

`getWWW` is primarily used by:

- M2's **update-check** mechanism (querying the homepage for the
  latest version).
- Some test fixtures that fetch reference data.
- Tutorial examples illustrating M2's network capabilities.

It's not the primary path for "fetch external data" — that's `run
"curl …"` or one of the user-package HTTP clients.

## Used by

- M2's version-check on startup.
- A few tutorial scripts.

## Related

- [`README.md`](README.md) — m2/ overview.
- [`file-system.md`](file-system.md) — `run` for shelling out to
  curl.
- `webapp.m2` ([`file-webapp.md`](file-webapp.md)) — for serving
  HTTP from M2.
