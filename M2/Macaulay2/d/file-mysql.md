# `mysql.d` / `mysqldummy.d` — MySQL client bindings (optional)

`mysql.d` provides bindings to the **MySQL C client library**
(`libmysqlclient`), letting M2 connect to MySQL databases. When MySQL
is not available at build time, the dummy file `mysqldummy.d`
provides stubs that error out informatively.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
-- Copyright 2008 by Daniel R. Grayson
-- M2 interface to the mysql C library
-- documentation:
--   http://dev.mysql.com/doc/refman/5.0/en/c.html
--   /usr/share/doc/mysql-doc-5.0/refman-5.0-en.html-chapter/index.html

use strings;
use hashtables;
```

The two URL references are to the MySQL 5.0 C client documentation
(both online and locally-installed Debian package paths). They've
moved since 2008 but the underlying API is unchanged.

## Why this exists

In the late 2000s, MySQL was the dominant relational database. M2
researchers wanted to query databases of computed examples (knot
invariants, polynomial systems, etc.) directly from M2. `mysql.d`
was added to enable that.

Modern usage has shifted toward JSON-over-HTTP and Python interop,
so the MySQL path is less central — but it's still maintained.

## `mysqldummy.d`

When configure detects that MySQL isn't available, the build uses
`mysqldummy.d` instead. The dummy file provides function names that
match `mysql.d`'s exports but with bodies that immediately raise
"MySQL support not compiled in" errors.

This pattern lets M2 code reference `mysqlConnect` etc.
unconditionally; the error only fires if the user actually tries to
use them on a no-MySQL build.

## What's exposed

- **`MysqlConnection`** — wrapped `MYSQL *` handle.
- **`MysqlResult`** — wrapped `MYSQL_RES *`.
- **`MysqlField`** — column metadata.
- **Operations** — `mysqlRealConnect`, `mysqlQuery`,
  `mysqlStoreResult`, `mysqlFetchRow`, etc.

All map directly to the MySQL C client functions.

## Used by

- Packages that query MySQL databases.
- Research workflows storing computed invariants in MySQL.

## Related

- [`README.md`](README.md) — d/ overview.
- `mysqldummy.d` — fallback implementation.
- MySQL C client library — external linked library.
- Sister FFI files: [`file-python.md`](file-python.md),
  [`file-xml.md`](file-xml.md), [`file-ffi.md`](file-ffi.md),
  [`file-json.md`](file-json.md).
