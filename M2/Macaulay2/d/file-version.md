# `version.dd` — version / build-info constants

`version.dd` exposes **build-time constants** — M2 version,
which optional libraries were linked in, distributed package
list, etc. — to M2 user code as a `version` HashTable.

Part of the [`d/` interpreter layer](README.md).

[← back to d/ overview](README.md) · [← top-level engine TOC](../../../README.md)

## Header

```d
--		Copyright 2010 by Daniel R. Grayson

use M2;
use hashtables;

declarations "
   #ifdef WITH_MYSQL
     #include <mysql/mysql.h>
   #endif
   #ifdef WITH_PYTHON
     #include <Python.h>
   #endif
   ";

header "
   #include <M2/config.h>
   #include <M2/gc-include.h>
   #include <M2/math-include.h>
   #include \"distributed-packages.h\"

   #define stringize0(a) #a
   #define stringize(a) stringize0(a)

   const char * M2_version () { return PACKAGE_VERSION; }
```

The `M2_version()` function (defined inline in the `header "..."`)
just returns the autotools/CMake `PACKAGE_VERSION` define.

## What `version` exposes

The `version` HashTable populated by this file:

| Key | Source | Value |
|---|---|---|
| `"VERSION"` | `M2/VERSION` | e.g. `"1.26.05"` |
| `"machine"` | `uname` | e.g. `"Linux x86_64"` |
| `"compile time"` | `__DATE__` `__TIME__` | build timestamp |
| `"compiler"` | configure | e.g. `"g++ 13.2"` |
| `"gmp version"` | `__GMP_VERSION` | GMP lib version |
| `"mpfr version"` | `MPFR_VERSION` | MPFR lib version |
| `"factory version"` | factory header | ... |
| `"distributed packages"` | `distributed-packages.h` | list of bundled packages |
| `"git description"` | configure | `git describe` output |
| `"endianness"` | runtime | `"little"` / `"big"` |
| `"pointer size"` | `sizeof(void*)` | 4 or 8 |
| `"build system"` | configure | `"cmake"` / `"autotools"` |

## `distributed-packages.h`

A generated header listing every package in the
`=distributed-packages` file. The build system regenerates it
whenever the package list changes; `version.dd` consumes it to
populate `version#"distributed packages"`.

## Why this matters

The version info is shown:

- On every M2 startup banner.
- In bug reports (`about M2`).
- For `--version` invocation.
- For docs that say "available in M2 ≥ X".

## Used by

- The startup banner.
- `about M2` documentation page.
- Packages that check `version#"VERSION"` against a minimum.

## Related

- [`README.md`](README.md) — d/ overview.
- `M2/VERSION` — single source of truth for the version.
- `Macaulay2/packages/=distributed-packages` — the package list.
- `../m2/startup.m2.in` — uses the version info on startup (see
  [`../m2/README.md`](../m2/README.md) for Core M2 overview).
