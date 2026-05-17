# `M2/distributions/` — packaging machinery

This directory contains everything needed to turn a built `M2` into an
installable package for end users — `.deb` for Debian/Ubuntu, `.rpm` for
Red Hat / Fedora, `.dmg` for macOS, plus a portable `.tar` distribution and
a templated `INSTALL` file.

## Subdirectories

| Directory | Target format |
|---|---|
| `top/` | Templates for the user-facing `INSTALL` and top-level distribution files |
| `tar/` | Portable tarball distribution |
| `dmg/` | macOS disk-image distribution |
| `freebsd/` | FreeBSD port packaging |
| `install/` | Generic install-time helpers |

## Other files

| File | Role |
|---|---|
| `Makefile.in` | Drives all packaging from autotools |
| `tar-exclusions` | List of paths to omit from the tarball |

`.deb` and `.rpm` packaging proper is driven from
[`M2/BUILD/`](../BUILD/) (e.g. `M2/BUILD/rpm/`) plus CPack config in
[`M2/cmake/packaging.cmake`](../cmake/README.md).

## End-user `INSTALL`

The `INSTALL` shown to end users is **templated** from
[`top/INSTALL.in`](top/INSTALL.in) — edit the template, not the generated file.

[← back to repository TOC](../../README.md#under-m2)
