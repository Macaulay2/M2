# `top/INSTALL.in`, `top/postinstall.in`, `top/preremove.in`, `Makefile.in`, per-platform subdirs — packaging

The `distributions/` directory is **the packaging machinery**.
This doc consolidates per-file coverage of the top-level
Makefile, the `top/` user-facing templates, and the four
per-format subdirs (`dmg/`, `freebsd/`, `install/`, `tar/`).

Part of [`distributions/`](README.md).

[← distributions/ overview](README.md) · [← top-level repo TOC](../../README.md)

## `Makefile.in` — packaging driver

The top-level Makefile orchestrates every packaging target. After
configure substitutes paths/versions, this Makefile knows how to:

- **`make dist`** — produce the `.tar.gz` source tarball.
- **`make dmg`** — produce a macOS disk image (via the `dmg/`
  subdir).
- **`make deb`** / **`make rpm`** — produce Debian / Red Hat
  packages (via templated control files).
- **`make freebsd`** — FreeBSD port packaging.

Each target dispatches into the relevant subdir.

## `top/` — user-facing install templates

| File | Substituted into | Role |
|---|---|---|
| `INSTALL.in` | `INSTALL` (top of source tarball) | End-user install instructions |
| `postinstall.in` | postinstall script | Run after `.deb`/`.rpm` install |
| `preremove.in` | preremove script | Run before `.deb`/`.rpm` removal |
| `Makefile.in` | local Makefile | Wires the templates into the build |

The `INSTALL.in` template is what users see when they download a
source tarball — the standard "configure / make / make install"
guide, plus M2-specific notes (system dependencies, recommended
build flags).

The postinstall script handles:

- Running `install-info` to register M2 with the system info
  database.
- Updating shared library caches (`ldconfig`).
- Creating any required runtime directories.

The preremove script undoes them.

## `dmg/` — macOS disk image

```
dmg/
├── Makefile.in
├── README.md
├── ReadMe-MacOSX-fink.txt
├── ReadMe-MacOSX.rtf
├── ReadMe-MacOSX.txt
```

The macOS-specific bits:

- **`ReadMe-MacOSX.{txt,rtf}`** — text shown in Finder when the
  user mounts the `.dmg`.
- **`Makefile.in`** — runs `hdiutil` to assemble the disk image
  from the built M2 tree.
- **`ReadMe-MacOSX-fink.txt`** — historical Fink-based install
  note (Fink has waned in popularity but the file is kept for
  compatibility).

The DMG layout:

```
M2.dmg/
├── Macaulay2/         (the actual app folder)
└── ReadMe-MacOSX.rtf  (auto-opened by Finder)
```

## `freebsd/` — FreeBSD port

```
freebsd/
├── Makefile.in
├── description
├── post-deinstall.in
├── post-install.in
```

A FreeBSD port skeleton. `description` is the one-line summary
the FreeBSD package browser shows; `post-install` /
`post-deinstall` are FreeBSD's equivalent of the Linux
postinstall/preremove scripts.

## `install/` — generic install helpers

```
install/
├── Makefile.in
└── README.md
```

Minimal scaffolding for things that apply across packaging
formats — generic shell helpers, common environment-variable
setup. Mostly thin glue, used by other subdir Makefiles.

## `tar/` — plain tarball

```
tar/
├── Makefile.in
└── README.md
```

Generates a **plain `.tar.gz`** distribution — no packaging
metadata, just the built tree. For users who want to install M2
by extracting into `/opt/M2/` and adding to `PATH`.

## `tar-exclusions` — paths to omit from tarballs

A text file listing patterns to exclude from the source tarball:
build directories, `.git/`, editor swap files, etc. Consumed by
`tar --exclude-from=tar-exclusions`.

## Why all the formats

Different audiences:

- **`.dmg`** — macOS users who want one-click install.
- **`.deb`/`.rpm`** — Linux distros (Debian/Ubuntu, Fedora/RHEL).
- **FreeBSD port** — BSD users.
- **`.tar.gz`** — for distros M2 doesn't directly target
  (Arch, NixOS, Gentoo) where downstream maintainers handle
  packaging.
- **Source `.tar.gz`** — for people building from source.

## Used by

- `make dist` / `make package` from the autotools build root.
- Release managers cutting binary releases.
- CI's release-artifact jobs.

## Related

- [`README.md`](README.md) — distributions/ overview.
- [`../cmake/file-misc-cmakes.md`](../cmake/file-misc-cmakes.md)
  — CPack-based packaging on the CMake side.
- [`../files/file-files-content.md`](../files/file-files-content.md)
  — info-dir template that the post-install script registers.
