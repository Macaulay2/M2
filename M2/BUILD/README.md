See the [Macaulay2 wiki](https://github.com/Macaulay2/M2/wiki) for current
build requirements and instructions, especially the "Building M2 from source"
pages for the autotools and CMake workflows.

For a CMake build using external dependencies, leave the submodules uninitialized
or run `git submodule deinit --all` from the repository root, then configure a
fresh build directory with `-DGIT_SUBMODULE=OFF`. CMake skips source build rules
for absent submodules and uses installed libraries instead. Install the Emacs
interface separately from
[M2-emacs](https://github.com/Macaulay2/M2-emacs).

`deinit` removes the submodule working trees while retaining downloaded Git
repositories under `.git/modules`. Without `--force`, it refuses to discard local
changes; commit or save those changes first. Restore the checkouts with
`git submodule update --init --recursive`. `GIT_SUBMODULE=OFF` prevents CMake from
restoring them automatically; it still permits using any sources you leave present.

The libraries normally supplied by submodules must already be installed:
BDWGC, FLINT, Frobby, Givaro, FFLAS-FFPACK, memtailor, mathic, and mathicgb, plus
GoogleTest when `BUILD_TESTING=ON`. The three engine libraries need their installed
CMake or pkg-config metadata; older packages without that metadata need updating.
Use `CMAKE_PREFIX_PATH` and/or `PKG_CONFIG_PATH` for a custom installation prefix.
For example, from the repository root:

```sh
git submodule deinit --all
cmake -S M2 -B build-external -G Ninja -DGIT_SUBMODULE=OFF \
  -DCMAKE_BUILD_TYPE=Release -DCMAKE_PREFIX_PATH=/path/to/dependencies
cmake --build build-external --target build-libraries build-programs
cmake --build build-external --target M2-core
```

If an external library is missing or unusable and its submodule is absent,
configuration fails with a diagnostic instead of registering a build with no
sources. Other downloaded dependencies retain their usual build rules. Use a
fresh build directory when changing dependency providers to avoid reusing cached
paths or libraries staged by a previous build.

The external-library CI jobs currently build the following upstream revisions.
As checked on 2026-10-23, the latest releases do not include the installed CMake
package exports and ABI-aware pkg-config metadata required by this build:

| Library | Latest release | Temporary CI revision | Required changes |
| --- | --- | --- | --- |
| memtailor | 1.4 | `0a2afc3fff096ed06bf98b7c1c224acfc8c97dc1` | [Package metadata](https://github.com/Macaulay2/memtailor/pull/23) |
| mathic | 1.5 | `a08a776c8973a4c25f44d7d769ba949766f3f13c` | [Package metadata](https://github.com/Macaulay2/mathic/pull/29), [dependency discovery compatibility](https://github.com/Macaulay2/mathic/pull/30) |
| mathicgb | 1.4 | `74e3708f52f989ac1bd698412d2ee67d6ef43b6d` | [Package metadata](https://github.com/Macaulay2/mathicgb/pull/84) |

These pins make CI reproducible while testing the unreleased installation
support. They do not restrict which revisions M2 can use: a compatible installed
package is sufficient. Replace the pins with release tags once releases contain
the linked changes. Existing release packages may instead backport those changes.

This directory may be used as a convenient location for the build directory
trees.  For example, it may contain subdirectories `Linux-i686` and
`Linux-x86_64`.  Those directories in turn may contain subdirectories, if
desired, with names such as `ubuntu`, `debian`, and `ubuntu-debug`, or even
`ubuntu/normal` and `ubuntu/debug`, serving as the actual build directories.

A subdirectory `tarfiles` is used as a repository for downloaded source code of
third party libraries, in compressed archive format, shared among the various
build directories. Source code of libraries available as Git repositories are
available in the `submodules` subdirectory of the parent directory.

A subdirectory `CommonStagingArea` is optionally and automatically used as a
place where files can be installed, in such a way that architecture-independent
files get made only once, saving time when versions for multiple architectures
are being built.

The path to the staging area may be specified explicitly by the user using the
`--with-staging-area` option of the `configure` script, or implicity using the
`--enable-common-staging-area` option.

The `make` target `all` will pre-install the files there, and the `make` target
`install` will copy the files from here to the final installation directory, if
there is one.  Alternatively, distribution files in tar, deb, rpm, etc.,
formats can be requested through options to configure, and they will be made by
the target `all`.

Installations will be done here with the SeparateExec option to the
installPackage in Macaulay 2 set to true, so that all the
architecture-independent files are in a subdirectory of this directory called
`common`, and the architecture-dependent files will be in other subdirectories,
such as `x86_64-Linux-Ubuntu-8.10`.  (Actually, this is being changed...)

Alternatively, see the [Docker guide](docker/README.md) for instructions on
using Docker to build Linux container images running Macaulay2.
