See the [Macaulay2 wiki](https://github.com/Macaulay2/M2/wiki) for current
build requirements and instructions, especially the "Building M2 from source"
pages for the autotools and CMake workflows.

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

### Optional Maple detection (CMake)

`WITH_MAPLE` defaults to `ON`: CMake enables Maple support if a working
installation is available, but missing Maple is not a configuration error.
Configure with `-DWITH_MAPLE=OFF` to disable detection and set both availability
results to false without launching Maple, even if `MAPLE_EXECUTABLE` is cached.
The configure output reports the interface as enabled, unavailable, or explicitly
disabled, together with convex availability.

To disable probing, or restore the default behavior in an existing build:

```sh
cmake -S M2 -B M2/BUILD/build -DWITH_MAPLE=OFF
cmake -S M2 -B M2/BUILD/build -DWITH_MAPLE=ON
```

Example status lines (only one applies to a given configuration):

```text
-- Maple interface: enabled; convex: TRUE
-- Maple interface: unavailable; convex: FALSE
-- Maple interface: disabled (WITH_MAPLE=OFF); convex: FALSE
```

"Enabled" here describes CMake's availability result. `WITH_MAPLE=OFF` does
not remove the distributed M2 interface packages or prevent a user from loading
them later; runtime configuration and package execution policy are separate.

When `WITH_MAPLE=ON`, CMake searches for command-line `maple` or `cmaple` and executes a small arithmetic
probe. Finding an executable alone does not establish that its license works.
Maple is optional: an absent executable, failed probe or timeout does not prevent
configuration, and CMake never downloads Maple. The optional `convex` Maple
library is checked separately by loading it with `with(convex)`.

For installations outside `PATH`, configure with, for example:

```sh
cmake -S M2 -B M2/BUILD/build \
  -DMAPLE_EXECUTABLE=/opt/maple/bin/maple \
  -DMAPLE_CONVEX_DIR=/opt/convex/lib
```

Omit `MAPLE_CONVEX_DIR` when convex is already on Maple's library search path.
`MAPLE_PROBE_TIMEOUT` defaults to 10 seconds per probe. Results are reported as
`Maple_FOUND` and `Maple_Convex_FOUND`; diagnostic logs are written under
`CMakeFiles/Maple` in the build directory. Probes run again on reconfiguration so
license availability is not permanently cached. When cross-compiling, probes
are not executed and Maple is reported unavailable.

CMake uses these build-host results when constructing package installation
commands. When Maple is unavailable or `WITH_MAPLE=OFF`, it passes
`RerunExamples=false` for `MapleInterface`, `AdjointIdeal` and `Parametrization`;
`ConvexInterface` also uses that setting when convex is unavailable. These
packages ship cached examples requiring the optional software. All other packages
retain the requested `RerunExamples` setting. Package selection and independent
checks are unchanged. Cached examples must still exist and have matching input
hashes: missing or stale caches can require execution and report an error.

These results are not installed-runtime configuration. For a custom runtime executable, also set `MapleInterface`'s
`MapleCommand` configuration; `ConvexInterface` similarly uses `ConvexPath`.
An explicit `installPackage(..., RerunExamples => true)` call outside the CMake
build still requires the external software used by those examples. An external M2 instance does not read CMake's results.

The detection tests use simulated executables and require no Maple license:

```sh
python3 M2/cmake/tests/test_find_maple.py
```
