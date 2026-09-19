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

For CMake, `MEMTAILOR_PROVIDER`, `MATHIC_PROVIDER` and `MATHICGB_PROVIDER`
each accept `AUTO` (the default), `SYSTEM`, or `BUNDLED`. `AUTO` prefers a
compatible installed package and otherwise builds the bundled source. `SYSTEM`
requires a compatible installation and fails configuration if it cannot be used.
`BUNDLED` always builds that library from the pinned source. Minimum versions are
memtailor 1.4, mathic 1.5 and mathicgb 1.4.

CMake config packages are preferred (`memtailor::memtailor`, `mathic::mathic`,
`mathicgb::mathicgb`), with pkg-config as a fallback. Metadata must describe the
installed library's debug ABI, transitive dependencies and, for mathicgb, TBB
support. Header/library-only installations are insufficient. Configuration
compiles and links probes and reports `MEMT_DEBUG`, `MATHIC_DEBUG` and
`MATHICGB_DEBUG`, the chosen providers, and reasons for fallbacks. Mathicgb's
`MATHICGB_NO_TBB` must agree with M2's `WITH_TBB` setting.

System mathic requires system memtailor; system mathicgb requires both system
mathic and memtailor. This prevents mixing an installed library with a second
bundled copy of its dependencies. `AUTO` propagates necessary fallbacks;
conflicting explicit `SYSTEM` requests fail with an explanation.

A Debug M2 can use a complete set of Release system libraries: consumers inherit
the installed libraries' actual ABI flags. However, the pinned bundled Debug
mathic/mathicgb sources require their dependencies' Debug layouts. If a bundled
Debug consumer would use an incompatible system dependency, `AUTO` falls back
to bundled libraries and `SYSTEM` fails. The same constraint applies to a
multi-configuration build offering Debug.

For example, from `M2/BUILD/build`:

```sh
cmake -GNinja -DMEMTAILOR_PROVIDER=SYSTEM -DMATHIC_PROVIDER=SYSTEM \
  -DMATHICGB_PROVIDER=SYSTEM ../..
cmake --build . --target memtailor-unit-tests mathic-unit-tests mathicgb-unit-tests
ctest --test-dir . --output-on-failure -R '^unit-tests:'
```

Those test targets remain available with either provider when `BUILD_TESTING`
is enabled. With installed libraries, the pinned library tests use the installed
headers and libraries. M2's core build and checks provide additional integration
coverage. Only bundled libraries are included in M2's library installation/export
rules; external libraries remain managed by their package manager.

`CMAKE_PREFIX_PATH` is a shared list of installation prefixes for all dependencies.
Standard system prefixes usually need no extra setting. For nonstandard prefixes,
use a quoted **semicolon-separated** CMake list, for example
`'-DCMAKE_PREFIX_PATH=/opt/math-libraries;/opt/another-dependency'`. The Unix
**environment variable** instead uses colons:

```sh
export CMAKE_PREFIX_PATH="/opt/math-libraries${CMAKE_PREFIX_PATH:+:$CMAKE_PREFIX_PATH}"
```

Alternatively, `memtailor_DIR`, `mathic_DIR` and `mathicgb_DIR` can point directly
to the corresponding config directories. For nonstandard pkg-config locations,
set `PKG_CONFIG_PATH` to their `lib/pkgconfig` or `lib64/pkgconfig` directories
(colon-separated on Unix).

Provider regression tests require CMake, a C++ compiler, Python and pkg-config:

```sh
python3 M2/cmake/tests/test_mathic_providers.py
```
