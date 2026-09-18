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

For the CMake build, `MEMTAILOR_PROVIDER` controls memtailor selection:
`AUTO` (the default) uses an installed memtailor 1.4 or newer package
when compatible, otherwise the bundled source; `SYSTEM` requires the installed
package; `BUNDLED` always builds the bundled source. Configuration reports the
selected provider and, for a fallback, the reason. CMake first looks for a config
package exporting `memtailor::memtailor`, then tries `pkg-config` for `memtailor`.
A configure-time compile/link check uses the selected target's flags to determine
and report `MEMT_DEBUG=ON` or `OFF`. Those flags are also propagated to consumers.
Both metadata formats must describe the installed library's ABI correctly;
installations with only headers and a library are not selected automatically.

For example, from `M2/BUILD/build`, use:

```sh
cmake -GNinja -DMEMTAILOR_PROVIDER=SYSTEM -DCMAKE_PREFIX_PATH=/path/to/memtailor ../..
cmake --build . --target memtailor-unit-tests
ctest --test-dir . --output-on-failure -R 'unit-tests:(Arena|BufferPool|MemoryBlocks)\.'
```

`memtailor_DIR` can instead name the directory containing `memtailorConfig.cmake`.
For a pkg-config installation outside the usual search paths, set
`PKG_CONFIG_PATH=/path/to/memtailor/lib/pkgconfig` (or `lib64/pkgconfig`).
Debug M2 builds require a memtailor installation built with `MEMT_DEBUG`, because
bundled mathic and mathicgb use its debug class layout. `AUTO` falls back to the
bundled library on a mismatch; `SYSTEM` reports an error. A multi-configuration
build offering Debug has the same requirement. The `memtailor-unit-tests` target
remains available with either provider when `BUILD_TESTING` is enabled.
