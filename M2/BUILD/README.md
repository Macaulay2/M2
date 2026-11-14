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

### Optional Maple diagnostics (CMake)

With `WITH_MAPLE=ON` (the default), CMake checks whether command-line Maple can
run an arithmetic calculation and whether its `convex` library loads. An absent
or unusable Maple does not fail configuration. Use `-DWITH_MAPLE=OFF` to skip
probing. Cross-compilation also skips execution.

Set `MAPLE_EXECUTABLE` to select a command-line Maple outside `PATH`, and
`MAPLE_CONVEX_DIR` if convex needs an additional Maple library directory, e.g.:

```sh
cmake -S M2 -B build -DMAPLE_EXECUTABLE=/opt/maple/bin/maple \
  -DMAPLE_CONVEX_DIR=/opt/convex/lib
```

`MAPLE_PROBE_TIMEOUT` defaults to 10 seconds per probe. Results are
`Maple_FOUND` and `Maple_Convex_FOUND`; execution logs are under
`CMakeFiles/Maple` in the build directory. Each configuration reruns the probes;
rerunning package tests alone does not recheck availability.

These diagnostics do not change package installation, example-cache policy or
test selection. Runtime configuration remains separate: use `MapleCommand` in
MapleInterface and `ConvexPath` in ConvexInterface for custom runtime paths.
