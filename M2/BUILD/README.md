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

CMake initializes only the submodules it needs: the bundled engine libraries
(memtailor, mathic, and mathicgb), the Emacs package, and fallback libraries that
were not found on the system. System FLINT, for example, avoids both its checkout
and its fallback build rules. Previously built fallback libraries keep their
build/test targets across reconfiguration.

To explicitly build a fallback even when the system library is available, select
it before configuring, for example `cmake -DBUILD_LIBRARIES=FLINT ../..`, then run
`cmake --build . --target build-libraries`. Multiple library names use a quoted
semicolon-separated list. Unneeded submodule-backed `build-<library>` targets
are not created until that fallback is selected.

For offline builds, initialize the required submodules first and configure with
`-DGIT_SUBMODULE=OFF`; configuration reports any missing required sources.
This option disables Git updates, not the source requirements. Source archives
must likewise contain the required submodule contents. CI checkouts should avoid
initializing all submodules recursively in advance to benefit from this selection.
