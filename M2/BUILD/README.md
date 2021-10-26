See [INSTALL](../INSTALL) or [INSTALL-CMake.md](../INSTALL-CMake.md) for build
requirements and instructions.

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

Alternatively, see the [this guide](docker/README.md) for instructions on using
Docker to build Linux container images running Macaulay2.
