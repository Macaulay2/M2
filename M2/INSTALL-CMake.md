Building Macaulay2 with CMake
=============================

## Warning! 
This is not yet the official build system of Macaulay2. See [INSTALL.md](INSTALL.md).

## Why CMake?
See this article on [why the KDE project switched to CMake](https://lwn.net/Articles/188693/) and
[this page](https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Really-Cool-CMake-Features).

## How to use CMake?

First, confirm that you have CMake installed with version at least 3.14:
```
cmake --version
```
You can always [download CMake](https://cmake.org/download/).

### From a terminal:
```
git clone https://github.com/mahrud/M2.git M2
cd M2/M2/BUILD/build
git checkout eigen
cmake ../..
make build-libraries build-programs
cmake .
make
make check-packages
```

### From Xcode or Visual Studio
IDEs such as Xcode or vscode can also configure and build using CMake.
Simply open the cloned repository using your IDE and select target to build.

## Advanced targets and options
After running `cmake ../..`, you can use `cmake -LA` to see a list of computed variables and options.
To change any, run `cmake -DVARIABLE=VALUE` (don't forget the `-D` prefix). Note that these changes are
sticky, meaning that to unset variables you need to run `cmake -UVARIABLE`.

The following are some general targets:
- `all (default)`
- `clean`
- `depend`
- `install`
  - `install/local`
  - `install/strip`
- `edit_cache`
- `list_install_components`
- `rebuild_cache`

Targets for building and installing libraries and programs
- `build-libraries`
- `build-programs`
- `build-[LIBRARY or PROGRAM]`
- `build-[LIBRARY or PROGRAM]-install`
- `googletest`
- `extract-gftables`

Targets for building various pieces of Macaulay2
- `scc1`
- `regex`
- `system`
- `kernel`
- `e-includes`
- `generate-d`
- `interpreter`
- `M2-engine`
- `M2-binary`
- `Core`
- `copy-packages`
- `emacs`

Targets for installing and checking Macaulay2 packages
- `install-packages`
- `check-packages`
- `uninstall-packages`
- `all-[PACKAGE]`
- `install-[PACKAGE]`
- `check-[PACKAGE]`
- `uninstall-[PACKAGE]`

For instace, you can run `make check-LocalRings` to run the tests from the `LocalRings` package.

## Libraries

Macaulay2 uses several external libraries and programs.

### Intel(R) Math Kernel Library

[MKL](https://software.intel.com/en-us/mkl) is a linear algebra routines library specifically optimized for
Intel(R) processors. To enable linking with MKL, adjust the path and architecture appropriately and run the
following before calling `cmake`:
```
source /opt/intel/bin/compilervars.sh intel64
```
Note that MKL is closed-source but released as a freeware.

### AMD(R) Optimizing CPU Libraries

[AOCL](https://developer.amd.com/amd-aocl/) includes AMD BLIS and AMD libFLAME
