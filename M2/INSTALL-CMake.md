Building Macaulay2 from Source using CMake
==========================================

![Build and Test Macaulay2](https://github.com/Macaulay2/M2/workflows/Build%20and%20Test%20Macaulay2/badge.svg?branch=master)

## Why CMake?
CMake is a cross-platform system for generating build environments using native tools such as Makefiles and Ninja or IDEs such as Xcode and Visual Studio. See this article on [why the KDE project switched to CMake](https://lwn.net/Articles/188693/) and this list of [cool CMake features](https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Really-Cool-CMake-Features).

Also see [this guide](BUILD/docker/README.md) for building and packaging Macaulay2 in a Docker container.

## Getting started
[Download](https://cmake.org/download/) the latest CMake for your platform.
CMake is available through [Homebrew](https://brew.sh/) for both Mac OS X and Linux distributions.
If using a packaged distribution, confirm using `cmake --version` that you have version at least 3.15.
This build system is tested on GCC 6+, Clang 6+, and Xcode 9+ compilers.

**TIP**: install `ccache` for caching compiler artifacts and `ninja-build` (`ninja` on Brew) for optimized parallel builds.

#### Requirements
There are various tools needed to compile Macaulay2 dependencies.
- On Debian/Ubuntu, install `autoconf build-essential bison libtool pkg-config yasm`.
- On Fedora/CentOS, install `autoconf automake bison libtool pkg-config yasm`.
- On Mac OS X, using Homebrew, install `autoconf automake bison libtool pkg-config yasm`.

There are 10 libraries that must be found on the system.
- On Debian/Ubuntu, install `libopenblas-dev libgmp3-dev libxml2-dev libreadline-dev libgdbm-dev libboost-regex-dev libboost-stacktrace-dev libatomic-ops-dev libomp-dev libtbb-dev libffi-dev`.
- On Fedora/CentOS, install `openblas-devel gmp-devel libxml2-devel readline-devel gdbm-devel boost-devel libatomic_ops-devel libomp-devel tbb-devel libffi-devel`.
- On Mac OS X, using Homebrew, install `gmp libxml2 readline gdbm boost libatomic_ops libomp tbb libffi`.

**TIP**: x86_64 binary packages for all dependencies on Mac OS X 10.15+ and Linux distributions are available through the [Macaulay2 tap](https://github.com/Macaulay2/homebrew-tap/) for Homebrew. To download the dependencies this way run:
```
brew tap Macaulay2/tap
brew install --only-dependencies macaulay2/tap/M2
```
and append ``-DCMAKE_PREFIX_PATH=`brew --prefix` `` to an invocation of CMake prior to starting the build so that CMake can find the dependencies installed through Homebrew.

#### Quick build
A quick build involves the following steps:
```
git clone https://github.com/Macaulay2/M2.git
cmake -GNinja -S M2/M2 -B M2/M2/BUILD/build -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/usr
cmake --build M2/M2/BUILD/build --target build-libraries build-programs
cmake --build M2/M2/BUILD/build --target install-packages
cmake --build M2/M2/BUILD/build --target M2-emacs
cmake --install M2/M2/BUILD/build
```
Each step is explained separately in the next section.

**NOTE**: the source directory must not contain any build artifacts from an in-source build. If you have built Macaulay2 in-source before, clean the build artifacts first by running `make clean distclean` in the source directory.

### Building Macaulay2
1. Clone Macaulay2:
```
git clone https://github.com/Macaulay2/M2.git
```

2. Setup the build environment:
```
cd M2/M2/BUILD/build
cmake -GNinja -S../.. -B. \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_INSTALL_PREFIX=/usr
```
The `-S../..` argument indicates the location of the source and `-B.` indicates the build directory. After those, arguments of type `-DNAME=VALUE` set the `NAME` variable to `VALUE`. For instance, `CMAKE_BUILD_TYPE` determines various compiler flags to be used. Defined options are `Release`, `Debug`, `RelWithDebInfo`, and `RelMinSize`, with `RelWithDebInfo` being the default. The value of `CMAKE_INSTALL_PREFIX` determines the installation prefix.

This command generates the `build.ninja` files used by the Ninja build system, which is much more efficient. To generate a `Makefile` instead, remove `-GNinja` and use `make` instead of `ninja` in subsequent commands.

3. Build the libraries that will be linked with the Macaulay2 executable:
```
ninja build-libraries
```
Note that this target **must** be built separately, before proceeding to `M2-binary`.

To enforce building certain libraries, for instance BDWGC and MPIR, run `cmake -DBUILD_LIBRARIES="BDWGC;MPIR"`, or `cmake -DBUILD_LIBRARIES=ALL` to build everything.

*Tip:* if you have already built the libraries in another build directory, use `-DM2_HOST_PREFIX=[usr-host path]` to tell CMake where to look for the libraries and programs. Note that most options can be changed after the initial call to CMake with a subsequent call:
```
cmake -DM2_HOST_PREFIX=[usr-host path] .
```

4. Build the Macaulay2 binary and core scripts:
```
ninja M2-binary M2-core
```
The `M2-binary` is a prerequisite of `M2-core`, which is currently the default target, so just `ninja` also suffices.

5. Build the programs used by some Macaulay2 packages:
```
ninja build-programs
```

Similar to the libraries, `cmake -DBUILD_PROGRAMS=...` can be used to enforce building specific programs.

6. Install packages and generate documentation:
```
ninja install-packages
```

### Testing Macaulay2
There are unit-tests available within the `Macaulay2/e/unit-tests` and `Macaulay2/tests` directories which can be tested using the CTest utility. Here are various ways of using `ctest`:
- `ctest --build-and-test`: build and run all tests.
- `ctest -N`: list all tests.
- `ctest -R unit-tests`: run all tests in `e/unit-tests`.
- `ctest -R normal -j6`: run all tests in `tests/normal`, in 6 parallel jobs.
- `ctest --rerun-failed -V`: rerun the tests that failed in the last batch and echo the results.
- `ctest -R check-LocalRings -j4`: run all tests in the LocalRings package, in 4 parallel jobs.
- `ctest -R check-LocalRings-2 -V`: run the 3rd test in the LocalRings package and echo the result.
- `ctest -T memcheck -R ARingQQFlint`: run matching tests through Valgrind


Note: if the last option does not work, try running `ninja info-packages` and `cmake .` to populate the tests.

### Installing Macaulay2
To install on `/usr`, simply run `ninja install`. To change the installation prefix, first run:
```
cmake -DCMAKE_INSTALL_PREFIX=/usr/local .
```
Note that by default the `install` target depends only on the `all` target. The environment variable `DESTDIR` can be used to install in a staging environment. For example:
```
DESTDIR=/home/mahrud/staging ninja install
```

### Packaging Macaulay2
CMake also supports creating rpm and deb packages as well as archives and dmg images using the `cpack` utility:
```
cpack -G DEB	# requires dpkg
cpack -G RPM	# requires rpmbuild
```

**TIP**: see [this guide](BUILD/docker/README.md) for packaging Macaulay2 for other Linux distributions using Docker.

**NOTE**: Macaulay2 is packaged for Mac OS X as a Homebrew bottle available through the [Macaulay2 tap](https://github.com/Macaulay2/homebrew-tap).


## Advanced cached flags
Within the build environment, you can:
- use `cmake -L .` to see a list of computed flags and options;
- use `cmake -DVARIABLE=VALUE .` to change a cached variable; and
- use `cmake -UVARIABLE` to unset the variable. Wildcard unsetting is allowed; e.g. `cmake -U*VAR* .`

For a complete list, along with descriptions, try `cmake -LAH .` or see `cmake/configure.cmake`. Here are the most useful flags, where the format is `[FLAG]:[TYPE]=[DEFAULT VALUE]`, though specifying the type is optional.

### Build flags
- `DEVELOPMENT:BOOL=OFF`: set the DEVELOPMENT macro in config.h
- `EXPERIMENT:BOOL=OFF`: set the EXPERIMENT macro in config.h
- `GIT_SUBMODULE:BOOL=ON`: update submodules during build
- `LINTING:BOOL=OFF`: enable linting C++ sources (see `cmake/prechecks.cmake`)
- `MEMDEBUG:BOOL=OFF`: enable memory allocation debugging
- `PROFILING:BOOL=OFF`: enable profiling build flags
- `USING_MPIR:BOOL=OFF`: use MPIR instead of GMP
- `WITH_OMP:BOOL=ON`: link with the OpenMP library
- `WITH_TBB:BOOL=ON`: link with the TBB library
- `WITH_FFI:BOOL=ON`: link with the FFI library
- `WITH_PYTHON:BOOL=OFF`: link with the Python library (set to `ON` to use the `Python` package)
- `WITH_SQL:BOOL=OFF`: link with the MySQL library
- `WITH_XML:BOOL=ON`: link with the libxml2 library
- `BUILD_DOCS:BOOL=OFF`: build internal documentation
- `BUILD_NATIVE:BOOL=ON`: use native SIMD instructions
  - `AUTOTUNE:BOOL=OFF`: autotune library parameters (NTL and FFLAS-FFPACK)
- `BUILD_SHARED_LIBS:BOOL=OFF`: build shared libraries
- `BUILD_TESTING:BOOL=ON`: build the testing targets
  - `SKIP_TESTS:STRING="mpsolve;googletest"`: tests to skip
  - `SLOW_TESTS:STRING="eigen;ntl;flint"`: slow tests to skip
- `BUILD_LIBRARIES:STRING=""`: build libraries, even if found on the system
- `BUILD_PROGRAMS:STRING=""`: build programs, even if found on the system
- `CMAKE_BUILD_TYPE:STRING=RelWithDebInfo`: valid CMake build types are `Debug` `Release` `RelWithDebInfo` `MinSizeRel`
- `CMAKE_INSTALL_PREFIX:PATH=/usr`: installation prefix
- `M2_HOST_PREFIX:PATH=${CMAKE_BINARY_DIR}/usr-host`: host build prefix
- `M2_DIST_PREFIX:PATH=${CMAKE_BINARY_DIR}/usr-dist`: target build prefix
- `PARALLEL_JOBS:STRING=4`: specify the number of parallel jobs for libraries and programs

### Macaulay2 flags
The following flags can be also set by giving environment variables to CMake (e.g. `errorDepth=2 cmake .`), though setting them this way is ephemeral.
- `CheckDocumentation:STRING=true`: check documentation for completeness
- `IgnoreExampleErrors:STRING=true`: ignore errors in example code
- `RemakeAllDocumentation:STRING=false`: remake all documentation
- `RerunExamples:STRING=false`: rerun example outpuat files
- `UserMode:STRING=[integer]`: bitmask for ctest check arguments
- `debugLevel:STRING=0`: set the debugging level
- `errorDepth:STRING=3`: set the error printing depth
- `gbTrace:STRING=0`: set the Groebner basis trace level
- `GC_MAXIMUM_HEAP_SIZE:STRING=400M`: maximum collected heap size for tests
- `PACKAGES:INTERNAL=[list of packages]`: a space-separated list of packages


## Advanced targets and options
Within the build environment, use `make help` (or `ninja help`) to get a list of valid targets.

The following are the general targets:
- `all (default)`
- `install`: install the build artifacts from the `all` target
  - `install/local`: only install artifacts from targets in the current directory
  - `install/strip`: install stripped binary and library artifacts
- `edit_cache`: edit configure options and cache variables
- `rebuild_cache`: rebuild the cache
- `clean`: clean generated artifacts (but not everything)
- `clean-stamps`: force libraries and programs to rerun their configure step.
- `list_install_components`

### Targets for Building Libraries and Programs
Macaulay2 uses several external libraries and programs, which can be built using the following targets:
- `build-libraries`: build all libraries
  - `build-bdwgc`:	[Boehm-Demers-Weiser] C/C++ Garbage Collector library
  - `build-cddlib`:	[cddlib] library for the Double Description Method of Motzkin et al.
  - `build-eigen`:	[Eigen3] C++ template library for linear algebra
  - `build-factory`: [Singular-Factory] library for multivariate polynomal representations
  - `build-fflas_ffpack`: [fflas-ffpack] library for finite field linear algebra routines
  - `build-flint`:	[FLINT] Fast Library for Number Theory
  - `build-frobby`:	[Frobby] library for computations with monomial ideals
  - `build-givaro`:	[Givaro] library for algebraic computations over prime and finite fields
  - `build-glpk`:	[GLPK] GNU Linear Programming Kit
  - `build-googletest`: [Googletest] C++ unit-testing library
  - `build-mathic`:	[Mathic] library for symbolic algebra data structures
  - `build-mathicgb`: [Mathicgb] library for signature Groebner bases library
  - `build-memtailor`: [Memtailor] library for special purpose memory allocators
  - `build-mpfr`:	[MPFR] GNU Multiple Precision Floating Point library
  - `build-mpfi`:	[MPFI] a multiple precision interval arithmetic library based on MPFR
  - `build-mpir`:	[MPIR] Multiple Precision Integers & Rationals library (optional replacement for GMP)
  - `build-mpsolve`: [MPSolve] library for solving multiprecision polynomials
  - `build-ntl`:	[NTL] library for doing number theory

[Boehm-Demers-Weiser]: https://www.hboehm.info/gc/
[cddlib]: https://github.com/cddlib/cddlib
[Eigen3]: http://eigen.tuxfamily.org
[Singular-Factory]: https://github.com/Singular/Sources/tree/spielwiese/factory
[fflas-ffpack]: https://linbox-team.github.io/fflas-ffpack/
[FLINT]: http://www.flintlib.org/
[Frobby]: https://www.broune.com/frobby/
[Givaro]: https://github.com/linbox-team/givaro
[GLPK]: https://www.gnu.org/software/glpk/
[Googletest]: https://github.com/google/googletest
[Mathic]: https://github.com/broune/mathic
[Mathicgb]: https://github.com/broune/mathicgb
[Memtailor]: https://github.com/broune/memtailor
[MPFR]: https://www.mpfr.org/
[MPFI]: http://perso.ens-lyon.fr/nathalie.revol/software.html
[MPIR]: http://mpir.org/
[MPSolve]: https://github.com/robol/MPSolve
[NTL]: https://www.shoup.net/ntl/

- `build-programs`: build all programs
  - `build-4ti2`:	[4ti2] software for algebraic, geometric, and combinatorial problems on linear spaces
  - `build-cohomcalg`: [cohomCalg] software for computation of sheaf cohomologies for line bundles on toric varieties
  - `build-csdp`:	[CSDP] software for solving semidefinite programming problems
  - `build-gfan`:	[Gfan] software for computing Grobner fans and tropical varieties
  - `build-lrslib`:	[lrs] software for vertex enumeration/convex hull problems
  - `build-nauty`:	[nauty] software for computing automorphism groups of graphs and digraphs
  - `build-normaliz`: [Normaliz] software for computations in affine monoids, lattice polytopes, and rational cones
  - `build-topcom`:	[TOPCOM] software for computing triangulations of point configurations and oriented matroids

Additionally, build targets are available for a few programs which are not built and distributed by default due to time or licensing constraints:
  - `build-bertini`:	[Bertini] software for numerical algebraic geometry
  - `build-phcpack`:	[PHCpack] software for solving polynomial systems by homotopy continuation methods
  - `build-polymake`:	[polymake] software for research in polyhedral geometry

[4ti2]: https://4ti2.github.io/
[Bertini]: https://bertini.nd.edu/
[CSDP]: https://github.com/coin-or/Csdp/wiki
[Gfan]: https://users-math.au.dk/~jensen/software/gfan/gfan.html
[Normaliz]: https://www.normaliz.uni-osnabrueck.de/
[TOPCOM]: https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM/
[cohomCalg]: https://github.com/BenjaminJurke/cohomCalg
[lrs]: http://cgm.cs.mcgill.ca/~avis/C/lrs.html
[nauty]: http://pallini.di.uniroma1.it/
[phcpack]: https://github.com/janverschelde/PHCpack
[polymake]: https://polymake.org/

Note that the targets for individual libraries and programs only build the respective component in the `libraries` subdirectory in the build directory, while the `build-libraries` and `build-programs` targets also invoke `build-[LIBRARY or PROGRAM]-install` on each component in order to install the artifacts in the `usr-host` subdirectory.

### Targets for Autotuning Libraries
Targets for autotuning various parameters in libraries:
- `build-fflas_ffpack-autotune`: generates `fflas-ffpack-thresholds.h`
- `build-ntl-wizard`: run the NTL wizard

Note: rerun the corresponding `build-[library]-install` target after the targets above.

### Targets for Building Macaulay2
The main targets for building Macaulay2 are:
- `M2-engine`: build the `libM2-engine` library
- `M2-binary`: build the Macaulay2 executable
- `M2-core`: generate and copy the Core package
- `M2-emacs`: generate the M2-mode package for Emacs
- `M2-highlightjs`: generate syntax highlighting library for javascript

In addition, the following targets are available:
- `scc1`: build the Safe C Compiler
- `M2-interpreter`: translate `.d` and `.dd` sources into C and C++ sources
- `M2-supervisor`: build the Macaulay2 multithreading supervisor
- `M2-tests`: build various miscellaneous tests (currently `ComputationsBook`)

### Targets for Installing Macaulay2 Packages
The following targets involve the Macaulay2 packages located in the `Macaulay2/packages` subdirectory in the source:
- `list-packages`: print the list of packages to be installed (can be modified using the `PACKAGES` cache option)
- `install-packages`: run `installPackage` for each [distributed package](Macaulay2/packages/%3Ddistributed-packages)
- `check-packages`: run `check` for each distributed package
- `uninstall-packages`: remove installed files for all packages

For specific packages, the following targets are also available:
- `install-[PACKAGE]`: install the individual package
- `check-[PACKAGE]`: run the tests for the individual package
- `all-[PACKAGE]`: install and run the tests for the individual package
- `uninstall-[PACKAGE]`: uninstall the individual package

### Targets for Generating Macaulay2 Syntax Highlighters
- `M2-emacs`: generate the [M2.el](https://github.com/Macaulay2/M2-emacs) package for Emacs
- `M2-editors`: generate various syntax highlighting grammar files, such as [this one](https://github.com/Macaulay2/language-macaulay2)


## FAQ
A list of common issues and errors related to the CMake build is available on the [GitHub Wiki](https://github.com/Macaulay2/M2/wiki/FAQ%3A-CMake-Build-Problems). If you run into a problem not listed there or among the list of issues labeled with [build system](https://github.com/Macaulay2/M2/labels/build%20system), please open a [new issue](https://github.com/Macaulay2/M2/issues/new) on GitHub.
