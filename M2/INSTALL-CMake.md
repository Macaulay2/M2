Building Macaulay2 from Source using CMake
==========================================

![Build and Test Macaulay2](https://github.com/Macaulay2/M2/workflows/Build%20and%20Test%20Macaulay2/badge.svg?branch=master)

## Why CMake?
CMake is a cross-platform system for generating build environments using native tools such as Makefiles and Ninja or IDEs such as Xcode and Visual Studio. See this article on [why the KDE project switched to CMake](https://lwn.net/Articles/188693/) and this list of [cool CMake features](https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Really-Cool-CMake-Features).

## Getting started
[Download](https://cmake.org/download/) the latest CMake for your platform.
If using a packaged distribution, confirm using `cmake --version` that you have version at least 3.15.

**NOTE**: This build system is tested on GCC, Clang, and AppleClang compilers. Optionally, install `ccache` for caching compiler artifacts and `ninja-build` (`ninja` on Brew) for optimized parallel builds.

There are various tools needed to compile Macaulay2 dependencies.
- On Debian/Ubuntu, install `autoconf build-essential bison libtool pkg-config yasm`.
- On Fedora/CentOS, install `autoconf automake bison libtool pkg-config yasm`.
- On Mac OS X, using Homebrew, install `autoconf automake bison libtool pkg-config yasm`.

There are 7 libraries that must be found on the system.
- On Debian/Ubuntu, install `libopenblas-dev libgmp3-dev libxml2-dev libreadline-dev libgdbm-dev libboost-regex-dev libboost-stacktrace-dev libatomic-ops-dev`.
- On Fedora/CentOS, install `openblas-devel gmp-devel libxml2-devel readline-devel gdbm-devel boost-devel libatomic_ops-devel`.
- On Mac OS X, using Homebrew, install `gmp libxml2 readline gdbm boost libatomic_ops`.

Finally, there are 2 optional libraries that help with building other requirements.
- On Debian/Ubuntu, install `libomp-dev libtbb-dev`.
- On Fedora/CentOS, install `libomp-devel tbb-devel`.
- On Mac OS X, using Homebrew, install `libomp tbb`.

**NOTE**: the source directory must not contain any build artifacts from an in-source build. If you have built Macaulay2 in-source before, clean the build artifacts first by running `make clean distclean` in the source directory.

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
- `WITH_OMP:BOOL=OFF`: link with the OpenMP library
- `WITH_PYTHON:BOOL=OFF`: link with the Python library
- `WITH_SQL:BOOL=OFF`: link with the MySQL library
- `WITH_TBB:BOOL=OFF`: link with the TBB library
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
[TOPCOM]: http://www.rambau.wm.uni-bayreuth.de/TOPCOM/
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
- `M2-core`: build (and install) the core
- `M2-emacs`: build the M2-mode package for Emacs

In addition, the following targets are available:
- `scc1`: build the Safe C Compiler
- `M2-interpreter`: translate `.d` and `.dd` sources into C and C++ sources
- `M2-regex`: build the vendored GNU regex library
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
- `M2-editors`: generate various syntax highlighting grammar files


## FAQ
Below are a list of common issues and errors. If you run into a problem not listed below or among the list of issues labeled with [build system](https://github.com/Macaulay2/M2/labels/build%20system), please open a [new issue](https://github.com/Macaulay2/M2/issues/new) on GitHub.


<details>
<summary>Installing dependencies using Linuxbrew</summary>

On macOS CMake automatically finds libraries installed on the Homebrew prefix. In order to use Linuxbrew (which has the same interface and packages), use the following command to tell CMake to look under the right prefix:
```
cmake -DCMAKE_SYSTEM_PREFIX_PATH=`brew --prefix` .
```
</details>

<details>
<summary>Building from the source code tarfile</summary>

When building from a source code tarfile (e.g. downloaded from GitHub, rather than a git clone), you may see an error like this:
```
  No download info given for 'build-bdwgc' and its source directory: ...
```
To resolve this, you should download and extract the source tarfiles for each git submodule in the `M2/submodules` directory. Also, you may need to manually turn off submodule updates:
```
cmake -DGIT_SUBMODULE=off .
```
</details>

<details>
<summary><code>/usr/include/c++/10.1.0/bits/unique_ptr.h:594:9: error: no matching function for call to std::__uniq_ptr_data</code> when using GCC 10 or Clang 10</summary>

This issue is due to an old version of FFLAS_FFPACK or Givaro libraries inserting an unnecessary `-fabi-version=6` flag to the compile command. Try uninstalling the packaged version of those libraries and building them using the `build-givaro` and `build-fflas_ffpack` targets.
</details>

<details>
<summary><code>/usr/include/boost/regex/v4/cpp_regex_traits.hpp:966: undefined reference to `boost::re_detail_107100::cpp_regex_traits_implementation<char>::transform_primary(char const*, char const*) const'</code></summary>

Same as above.
</details>

<details>
<summary><code>collect2: error: ld returned 1 exit status</code> on WSL2</summary>

This issue is likely due to a memory exhaustion bug in WSL2. Try cleaning the build artifacts and building with parallelization disabled:
```
ninja clean
ninja M2-core -j1
```
</details>

<details>
<summary><code>undefined reference to cblas_dgemm</code> on Arch Linux</summary>
The default OpenBLAS package on Arch Linux does not include function declarations for LAPACK and CBLAS, causing issues with some libraries and parts of Macaulay2. Try installing the community package [OpenBLAS-LAPACK](https://aur.archlinux.org/packages/openblas-lapack/) instead.
</details>

<details>
<summary>CMake is not using the local version of MPIR, Flint, etc.</summary>

Currently, when CMake is set to use the MPIR library, it compiles MPIR and a number of other libraries from source, including MPFR, NTL, Flint, Factory, Frobby, and Givaro. This is done to avoid linking conflicts caused by the libraries linking instead with the GMP library. Therefore, in order to link with system libraries the `-DUSING_MPIR=OFF` option is required. See this [comment](https://github.com/Macaulay2/M2/issues/1275#issuecomment-644217756) for more details on the reasoning behind this.
</details>

<details>
<summary><code>No download info given for 'build-flint' and its source directory</code></summary>

When building from a downloaded archive (i.e., not a git repository), it is necessary to also download and extract archives of the required submodules in the `M2/submodules` directory.

If a given library is not required on a particular system, CMake might still complain that the submodule directory is empty. One way to prevent this is to create a dummy file in the submodule directory for the libraries that are not required; for instance:
```
touch M2/submodules/flint2/empty
```
</details>

<details>
<summary>Detected library incompatibilities; rerun the build-libraries target</summary>

This message indicates that the build scripts detected an inconsistency between the libraries found on the system, and that those libraries are marked to be compiled from source. Run `ninja build-libraries` (or `make build-libraries`) to build the libraries from source.

If the problem persists, run `cmake --debug-trycompile` and open an issue with the contents of `CMakeFiles/CMakeError.log`.
</details>

<details>
<summary>macOS 10.15 Catalina issues with Clang and AppleClang</summary>

<b>Clang</b>: The Clang compiler installed via Homebrew or built from source typically includes OpenMP, but by default the system root is set to the Xcode SDK directory which does not contain the `libomp.dylib` library. The following command tells Clang how to find the correct library path:
```
export LIBRARY_PATH=`llvm-config --libdir`
cmake -S../.. -B. -GNinja -DCMAKE_BUILD_TYPE=Release
```
The `llvm-config` executable is typically located at `/usr/local/opt/llvm/bin/llvm-config`.

<b>AppleClang</b>: After ensuring that you have followed the usual steps from the [INSTALL](INSTALL) manual (e.g. running `xcode-select --install`, consider setting the `CMAKE_OSX_SYSROOT` variable to match the current SDK:
```
cmake -DCMAKE_OSX_SYSROOT=`xcrun --show-sdk-path` .
```
This would, for instance, tell CMake to look in `/Applications/Xcode.app/Contents/Developer/SDKs/MacOSX.sdk/usr/include` for headers.

Moreover, when building with MPIR, adding the following option allows CMake to find OpenMP from its own prefix rather than the common prefix at `/usr/local`, which may help avoid linking conflicts:
```
cmake -DCMAKE_SYSTEM_PREFIX_PATH=`brew --prefix libomp` .
```

If problems persist for either compiler, open an issue.
</details>

<details>
<summary><code>M2/include/M2/config.h:75:0: warning: "HAVE_ALARM" redefined</code></summary>

This error indicates that a previous in-source build has files left in the source tree. Run `make clean distclean` from the source directory or start from a clean clone of Macaulay2.
</details>

<details>
<summary><code>engine.h:84:3: error: ‘gmp_arrayZZ’ does not name a type; did you mean ‘gmp_newZZ’?</code></summary>

Same as above.
</details>

<details>
<summary><code>gmp-util.h:96:31: error: ‘gmp_CCmutable’ was not declared in this scope</code></summary>

Same as above.
</details>

<details>
<summary><code>mpirxx.h:3482:13: error: ‘mpir_ui’ has not been declared</code></summary>

This error indicates that a system version of `gmp.h` was found before `mpir.h`. If this occurs, open an issue.
</details>


<details>
<summary><code>undefined reference to `GC_malloc`</code></summary>

This error occurs if the GC library path is not set correctly.
<pre>
[  4%] Linking C executable scc1
CMakeFiles/scc1.dir/scc1.c.o: In function `getmem':
/home/macaulay/M2/M2/Macaulay2/c/scc1.c:23: undefined reference to `GC_malloc'
</pre>
Solution:
<pre>
cmake --build . --target build-bdwgc-install
cmake -U*BDWGC* .
</pre>
</details>


<details>
<summary><code>error: Runtime CPU support is only available with GCC 4.6 or later.</code></summary>

When compiling using Clang, the following error might occur if NTL was built with GCC instead:
<pre>
[ 25%] Building CXX object Macaulay2/e/CMakeFiles/M2-engine.dir/ntl-internal.cpp.o
In file included from M2/Macaulay2/e/ntl-debugio.cpp:4:
In file included from M2/Macaulay2/e/./ntl-interface.hpp:16:
In file included from /usr/include/NTL/ZZ.h:18:
In file included from /usr/include/NTL/lip.h:5:
/usr/include/NTL/ctools.h:510:2: fatal error: Runtime CPU support is only available with GCC 4.6 or later.
#error Runtime CPU support is only available with GCC 4.6 or later.
 ^
</pre>
Solution:
<pre>
cmake --build . --target build-ntl-install
cmake -U*NTL_* .
</pre>
</details>


<details>
<summary>How to compile with Intel(R) Math Kernel Library</summary>

[MKL](https://software.intel.com/en-us/mkl) is a linear algebra routines library specifically optimized for Intel(R) processors. To enable linking with MKL, adjust the path and architecture appropriately and run the following before calling `cmake`:
<pre>
source /opt/intel/bin/compilervars.sh intel64
</pre>
Note that MKL is closed-source but released as a freeware.

See [FindLAPACK](https://cmake.org/cmake/help/latest/module/FindLAPACK.html) for information on specifying the linear algebra library by setting the value of `BLA_VENDOR` in `cmake/check-libraries.cmake`.
</details>


<details>
<summary>How to compile with AMD(R) Optimizing CPU Libraries</summary>

TODO: [AOCL](https://developer.amd.com/amd-aocl/) includes AMD BLIS and AMD libFLAME
</details>
