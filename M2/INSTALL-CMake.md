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
Clone Macaulay2:
```
git clone https://github.com/mahrud/M2.git -b feature/cmake
```
Setup the build directory:
```
cd M2/M2/BUILD/build
cmake -S ../.. -B . \
      -DMP_LIBRARY=MPIR \
      -DBUILD_TESTING=ON \
      -DCMAKE_BUILD_TYPE=Release
```
Build the libraries and programs that Macaulay2 requires:
```
make build-libraries build-programs rebuild_cache
```
Build the Macaulay2 binary and core packagesb:
```
make -j4 M2-binary M2-core
```
Install and check packages:
```
make -j4 install install-packages check-packages
```

To install on the system, rerun CMake with `-DCMAKE_INSTALL_PREFIX=/usr` option.

### From Xcode or Visual Studio
IDEs such as Xcode or vscode can also configure and build using CMake.
Simply open the cloned repository using your IDE and select target to build.

## Advanced targets and options
After running `cmake ../..`, you can use `cmake -LA` to see a list of computed variables and options.
To change any, run `cmake -DVARIABLE=VALUE` (don't forget the `-D` prefix). Note that these changes are
sticky, meaning that to unset variables you need to run `cmake -UVARIABLE`.

The following are some general targets:
- `all (default)`
- `depend`
- `install`
  - `install/local`: only install targets in the current directory
  - `install/strip`: install and strip binaries and executables
- `edit_cache`: edit configure options and cache variables
- `rebuild_cache`: rebuild the cache
- `clean`: clean installed files
- `clean-stamps`: clean the install stamps on libraries and programs
- `list_install_components`

### Building Libraries and Programs

Macaulay2 uses several external libraries and programs, which can be built using the following targets:
- `build-libraries`: build all libraries
  - `build-bdwgc`:	[Boehm-Demers-Weiser] C/C++ Garbage Collector library
  - `build-cddlib`:	[cddlib] library for the Double Description Method of Motzkin et al.
  - `build-factory`: [Singular-Factory] library for multivariate polynomal representations
  - `build-fflas_ffpack`: [fflas-ffpack] library for finite field linear algebra routines
  - `build-flint`:	[FLINT] Fast Library for Number Theory
  - `build-frobby`:	[Frobby] library for computations with monomial ideals
  - `build-givaro`:	[Givaro] library for algebraic computations over prime and finite fields
  - `build-glpk`:	[GLPK] GNU Linear Programming Kit
  - `build-googletest`: [Googletest] C++ unit-testing library
  - `build-mathic`:	library for symbolic algebra data structures
  - `build-mathicgb`: library for signature Groebner bases library
  - `build-memtailor`: library for special purpose memory allocators
  - `build-mpfr`:	[MPFR] GNU Multiple Precision Floating Point library
  - `build-mpir`:	[MPIR] Multiple Precision Integers & Rationals library (optional replacement for GMP)
  - `build-mpsolve`: [MPSolve] library for solving multiprecision polynomials
  - `build-ntl`:	[NTL] library for doing number theory

[Boehm-Demers-Weiser]: https://www.hboehm.info/gc/
[cddlib]: https://github.com/cddlib/cddlib
[Singular-Factory]: https://github.com/Singular/Sources/tree/spielwiese/factory
[fflas-ffpack]: https://linbox-team.github.io/fflas-ffpack/
[FLINT]: http://www.flintlib.org/
[Frobby]: https://www.broune.com/frobby/
[Givaro]: https://github.com/linbox-team/givaro
[GLPK]: https://www.gnu.org/software/glpk/
[Googletest]: https://github.com/google/googletest
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

[4ti2]: https://4ti2.github.io/
[CSDP]: https://github.com/coin-or/Csdp/wiki
[Gfan]: https://users-math.au.dk/~jensen/software/gfan/gfan.html
[Normaliz]: https://www.normaliz.uni-osnabrueck.de/
[TOPCOM]: http://www.rambau.wm.uni-bayreuth.de/TOPCOM/
[cohomCalg]: https://github.com/BenjaminJurke/cohomCalg
[lrs]: http://cgm.cs.mcgill.ca/~avis/C/lrs.html
[nauty]: http://pallini.di.uniroma1.it/

Note that the targets for individual libraries and programs only build the respective component in 
the `libraries` subdirectory in the build directory, while the `build-libraries` and `build-programs`
also invoke `build-[LIBRARY or PROGRAM]-install` on each component in order to install the artifacts
in the `usr-host` subdirectory.

### Building Macaulay2

The main targets for building Macaulay2 are:
- `M2-engine`: build the `libM2-engine` library
- `M2-binary`: build the Macaulay2 executable
- `M2-core`: build (and install) the core
- `M2-emacs`: build the M2-mode package for Emacs

In addition, the following targets are available for 
- `scc1`: build the Simple C Compiler
- `M2-interpreter`: translate `.d` and `.dd` sources into C and C++ sources
- `M2-regex`
- `M2-system`
- `M2-kernel`
- `e-includes`

### Installing the Packages

The following targets involve the Macaulay2 packages located in the `Macaulay2/packages` subdirectory in the source:
- `install-packages`: run `installPackage` for each [distributed package](Macaulay2/packages/%3Ddistributed-packages)
- `check-packages`: run `check` for each distributed package
- `uninstall-packages`: remove installed files for all packages

For specific packages, the following targets are also available:
- `install-[PACKAGE]`: install the individual package (note: does not install required packages)
- `check-[PACKAGE]`: run the tests for the individual package
- `all-[PACKAGE]`: install and run the tests for the individual package
- `uninstall-[PACKAGE]`: uninstall the individual package

## Testing and Checking Macaulay2

There are unit tests available within the `Macaulay2/e/unit-tests` and `Macaulay2/tests` directories which 
can be tested using the CTest utility. Here are some examples of using `ctest`:
```
cd Macaulay2/tests # within the build directory
ctest -N
ctest --build-and-test
ctest --build-run-dir
ctest -R schorder --rerun-failed -V
```

## Package Macaulay2 (TODO)

The CPack utility supports making compressed packages, as well as `deb` and `rpm` packages for Linux
and `dmg` packages for OSX. The following commands enable packaging and create a package Debian:
```
cmake -DDEB=ON .
make package
```

## FAQ

Below are a list of common issues and errors. If you run into a problem not listed below,
please open a [new issue](https://github.com/Macaulay2/M2/issues/new) on GitHub.

<details>
<summary><code>error while loading shared libraries: libgmp.so.23: cannot open shared object file</code></summary>
This error occurs when a library or component is linked with libmpir, located in `usr-host/lib`, but that
directory is not in the path of the linker.
<code><pre>
export LD_LIBRARY_PATH=[BUILD DIRECTORY]/usr-host/lib
</code></pre>
</details>


<details>
<summary><code>/usr/bin/ld: warning: libgmp.so.10, needed by ..., may conflict with libgmp.so.23</code></summary>
This happens because the local version of a high level library, for instance libflint, is linked against
an older version of a lower level library, such as libmpfr or libgmp.
<code><pre>
/usr/bin/ld: warning: libgmp.so.10, needed by ../../usr-host/lib/libmpfr.so, may conflict with libgmp.so.23
/usr/bin/ld: warning: libgmp.so.10, needed by /usr/lib64/libntl.so, may conflict with libgmp.so.23
/usr/bin/ld: warning: libmpfr.so.4, needed by /usr/local/lib/libflint.so, may conflict with libmpfr.so.6
/usr/bin/ld: warning: libgmpxx.so.4, needed by /usr/lib64/libfrobby.so, may conflict with libgmpxx.so.8
</code></pre>
Solution:
<code><pre>
make build-[mpfr | ntl | flint | frobby]-install
cmake -U*[MPFR | NTL | FLINT | FROBBY]* .
</code></pre>
</details>


<details>
<summary><code>fatal error: no member named 'isunit' in '...'; did you mean 'isUnit'?</code></summary>
Yes. This error occurs when the local givaro headers are version 4.0.2 or below, but we have
built version 4.0.3 or above in `usr-host`.
<code><pre>
cmake -U*GIVARO* .
</code></pre>
</details>


<details>
<summary><code>error: Runtime CPU support is only available with GCC 4.6 or later.</code></summary>
When compiling using Clang, the following error might occur:
<code><pre>
[ 25%] Building CXX object Macaulay2/e/CMakeFiles/M2-engine.dir/ntl-internal.cpp.o
In file included from M2/Macaulay2/e/ntl-debugio.cpp:4:
In file included from M2/Macaulay2/e/./ntl-interface.hpp:16:
In file included from /usr/include/NTL/ZZ.h:18:
In file included from /usr/include/NTL/lip.h:5:
/usr/include/NTL/ctools.h:510:2: fatal error: Runtime CPU support is only available with GCC 4.6 or later.
#error Runtime CPU support is only available with GCC 4.6 or later.
 ^
</code></pre>
If this happens, run the following to build NTL in the `usr-host` library and use it.
<code><pre>
make build-ntl-install
cmake -UNTL_* .
</code></pre>
</details>


<details>
<summary><code>error: variable '_flint_primes' cannot be threadprivate because it is thread-local</code></summary>
If certain prerequisite libraries, such as OpenMP, are not installed before beginning the process,
<code><pre>
[ 60%] Building CXX object Macaulay2/e/CMakeFiles/M2-unit-tests.dir/unit-tests/ARingZZTest.cpp.o
In file included from M2/Macaulay2/e/unit-tests/ARingZZTest.cpp:11:
In file included from M2/Macaulay2/e/./aring-zz-flint.hpp:18:
In file included from /usr/local/include/flint/arith.h:24:
In file included from /usr/local/include/flint/fmpz.h:31:
In file included from /usr/local/include/flint/nmod_vec.h:29:
/usr/local/include/flint/ulong_extras.h:123:27: error: variable '_flint_primes' cannot be threadprivate because it is thread-local
#pragma omp threadprivate(_flint_primes, _flint_prime_inverses, _flint_primes_used)
                          ^
<code><pre>
If this happens, run the following to build Flint in the `usr-host` library and use it.
</code></pre>
make build-flint-install
cmake -UFLINT_* .
</code></pre>
</details>


<details>
<summary><code>recompile with -fPIC</code></summary>
TODO
<code><pre>
/usr/bin/ld: M2/BUILD/build/usr-host/lib/libgmp.a(randmts.o): relocation R_X86_64_32S against `.rodata' can not be used when making a shared object; recompile with -fPIC
</code></pre>
</details>


<details>
<summary><code>sample Factory finite field addition table file missing, needed for factorization: ...</code></code></summary>
TODO
<code><pre>
[ 60%] Generating ../../usr-dist/share/Macaulay2/Core/tvalues.m2
../../../../../../Macaulay2/m2/debugging.m2:20:6:(1):[7]: error: sample Factory finite field addition table file missing, needed for factorization: /home/mahrud/Projects/M2/M2/M2/BUILD/build-cmake/usr-dist/
</code></pre>
</details>


<details>
<summary>How to compile with Intel(R) Math Kernel Library</summary>
[MKL](https://software.intel.com/en-us/mkl) is a linear algebra routines library specifically optimized for
Intel(R) processors. To enable linking with MKL, adjust the path and architecture appropriately and run the
following before calling `cmake`:
<code><pre>
source /opt/intel/bin/compilervars.sh intel64
</code></pre>
Note that MKL is closed-source but released as a freeware.
</details>


<details>
<summary>How to compile with AMD(R) Optimizing CPU Libraries</summary>
TODO: [AOCL](https://developer.amd.com/amd-aocl/) includes AMD BLIS and AMD libFLAME
</details>
