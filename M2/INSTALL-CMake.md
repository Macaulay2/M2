Building Macaulay2 from Source using CMake
==========================================

## Warning!
This is not (yet) the official build system of Macaulay2.

## Why CMake?
CMake is a cross-platform system for generating build environments using native tools such as
Makefiles and Ninja or IDEs such as Xcode and Visual Studio.
See this article on [why the KDE project switched to CMake](https://lwn.net/Articles/188693/) and this list of
[cool CMake features](https://gitlab.kitware.com/cmake/community/-/wikis/doc/cmake/Really-Cool-CMake-Features).

## Getting started
[Download](https://cmake.org/download/) the latest release of CMake for your platform.
If using a packaged distribution, confirm using `cmake --version` that you have version at least 3.14.

There are various other tools needed to compile Macaulay2 dependencies.
On Ubuntu, install `autoconf build-essential bison libtool yasm`.
See [INSTALL](INSTALL) for more details for Mac OS X and other systems.

There are also 7 libraries that must be found on the system. On Ubuntu, install
`libopenblas-dev libeigen3-dev libxml2-dev libreadline-dev libomp-dev libtbb-dev libgdbm-dev`.
In addition, install `libatomic-ops-dev`, though this dependency will be removed soon.

A quick build involves the following steps:
```
git clone https://github.com/mahrud/M2.git -b feature/cmake
cmake -S M2/M2 -B M2/M2/BUILD/build -DCMAKE_BUILD_TYPE=Release
cmake --build M2/M2/BUILD/build --target build-libraries build-programs
cmake --build M2/M2/BUILD/build --target install-packages -j4
cmake --install M2/M2/BUILD/build
```
Each step is explained separately in the next section.

### Building Macaulay2
1. Clone Macaulay2 and switch to branch `feature/cmake`:
```
git clone https://github.com/mahrud/M2.git -b feature/cmake
```

2. Setup the build environment:
```
cd M2/M2/BUILD/build
cmake -S ../.. -B . \
      -DUSING_MPIR=ON \
      -DCMAKE_BUILD_TYPE=Release
```
We build with MPIR as the multiple precision arithmetic library by default. To use GMP, use `-DUSING_MPIR=OFF` instead.
This command generates Makefiles. To generate Ninja files instead, add `-GNinja`.

3. Build the libraries that will be linked with the Macaulay2 executable:
```
make build-libraries
```
Note that this target must be built separately, before proceeding to `M2-binary`.

4. Build the Macaulay2 binary and core scripts:
```
make M2-binary M2-core -j4
```
The `M2-binary` is a prerequisite of `M2-core`, which is currently the default target, so `make -j4` also suffices.

5. Build the programs used by some Macaulay2 packages:
```
make build-programs
```

6. (Optional) Install and check packages:
```
make install-packages check-packages -j4
```

### Testing Macaulay2
There are unit-tests available within the `Macaulay2/e/unit-tests` and `Macaulay2/tests` directories which
can be tested using the CTest utility. Here are some examples of using `ctest`:
- `ctest --build-and-test`: build and run all tests
- `ctest -N`: list all tests
- `ctest -R unit-tests`: run all tests in `e/unit-tests`
- `ctest -R normal -j6`: run all tests in `tests/normal`, in 6 parallel jobs
- `ctest --rerun-failed -V`: rerun the tests that failed in the last batch, printing the results

### Installing Macaulay2
To install on `/usr`, simply run `make install`. To change the installation prefix, add the flag:
```
cmake -DCMAKE_INSTALL_PREFIX=/usr/local .
```
Note that by default the `install` target depends on the `all` target.

### Packaging Macaulay2
CMake also supports creating rpm and deb packages as well as archives and dmg images using the `cpack` utility:
```
cpack -G DEB	# requires dpkg
cpack -G RPM	# requires rpmbuild
```

## Advanced cached flags
Within the build environment, you can use `cmake -L .` to see a list of computed flags and options.
Use `cmake -DVARIABLE=VALUE .` (note the `-D` prefix) to change a cached variable and 
use `cmake -UVARIABLE` to unset the variable. Wildcard unsetting is allowed; e.g. `cmake -U*VAR* .`

For a complete list, along with descriptions, try `cmake -LAH .`. Here are the most useful flags,
where the format is `[FLAG]:[TYPE]=[DEFAULT VALUE]`, though specifying the type is optional.

### Build flags
- `BUILD_LIBRARIES:BOOL=ON`: build all libraries, even if found on the system
- `BUILD_PROGRAMS:BOOL=ON`: build all programs, even if found on the system
- `BUILD_TESTING:BOOL=ON`: build the testing targets
- `CMAKE_BUILD_TYPE:STRING=Release`: choose the type of build, options are: `Debug` `Release` `RelWithDebInfo` `MinSizeRel`
- `CMAKE_INSTALL_PREFIX:PATH=/usr`: installation prefix
- `M2_HOST_PREFIX:PATH=${CMAKE_BINARY_DIR}/usr-host`: host build prefix
- `M2_DIST_PREFIX:PATH=${CMAKE_BINARY_DIR}/usr-dist`: target build prefix

- `MEMDEBUG:BOOL=OFF`: enable memory allocation debugging
- `USING_MPIR:BOOL=ON`: use MPIR instead of GMP
- `PARALLEL_JOBS:STRING=4`: specify the number of parallel jobs for libraries and programs
- `GIT_SUBMODULE:BOOL=OFF`: update submodules during build

### Macaulay2 flags
- `M2_CheckDocumentation:STRING=true`: check documentation for completeness
- `M2_IgnoreExampleErrors:STRING=true`: ignore errors in example code
- `M2_ReinstallPackages:STRING=false`: reinstall the packages
- `M2_RemakeAllDocumentation:STRING=false`: remake all documentation
- `M2_RerunExamples:STRING=false`: rerun example outpuat files
- `M2_debugLevel:STRING=0`: set the debugging level
- `M2_errorDepth:STRING=3`: set the error printing depth
- `M2_gbTrace:STRING=0`: set the Groebner basis trace level
- `GC_MAXIMUM_HEAP_SIZE:STRING=400M`: maximum collected heap size for tests
- `PACKAGES:INTERNAL=[list of packages]`: a space-separated list of packages


## Advanced targets and options
Within the build environment, use `make help` (or `ninja help`) to get a list of valid targets.

The following are the general targets:
- `all (default)`
- `depend`
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

In addition, the following targets are available for
- `scc1`: build the Safe C Compiler
- `M2-interpreter`: translate `.d` and `.dd` sources into C and C++ sources
- `M2-regex`
- `M2-system`
- `M2-kernel`
- `e-includes`

### Targets for Installing Macaulay2 Packages
The following targets involve the Macaulay2 packages located in the `Macaulay2/packages` subdirectory in the source:
- `list-packages`: print the list of packages to be installed (can be modified using the `PACKAGES` cache option)
- `install-packages`: run `installPackage` for each [distributed package](Macaulay2/packages/%3Ddistributed-packages)
- `check-packages`: run `check` for each distributed package
- `uninstall-packages`: remove installed files for all packages

For specific packages, the following targets are also available:
- `install-[PACKAGE]`: install the individual package (note: does not install required packages)
- `check-[PACKAGE]`: run the tests for the individual package
- `all-[PACKAGE]`: install and run the tests for the individual package
- `uninstall-[PACKAGE]`: uninstall the individual package

## FAQ
Below are a list of common issues and errors. If you run into a problem not listed below,
please open a [new issue](https://github.com/Macaulay2/M2/issues/new) on GitHub.


<details>
<summary><code>error while loading shared libraries: libgmp.so.23: cannot open shared object file</code></summary>
This error occurs when a library or component is linked with libmpir, located in `usr-host/lib`, but that
directory is not in the path of the linker. Solution:
<pre>
export LD_LIBRARY_PATH=[BUILD DIRECTORY]/usr-host/lib
</pre>
</details>


<details>
<summary><code>warning: libgmp.so.10, needed by ..., may conflict with libgmp.so.23</code></summary>
This happens because the local version of a high level library, for instance libflint, is linked against
an older version of a lower level library, such as libmpfr or libgmp.
<pre>
/usr/bin/ld: warning: libgmp.so.10, needed by ../../usr-host/lib/libmpfr.so, may conflict with libgmp.so.23
/usr/bin/ld: warning: libgmp.so.10, needed by /usr/lib64/libntl.so, may conflict with libgmp.so.23
/usr/bin/ld: warning: libmpfr.so.4, needed by /usr/local/lib/libflint.so, may conflict with libmpfr.so.6
/usr/bin/ld: warning: libgmpxx.so.4, needed by /usr/lib64/libfrobby.so, may conflict with libgmpxx.so.8
</pre>
Solution:
<pre>
make build-[mpfr | ntl | flint | frobby]-install
cmake -U*[MPFR | NTL | FLINT | FROBBY]* .
</pre>
</details>


<details>
<summary><code>cf_factor.cc:(.text+0x20b9): undefined reference to 'fq_nmod_poly_factor_init'</code></summary>
<pre>
/usr/bin/ld: ../../usr-host/lib/libfactory.a(cf_factor.o): in function `factorize(CanonicalForm const&, Variable const&)':
cf_factor.cc:(.text+0x20b9): undefined reference to `fq_nmod_poly_factor_init'
</pre>
TODO
</details>


<details>
<summary><code>TLS reference in libfactory.a(facFqBivar.o) mismatches non-TLS definition in libntl.a(lzz_p.o) section .bss</code></summary>
This is most likely caused when factory is linked with a local ntl library. TODO
<pre>
/usr/bin/ld: _ZN3NTL8zz_pInfoE: TLS reference in ../../usr-host/lib/libfactory.a(facFqBivar.o) mismatches non-TLS definition in ../../usr-host/lib/libntl.a(lzz_p.o) section .bss
</pre>
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
make build-bdwgc-install
cmake -U*BDWGC* .
</pre>
</details>


<details>
<summary><code>fatal error: no member named 'isunit' in '...'; did you mean 'isUnit'?</code></summary>
Yes. This error occurs when the local givaro headers are version 4.0.2 or below, but we have
built version 4.0.3 or above in `usr-host`.
<pre>
cmake -U*GIVARO* .
</pre>
</details>


<details>
<summary><code>error: Runtime CPU support is only available with GCC 4.6 or later.</code></summary>
When compiling using Clang, the following error might occur:
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
If this happens, run the following to build NTL in the `usr-host` library and use it.
<pre>
make build-ntl-install
cmake -UNTL_* .
</pre>
</details>


<details>
<summary><code>error: variable '_flint_primes' cannot be threadprivate because it is thread-local</code></summary>
If certain prerequisite libraries, such as OpenMP, are not installed before beginning the process, the compiler
cannot understand the respective pragmas.
<pre>
[ 60%] Building CXX object Macaulay2/e/CMakeFiles/M2-unit-tests.dir/unit-tests/ARingZZTest.cpp.o
In file included from M2/Macaulay2/e/unit-tests/ARingZZTest.cpp:11:
In file included from M2/Macaulay2/e/./aring-zz-flint.hpp:18:
In file included from /usr/local/include/flint/arith.h:24:
In file included from /usr/local/include/flint/fmpz.h:31:
In file included from /usr/local/include/flint/nmod_vec.h:29:
/usr/local/include/flint/ulong_extras.h:123:27: error: variable '_flint_primes' cannot be threadprivate because it is thread-local
#pragma omp threadprivate(_flint_primes, _flint_prime_inverses, _flint_primes_used)
                          ^
</pre>
If this happens, run the following to build Flint in the `usr-host` library and use it.
<pre>
make build-flint-install
cmake -UFLINT_* .
</pre>
</details>


<details>
<summary><code>recompile with -fPIC</code></summary>
This error involves the choice of static and shared libraries. Please make an issue with information about your system.
<pre>
/usr/bin/ld: M2/BUILD/build/usr-host/lib/libgmp.a(randmts.o): relocation R_X86_64_32S against `.rodata' can not be used when making a shared object; recompile with -fPIC
</pre>
</details>


<details>
<summary><code>sample Factory finite field addition table file missing, needed for factorization: ...</code></code></summary>
This error occurs if the `factory/gftables` folder is missing from the `Core` directory.
<pre>
[ 60%] Generating ../../usr-dist/share/Macaulay2/Core/tvalues.m2
../../../../../../Macaulay2/m2/debugging.m2:20:6:(1):[7]: error: sample Factory finite field addition table file missing, needed for factorization: /home/mahrud/Projects/M2/M2/M2/BUILD/build-cmake/usr-dist/
</pre>
Solution:
<pre>
make build-factory-install
</pre>
</details>


<details>
<summary>How to compile with Intel(R) Math Kernel Library</summary>
[MKL](https://software.intel.com/en-us/mkl) is a linear algebra routines library specifically optimized for
Intel(R) processors. To enable linking with MKL, adjust the path and architecture appropriately and run the
following before calling `cmake`:
<pre>
source /opt/intel/bin/compilervars.sh intel64
</pre>
Note that MKL is closed-source but released as a freeware.
</details>


<details>
<summary>How to compile with AMD(R) Optimizing CPU Libraries</summary>
TODO: [AOCL](https://developer.amd.com/amd-aocl/) includes AMD BLIS and AMD libFLAME
</details>
