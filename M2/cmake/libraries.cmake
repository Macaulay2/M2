# TODO:
# for mac:    history
## tip: use cmake -LA to list resolved variables

set(TARFILES
  # My machine download these:
  4ti2      3053e7467b5585ad852f6a56e78e28352653943e7249ad5e5174d4744d174966 4ti2-1.6.9.tar.gz
  cddlib    fe6d04d494683cd451be5f6fe785e147f24e8ce3ef7387f048e739ceb4565ab5 cddlib-094h.tar.gz
  cohomcalg 367c52b99c0b0a4794b215181439bf54abe4998872d3ef25d793bc13c4d40e42 cohomCalg-0.32.tar.gz
  csdp      7f202a15f33483ee205dcfbd0573fdbd74911604bb739a04f8baa35f8a055c5b Csdp-6.2.0.tgz
  gfan      a674d5e5dc43634397de0d55dd5da3c32bd358d05f72b73a50e62c1a1686f10a gfan0.6.2.tar.gz
  glpk      e398be2e7cb8a98584325268704729872558a4a88555bc8a54139d017eb9ebae glpk-4.59.tar.gz
  lrslib    adf92f9c7e70c001340b9c28f414208d49c581df46b550f56ab9a360348e4f09 lrslib-062.tar.gz
  mpc       6985c538143c1208dcb1ac42cedad6ff52e267b47e5f970183a3e75125b43c2e mpc-1.1.0.tar.gz
  mpfr      1d3be708604eae0e42d578ba93b390c2a145f17743a744d8f3f8c2ad5855a38a mpfr-4.0.2.tar.xz
  nauty     5d52211cec767d8d8e43483d96202be235f85696d1373c307291273463c812fa nauty27b11.tar.gz
  normaliz  436a870a1ab9a5e0c2330f5900d904dc460938c17428db1c729318dbd9bf27aa normaliz-3.7.2.tar.gz
  ntl       b90b36c9dd8954c9bc54410b1d57c00be956ae1db5a062945822bbd7a86ab4d2 ntl-10.5.0.tar.gz
  pari      4a6532b3c77350363fa618ead5cd794a172d7b7e5757a28f7788e658b5469339 pari-2.11.2.tar.gz
  topcom    3f83b98f51ee859ec321bacabf7b172c25884f14848ab6c628326b987bd8aaab TOPCOM-0.17.8.tar.gz
  # Everything possible:
  # LIBLIST gc gdbm mpir mpfr ntl flint factory lapack frobby glpk cddlib fplll givaro linbox boost mpc qd mpack gtest
  # PROGLIST 4ti2 gfan normaliz csdp nauty cddplus lrslib gftables topcom cohomcalg
  # SUBLIST memtailor mathic mathicgb fflas_ffpack
  # FIXME: gmp libtool pari are Missing below
  )

# The list LIBLIST is the list of libraries that might be used and linked into M2.
set(LIBLIST
  gc gdbm mpir mpfr ntl flint factory # lapack
  frobby glpk cddlib fplll givaro linbox boost mpc # qd gtest
  )

# The list PROGLIST is the list of programs and libraries for them that are distributed with M2.
#     Initially, we offer no option for not compiling some of them.
set(PROGLIST 4ti2 gfan normaliz csdp nauty cddplus lrslib topcom cohomcalg)

################################################################
## pkg-config is useful for fflas-ffpack and certain other packages
find_package(PkgConfig  REQUIRED QUIET)
set(ENV{PKG_CONFIG_PATH} "${M2_HOST_DIR}/lib/pkgconfig:${M2_HOST_DIR}/lib64/pkgconfig")

################################################################
## 1. Look for prerequisite packages and libraries using CMake or pkg-config

## Find libraries available as CMake modules
find_package(BLAS    3.8 REQUIRED QUIET)
find_package(LAPACK  3.8 REQUIRED QUIET) # TODO: both?
find_package(Eigen3  3.3 REQUIRED QUIET NO_MODULE)
find_package(LibXml2 2.9 REQUIRED QUIET) # need xmlNewNode
find_package(Threads 2.1 REQUIRED QUIET) # pthread
find_package(LibLZMA 5.2 REQUIRED QUIET) # need lzma_end
# OpenMP is required for building the library csdp and good for building the library normaliz
find_package(OpenMP      REQUIRED QUIET) # TODO: use OPENMP_LIBS/CXXFLAGS for csdb

## Find libraries available via pkg-config
# TODO: use foo>=VERSION to specify version
pkg_search_module(READLINE readline IMPORTED_TARGET) # TODO: make this REQUIRED
# TODO: replace readline with https://github.com/AmokHuginnsson/replxx

## TODO: remove these
find_library(LIBHISTORY history)
find_library(LIBGDBM gdbm)
find_library(LIBM m) # need pow from math.h
find_library(LIBC c)

## We provide modules for finding these libraries in cmake/
## They are not required because we can build them if they are not found.
#find_package(GMP  6.1.0 QUIET)
#find_package(MPC  1.1.0 QUIET)
#find_package(MPFR 4.0.2 QUIET)
#find_package(GLPK 4.59  QUIET)
#find_package(MPIR 3.0.0 QUIET)
#find_package(Mathicgb   QUIET)

# TODO: investigate error when factory-devel package is installed:
# sample Factory finite field addition table file missing, needed for factorization:
# /home/mahrud/Projects/M2/M2/M2/BUILD/mahrud/build/usr-dist//usr/share/factory/
#pkg_search_module(FACTORY      factory singular-factory IMPORTED_TARGET)
#pkg_search_module(FFLAS_FFPACK fflas-ffpack             IMPORTED_TARGET)
#pkg_search_module(BDWGC        bdw-gc                   IMPORTED_TARGET)

#find_library(GIVARO    libfrobby.a REQUIRED givaro                   IMPORTED_TARGET)
#find_library(LIBFROBBY libfrobby.a frobby PATHS ${M2_HOST_DIR}/lib)
# To fix the error, change the givaro requirement in ${BOOTSTRAP}/usr-host/lib/pkgconfig/fflas-ffpack.pc to 4.0.2

if(NOT ${MPIR_FOUND}) # also for GMP, MPC, MPFR, GLPK?
  set(BUILD_MPIR ON)
endif()

if(NOT ${FACTORY_FOUND})
  # TODO
endif()

if(NOT ${FFLAS_FFPACK_FOUND})
  # TODO
endif()

if(NOT ${BDWGC_FOUND})
  # TODO
endif()

#################################################################################

# TODO: add option to disable building shared libraries
# ifeq (@SHARED@,no)
# CONFIGOPTIONS += --disable-shared --enable-static
# endif

# Testing ..
set(M2_SOURCE_URL https://faculty.math.illinois.edu/Macaulay2/Downloads/OtherSourceCode)

include(ExternalProject) # populate at build time; FetchContent populates at configure time
find_program(MAKE_EXE NAMES make gmake nmake)

# TODO: Are these necessary to define?
# AR=${CMAKE_AR} OBJDUMP=${CMAKE_OBJDUMP} STRIP=${CMAKE_STRIP} RANLIB=${CMAKE_RANLIB} AS=${CMAKE_AS} DLLTOOL=${CMAKE_DLLTOOL}

# TODO: probably everything needs this:
#'CXXFLAGS=-std=gnu++11 -g3 -O2 -Wno-mismatched-tags -w -Wno-deprecated-register'
#'CFLAGS=-std=gnu11 -g3 -O2 -w -Wimplicit -Werror'
add_compile_options(-I${CMAKE_SOURCE_DIR}/include -I${CMAKE_BINARY_DIR}/include -I${M2_HOST_DIR}/include)
get_property(COMPILE_OPTIONS     DIRECTORY PROPERTY COMPILE_OPTIONS)
get_property(COMPILE_DEFINITIONS DIRECTORY PROPERTY COMPILE_DEFINITIONS)
set(CPPFLAGS "${COMPILE_OPTIONS}") # TODO: ;${COMPILE_DEFINITIONS}")
string(REPLACE ";" " " CPPFLAGS "${CPPFLAGS}")

add_link_options(-L${M2_HOST_DIR}/lib)
get_property(LINK_OPTIONS DIRECTORY PROPERTY LINK_OPTIONS)
set(LDFLAGS "${LINK_OPTIONS}")
string(REPLACE ";" " " LDFLAGS "${LDFLAGS}")

#################################################################################

#then
#     LIBS="`$PKG_CONFIG --libs $FACTORY_NAME` $LIBS"
#     CPPFLAGS="`$PKG_CONFIG --cflags $FACTORY_NAME` $CPPFLAGS"
#fi

# TODO: use flint2 from https://github.com/Macaulay2/flint2.git ??
# TODO: cflags: normal: -std=c90 -pedantic-errors +debug: -O0 -fno-unroll-loops
# TODO: confirm that building with mpir works
ExternalProject_Add(build-flint
  URL               ${M2_SOURCE_URL}/flint-2.5.2.tar.gz
  URL_HASH          SHA256=cbf1fe0034533c53c5c41761017065f85207a1b770483e98b2392315f6575e87
  PREFIX            libraries/flint
  SOURCE_DIR        libraries/flint/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND LIB_DIRS=${M2_HOST_DIR}/lib ./configure --prefix=${M2_HOST_DIR}
                      --with-blas # TODO: ${BLAS_INCLUDE_DIR} is empty
                      --disable-tls
                      --disable-shared
                      # --enable-assert
                      CFLAGS=${CPPFLAGS} # TODO: add CFLAGS
  BUILD_COMMAND     ${MAKE_EXE} -j
        COMMAND     ${MAKE_EXE} install
  INSTALL_COMMAND   ""
  )

set(factory_CPPFLAGS "${CPPFLAGS} -Dmpz_div_2exp=mpz_fdiv_q_2exp -Dmpz_div_ui=mpz_fdiv_q_ui -Dmpz_div=mpz_fdiv_q")
ExternalProject_Add(build-factory
  URL               ${M2_SOURCE_URL}/factory-4.1.1.tar.gz
  URL_HASH          SHA256=9dd84d11204e1457dac0a0d462a78d4cd4103c14cbf792b83d488aa529ad5724
  PREFIX            libraries/factory
  SOURCE_DIR        libraries/factory/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p0 < ${CMAKE_SOURCE_DIR}/libraries/factory/patch-4.1.1
  CONFIGURE_COMMAND cd factory-4.1.1 && autoreconf -vif
            COMMAND cd factory-4.1.1 && ./configure --prefix=${M2_HOST_DIR} --includedir=${M2_HOST_DIR}/include
                      # --with-ntl=${M2_HOST_DIR}/lib # TODO: only if needed
                      --with-flint=${M2_HOST_DIR}/lib # TODO: only if needed
                      --disable-omalloc
                      --enable-streamio
                      --disable-shared
                      --without-Singular
                      --cache-file=/dev/null
                      CPPFLAGS=${factory_CPPFLAGS}
                      # --enable-assertions
  BUILD_COMMAND     cd factory-4.1.1 && ${MAKE_EXE} -j1 all prefix=${M2_HOST_DIR} ftmpl_inst.o
                    AM_DEFAULT_VERBOSITY=1 'WARNFLAGS=-Wno-uninitialized -Wno-write-strings -Wno-deprecated'
        COMMAND     cd factory-4.1.1 &&
                    ./bin/makeheader factory.template     factory.h     && cp factory.h     include/factory/ &&
                    ./bin/makeheader factoryconf.template factoryconf.h && cp factoryconf.h include/factory/
        COMMAND     cd factory-4.1.1 && ${MAKE_EXE} -j1 prefix=${M2_HOST_DIR} all-recursive
        COMMAND     cd factory-4.1.1 && ${MAKE_EXE} install
        INSTALL_COMMAND   ""
  DEPENDS build-flint
  )
# TODO: remove this, since the one above has the tables at libraries/factory/build/factory-4.1.1/gftables/
ExternalProject_Add(gftables
  URL               ${M2_SOURCE_URL}/factory.4.0.1-gftables.tar.gz
  URL_HASH          SHA256=9cd158ceb1c2b1c47bdca2c0b004bba92cb0e0aaa0ea6a43ca784ebdce10eebd
  PREFIX            libraries/gftables
  SOURCE_DIR        ${M2_CORE_DIR}/factory/gftables
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
  )

# TODO: use git? https://github.com/Macaulay2/mpir.git 82816d99
ExternalProject_Add(build-mpir
  URL               ${M2_SOURCE_URL}/mpir-3.0.0.tar.bz2
  URL_HASH          SHA256=52f63459cf3f9478859de29e00357f004050ead70b45913f2c2269d9708675bb
  PREFIX            libraries/mpir
  SOURCE_DIR        libraries/mpir/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/mpir/patch-3.0.0
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_DIR}
                      --enable-gmpcompat
                      --enable-cxx
                      --disable-shared
                      --cache-file=/dev/null
                      CPPFLAGS=${CPPFLAGS}
                      # --enable-assert
  BUILD_COMMAND     ${MAKE_EXE} -j
        COMMAND     ${MAKE_EXE} install
  INSTALL_COMMAND   ""
  )

ExternalProject_Add(build-givaro
  URL               ${M2_SOURCE_URL}/givaro-4.0.3.tar.gz
  URL_HASH          SHA256=19101e41161db46a925a0d055cf530c6d731b0dcc79e69f4358e483778306d16
  PREFIX            libraries/givaro
  SOURCE_DIR        libraries/givaro/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ./configure --prefix=${M2_HOST_DIR}
                      --disable-shared
                      --disable-simd
                      CPPFLAGS=${CPPFLAGS}
  BUILD_COMMAND     ${MAKE_EXE}
        COMMAND     ${MAKE_EXE} install
  INSTALL_COMMAND   ""
  )

set(frobby_CFLAGS "-Wno-deprecated")
# FIXME: permissions on the installed files are wrong
ExternalProject_Add(build-frobby
  URL               ${M2_SOURCE_URL}/frobby_v0.9.0.tar.gz
  URL_HASH          SHA256=af092383e6dc849c86f4e79747ae0e5cd309a690747230e10aa38d60640062df
  PREFIX            libraries/frobby
  SOURCE_DIR        libraries/frobby/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/frobby/patch-0.9.0
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} library -j
                    prefix=${M2_HOST_DIR}
                    GMP_INC_DIR=${M2_HOST_DIR}/include
                    CPPFLAGS=${CPPFLAGS}
                    CFLAGS=${frobby_CFLAGS}
                    LDFLAGS=${LDFLAGS}
                    RANLIB=${CMAKE_RANLIB} # TODO: needed?
        COMMAND     /usr/bin/install -c -d ${M2_HOST_DIR}/lib &&
                    /usr/bin/install -c -d ${M2_HOST_DIR}/include &&
                    cp bin/libfrobby.a ${M2_HOST_DIR}/lib/libfrobby.a &&
                    cp src/frobby.h ${M2_HOST_DIR}/include/frobby.h &&
                    cp src/stdinc.h ${M2_HOST_DIR}/include/stdinc.h
  INSTALL_COMMAND   ""
  )

#################################################################################
## git submodules section

# TODO: do we actually need it built?
ExternalProject_Add(googletest
  GIT_REPOSITORY    https://github.com/google/googletest.git
  GIT_TAG           release-1.10.0 # 42bc671f
  PREFIX            libraries/googletest
  SOURCE_DIR        libraries/googletest/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ""
#  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_DIR} -DBUILD_GMOCK=OFF # -DINSTALL_GTEST=OFF
  )
set(GTEST_PATH  ${CMAKE_BINARY_DIR}/libraries/googletest/build/googletest) # ${M2_HOST_DIR}/include/gtest

## bdwgc
# Note: Starting with 8.0, libatomic_ops is not necessary for C11 or C++14.
# Currently cloning master for significant cmake support. Hopefully soon there will be a stable release
ExternalProject_Add(bdwgc
  GIT_REPOSITORY    https://github.com/ivmai/bdwgc.git
  GIT_TAG           master
  PREFIX            libraries/bdwgc
  SOURCE_DIR        libraries/bdwgc/build
  INSTALL_DIR       usr-host
  BUILD_IN_SOURCE   ON
# TODO: what is GC_LARGE_ALLOC_WARN_INTERVAL=1?
  CMAKE_ARGS        -Denable_cplusplus=ON -Denable_threads=ON -Denable_large_config=ON -Dbuild_cord=OFF
                    -Denable_throw_bad_alloc_library=OFF -Denable_gcj_support=OFF -Denable_java_finalization=OFF
                    # -Denable_gc_debug=ON -Denable_parallel_mark=OFF -Denable_gc_assertions=ON -Dbuild_tests=ON
                    # -DGC_ABORT_ON_LEAK
                    -DCMAKE_INSTALL_PREFIX=${M2_HOST_DIR}
  INSTALL_COMMAND   ""
  )

# TODO: PATCHFILE = libraries/fflas_ffpack/patch-2.2.2
ExternalProject_Add(build-fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            libraries/fflas_ffpack
  SOURCE_DIR        libraries/fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
                    COMMAND PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} ./configure --prefix=${M2_HOST_DIR}
                    AR=${CMAKE_AR} OBJDUMP=${CMAKE_OBJDUMP} # AS=${CMAKE_AS} DLLTOOL=${CMAKE_DLLTOOL}
                    STRIP=${CMAKE_STRIP} RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE_EXE}
        COMMAND     ${MAKE_EXE} install
  INSTALL_COMMAND   ""
  DEPENDS           build-givaro
  )

# TODO: would it be better to use FetchContent_Declare instead?
ExternalProject_Add(build-memtailor
  GIT_REPOSITORY    https://github.com/mahrud/memtailor.git
  GIT_TAG           4fb227410ca7e37baf4451a1540e349536dd3cde # original: e85453b
  PREFIX            libraries/memtailor
#  SOURCE_DIR        libraries/memtailor/src
  BINARY_DIR        libraries/memtailor/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_DIR} -DPACKAGE_TESTS=OFF
  # TODO: use this
  DEPENDS           googletest
  )

ExternalProject_Add(build-mathic
  GIT_REPOSITORY    https://github.com/mahrud/mathic.git
  GIT_TAG           0f473e0886e70099070daf4686f6b1e66c84f7a8 # original: 023afcf
  PREFIX            libraries/mathic
#  SOURCE_DIR        libraries/mathic/src
  BINARY_DIR        libraries/mathic/build
  CMAKE_ARGS        -DCMAKE_PREFIX_PATH=${M2_HOST_DIR} -DCMAKE_INSTALL_PREFIX=${M2_HOST_DIR}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake -DPACKAGE_TESTS=OFF
  DEPENDS           build-memtailor
  )

ExternalProject_Add(build-mathicgb
  GIT_REPOSITORY    https://github.com/mahrud/mathicgb.git
  GIT_TAG           9e7088de873d783c5a8c15482c68f14763bb8381 # original: bd634c8
  PREFIX            libraries/mathicgb
#  SOURCE_DIR        libraries/mathicgb/src
  BINARY_DIR        libraries/mathicgb/build
  CMAKE_ARGS        -DCMAKE_PREFIX_PATH=${M2_HOST_DIR} -DCMAKE_INSTALL_PREFIX=${M2_HOST_DIR}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake -DPACKAGE_TESTS=OFF
  DEPENDS           build-mathic
  )

###############################################################################
## STILL PROCESSING

# whether the package 4ti2 is installed
#4ti2-circuits 4ti2
#cohomcalg cohomcalg
#gfan gfan
# whether the package lrs is installed
#lrs lrslib
#csdp csdp
#normaliz normaliz
# whether the package nauty is installed
#nauty-complg nauty

#############################################################################

# These three lists reflect dependencies, with prerequisites listed first, including the following dependencies:
#    mathicgb needs mathic and memtailor
#    mathic needs memtailor
#    factory needs flint, ntl and gmp; it includes and installs gftables, so doesn't need it separately
#    libfac has been replaced by code in factory
#    lapack includes blas, makes both libblas and liblapack
#    mpir is a plug-in replacement for gmp and can provide libgmp and libgmpxx, too.
#    mpfr needs gmp (or mpir
#    	  mpfr puts pointers to gmp numbers in thread local variables, unless
# 	  specially configured, so we shouldn't tell gmp to use libgc (we used to do that)
#    mpc needs mpfr
#    ntl needs gmp (or mpir)
#    4ti2 needs glpk
#    glpk needs gmp (or mpir)
#    topcom depends on cddlib
#    gfan needs cddlib
#    cddlib uses gmp
#    polymake cannot be included in Macaulay2 because its compile/build/install procedure is flawed:
#	it uses a dynamic readline library that can only be provided by fink and probably will not be copied into the install location
#       it offers no way to provide a directory tree containing a readline library we've compiled
#	it writes a file into the user's home directory called ".polymake"
#	it asks questions
#    normaliz needs libgmp, libgmpxx, boost
#    Greg Smith requested cddplus and lrslib for future use
#    nauty is used by the package Nauty
#    normaliz is used by the package Normaliz
#    gfan is used by the packages gfanInterface and StatePolytope
#    4ti2 is used by the package FourTiTwo
#    linbox is provided as an option for experimentation
#    linbox needs fflas_ffpack and givaro
#    givaro uses gmp
#    fflas_ffpack needs givaro and lapack, but fflas_ffpack is just source code, so we don't *have* to build it
#    mpir is used by givaro
#    mpir and mpfr are used by fplll
#    Henry Duong is experimenting with mpack, a multi-precision version of blas+lapack based on mpfr
#    mpack depends on gmp (or mpir) mpfr mpc qd
#    flint depends on gmp (or mpir) and mpfr
#    frobby depends on gmp

# TOOD
message("## External library information:
     BUILDLIBLIST      = ${BUILDLIBLIST}
     BUILDSUBLIST      = ${BUILDSUBLIST}
     BUILDPROGLIST     = ${BUILDPROGLIST}
     BUILDLIST         = ${BUILDLIST}
     BUILD_ALWAYS      = ${BUILD_ALWAYS}")

# TODO
message("## Linker information:
     BUILTLIBS         = ${BUILTLIBS}
     LINALGLIBS        = ${LINALGLIBS}
     LIBS              = ${LIBS}")

#############################################################################



#AC_LANG(C)
#dnl topcom includes setoper.h, rather than cdd/setoper.h, so we do, too
#AC_CHECK_HEADER(setoper.h,,BUILD_cddlib=yes)
#if test $BUILD_cddlib = yes
#then BUILTLIBS="-lcdd $BUILTLIBS"
#else LIBS="-lcdd $LIBS"
#fi

#AC_LANG(C)
#AC_CHECK_HEADER(NTL/version.h,,BUILD_ntl=yes)
#if test $BUILD_ntl = yes
#then BUILTLIBS="-lntl $BUILTLIBS"
#else LIBS="-lntl $LIBS"
#fi
#AC_SUBST(BUILD_ntl)		dnl factory needs to know

##############################################################################

#PROGLIST=" $PROGLIST "
#AC_ARG_WITH(unbuilt-programs,
#    [AS_HELP_STRING(--with-unbuilt-programs=...,list of programs not to build from downloaded source code (e.g., $PROGLIST))],
#    [for i in $withval
#     do case $PROGLIST in
#	     *" $i "*) eval BUILD_$i=no ;;
#	     *) AC_MSG_ERROR(unrecognized program name: $i) ;;
#	esac
#     done])

#
## the order of these segments also reflects dependencies
#AC_LANG(C)
#AC_SEARCH_LIBS(tgoto,tinfo ncurses curses,,AC_MSG_ERROR([[not found: library containing symbol tgoto; tried libcurses, libncurses, and libtinfo)]]))
#if test $BUILD_readline = no
#then AC_CHECK_HEADER(readline/readline.h,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(rl_set_prompt,readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(rl_completion_matches,readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(readline,readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then AC_SEARCH_LIBS(add_history,history readline,,BUILD_readline=yes)
#fi
#if test $BUILD_readline = no
#then # readline on Mac OS X is stuck at version 4.2, which has this bug:
#     #   CTRL-A doesn't go all the way to the beginning of the
#     #   line after typing r e s o TAB C-a
#     # So we build it ourselves.
#     AC_LANG(C)
#     AC_MSG_CHECKING([whether readline library is new enough (version at least 6)])
#     AC_RUN_IFELSE([AC_LANG_SOURCE([[
#	     #include <stdio.h>
#	     #include <readline/readline.h>
#	     int main () { return ! ( RL_READLINE_VERSION >= 6 * 0x100 ) ; }]])],
#	[ AC_MSG_RESULT([yes]) ],
#	[ AC_MSG_RESULT([no, will build it]) ; BUILD_readline=yes ],
#	[ AC_MSG_RESULT([cross-compiling, test not possible]) ])
#fi
#if test $BUILD_readline = no
#then AC_CHECK_DECL(rl_catch_signals,,BUILD_readline=yes,[
#	#include <stdio.h>
#	#include <readline/readline.h>
#	])
#fi
#if test $BUILD_readline = yes
#then AC_MSG_NOTICE(readline library will be compiled)
#     BUILTLIBS="-lreadline -lhistory $BUILTLIBS"
#fi

#if test $BUILD_memtailor = no
#then AC_LANG(C++)
#     SAVE_CXXFLAGS="$CXXFLAGS"
#     CXXFLAGS="$CXXFLAGS"
#     AC_SEARCH_LIBS(MEMTAILOR_VERSION_STRING,memtailor,,BUILD_memtailor=yes)
#     AC_CHECK_HEADER(memtailor.h,,BUILD_memtailor=yes)
#     CXXFLAGS="$SAVE_CXXFLAGS"
#fi
#test $BUILD_memtailor = yes && BUILTLIBS="-lmemtailor $BUILTLIBS"

#if test $BUILD_mathic = no
#then AC_LANG(C++)
#     SAVE_CXXFLAGS="$CXXFLAGS"
#     CXXFLAGS="$CXXFLAGS"
#     AC_SEARCH_LIBS(MATHIC_VERSION_STRING,mathic,,BUILD_mathic=yes)
#     AC_CHECK_HEADER(mathic.h,,BUILD_mathic=yes)
#     CXXFLAGS="$SAVE_CXXFLAGS"
#fi
#test $BUILD_mathic = yes && BUILTLIBS="-lmathic $BUILTLIBS"

#if test $BUILD_mathicgb = no
#then AC_LANG(C++)
#     SAVE_CXXFLAGS="$CXXFLAGS"
#     CXXFLAGS="$CXXFLAGS"
#     AC_SEARCH_LIBS(MATHICGB_VERSION_STRING,mathicgb,,BUILD_mathicgb=yes)
#     AC_CHECK_HEADER(mathicgb.h,,BUILD_mathicgb=yes)
#     CXXFLAGS="$SAVE_CXXFLAGS"
#fi
#test $BUILD_mathicgb = yes && BUILTLIBS="-lmathicgb $BUILTLIBS"

#
#if test "$PYTHON" = yes
#then AC_LANG(C)
#     if test "$LIBPYTHON" = "$LIBPYTHONORIG"
#     then AC_SEARCH_LIBS(Py_Initialize,python2.7,,AC_MSG_ERROR(libpython2.7 not found))
#     else LIBS="$LIBPYTHON $LIBS"
#     fi
#     AC_CHECK_HEADER(python2.7/Python.h,,AC_MSG_ERROR(include file python2.7/Python.h not found))
#fi

#
#if test $BUILD_boost = no
#then AC_LANG(C++)
#     AC_CHECK_HEADER(boost/version.hpp,,BUILD_boost=yes)
#fi
