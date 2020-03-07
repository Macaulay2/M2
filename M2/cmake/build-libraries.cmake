################################################################
## This target requires all external projects to be built and installed
add_custom_target(install-libraries ALL)

## Hack to force CMake to reconfigure after library is reinstalled
file(TOUCH ${CMAKE_SOURCE_DIR}/cmake/check-libraries.cmake)

#################################################################################
## Setting a baseline for compile and link options for external projects
get_property(COMPILE_OPTIONS DIRECTORY PROPERTY COMPILE_OPTIONS)
get_property(LINK_OPTIONS    DIRECTORY PROPERTY LINK_OPTIONS)

# TODO: add configure option to disable building shared libraries
# --disable-shared --enable-static

## Preprocessor flags
string(REPLACE ";" " " CPPFLAGS "${COMPILE_OPTIONS}")

# NOTE: CMake does not easily support compiler dependent flags, so we put all preprocessor flags as compiler flags
## C compiler flags
set(CFLAGS   "${CPPFLAGS} -w -Wimplicit -Werror")

## C++ compiler flags
set(CXXFLAGS "${CPPFLAGS} -Wno-mismatched-tags -w -Wno-deprecated-register")

## Linker flags
string(REPLACE ";" " " LDFLAGS "${LINK_OPTIONS}")

## Toolchain flags
# TODO: Are these necessary to define?
# TODO: LIBS=??? LIB_DIRS=??
# Possible assembly dialects are "", "_NASM", "_MASM", "-ATT"
# AS=${CMAKE_ASM${DIALECT}_COMPILER}
# DLLTOOL=??? (use find_program if needed)

#################################################################################
## Build required libraries, first those downloaded as a tarfile

include(ExternalProject) # populate at build time; FetchContent populates at configure time
find_program(MAKE_EXE NAMES make gmake nmake)
set(M2_SOURCE_URL https://faculty.math.illinois.edu/Macaulay2/Downloads/OtherSourceCode)

if(USING_MPI AND NOT MPIR_FOUND)
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
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      --enable-gmpcompat
                      --enable-cxx
                      --disable-shared
                      --cache-file=/dev/null
                      # --enable-assert
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE_EXE} -j4
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-mpir-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
endif()

if(NOT FLINT_FOUND)
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
  CONFIGURE_COMMAND LIB_DIRS=${M2_HOST_PREFIX}/lib ./configure --prefix=${M2_HOST_PREFIX}
                      --with-blas # TODO: ${BLAS_INCLUDE_DIR} is empty
#                      --with-gmp=${M2_HOST_PREFIX}
#                      --with-mpir=${M2_HOST_PREFIX}
#                      --with-mpfr=${M2_HOST_PREFIX}
#                      --with-ntl=${M2_HOST_PREFIX}
                      --enable-cxx
                      --disable-tls
                      --disable-shared
                      # --enable-assert
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE_EXE} -j4
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-flint-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
endif()

if(NOT FACTORY_FOUND)
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
            COMMAND cd factory-4.1.1 && ./configure --prefix=${M2_HOST_PREFIX} --includedir=${M2_HOST_PREFIX}/include
                      --disable-omalloc
                      --enable-streamio
                      --disable-shared
                      --without-Singular
                      --cache-file=/dev/null
                      # --with-ntl=${M2_HOST_PREFIX} # TODO: only if needed
                      # --with-flint=${M2_HOST_PREFIX} # TODO: only if needed
                      # --enable-assertions
                      CPPFLAGS=${factory_CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     cd factory-4.1.1 && ${MAKE_EXE} -j1 all prefix=${M2_HOST_PREFIX} ftmpl_inst.o
                    AM_DEFAULT_VERBOSITY=1 'WARNFLAGS=-Wno-uninitialized -Wno-write-strings -Wno-deprecated'
        COMMAND     cd factory-4.1.1 &&
                    ./bin/makeheader factory.template     factory.h     && cp factory.h     include/factory/ &&
                    ./bin/makeheader factoryconf.template factoryconf.h && cp factoryconf.h include/factory/
        COMMAND     cd factory-4.1.1 && ${MAKE_EXE} -j1 prefix=${M2_HOST_PREFIX} all-recursive
  INSTALL_COMMAND   cd factory-4.1.1 && ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-factory-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
# TODO: repeat pkg_search_module(FACTORY      factory singular-factory IMPORTED_TARGET)
if(NOT FLINT_FOUND)
  # TODO: also add mpfr, ntl, gmp/mpir?
  ExternalProject_Add_StepDependencies(build-factory build build-flint-install) # lol
endif()
endif()
# TODO: remove this, since the one above has the tables at libraries/factory/build/factory-4.1.1/gftables/
ExternalProject_Add(extract-gftables
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

if(NOT GIVARO_FOUND)
ExternalProject_Add(build-givaro
  URL               ${M2_SOURCE_URL}/givaro-4.0.3.tar.gz
  URL_HASH          SHA256=19101e41161db46a925a0d055cf530c6d731b0dcc79e69f4358e483778306d16
  PREFIX            libraries/givaro
  SOURCE_DIR        libraries/givaro/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      --disable-shared
                      --disable-simd
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE_EXE}
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-givaro-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
endif()

if(NOT FROBBY_FOUND)
set(frobby_CXXFLAGS "${CPPFLAGS} ${CXXFLAGS} -Wno-deprecated-declarations")
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
  BUILD_COMMAND     ${MAKE_EXE} library -j4 prefix=${M2_HOST_PREFIX}
                      GMP_INC_DIR=${M2_HOST_PREFIX}/include
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${frobby_CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  INSTALL_COMMAND   /usr/bin/install -c -d ${M2_HOST_PREFIX}/lib &&
                    /usr/bin/install -c -d ${M2_HOST_PREFIX}/include &&
                    cp bin/libfrobby.a ${M2_HOST_PREFIX}/lib/libfrobby.a &&
                    cp src/frobby.h ${M2_HOST_PREFIX}/include/frobby.h &&
                    cp src/stdinc.h ${M2_HOST_PREFIX}/include/stdinc.h # FIXME
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-frobby-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
endif()

#################################################################################
## Packages downloaded via git

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
#  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX} -DBUILD_GMOCK=OFF # -DINSTALL_GTEST=OFF
  EXCLUDE_FROM_ALL  ON
  )
set(GTEST_PATH  ${CMAKE_BINARY_DIR}/libraries/googletest/build/googletest) # ${M2_HOST_PREFIX}/include/gtest

if(NOT BDWGC_FOUND)
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
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -Denable_cplusplus=ON
                    -Denable_threads=ON
                    -Denable_large_config=ON
                    -Dbuild_cord=OFF
                    -Denable_throw_bad_alloc_library=OFF
                    -Denable_gcj_support=OFF
                    -Denable_java_finalization=OFF
                    # -Denable_gc_debug=ON
                    # -Denable_parallel_mark=OFF
                    # -Denable_gc_assertions=ON
                    # -Dbuild_tests=ON
                    # -DGC_ABORT_ON_LEAK
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-bdwgc-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
endif()

if(NOT FFLAS_FFPACK_FOUND)
# TODO: fflas_ffpack is just source code, so we don't *have* to build it
# TODO: PATCHFILE = libraries/fflas_ffpack/patch-2.2.2
ExternalProject_Add(build-fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            libraries/fflas_ffpack
  SOURCE_DIR        libraries/fflas_ffpack/build
#  BINARY_DIR        libraries/fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} ./configure --prefix=${M2_HOST_PREFIX}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE_EXE}
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-fflas_ffpack-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
if(NOT GIVARO_FOUND)
  # TODO: also add gmp/mpir?
  ExternalProject_Add_StepDependencies(build-fflas_ffpack build build-givaro-install) # lol
endif()
endif()

if(NOT MEMTAILOR_FOUND)
# TODO: would it be better to use FetchContent_Declare instead?
ExternalProject_Add(build-memtailor
  GIT_REPOSITORY    https://github.com/mahrud/memtailor.git
  GIT_TAG           af4a81f57fb585a541f5fefb517f2ad91b38cbe9 # original: e85453b
  PREFIX            libraries/memtailor
  BINARY_DIR        libraries/memtailor/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DPACKAGE_TESTS=OFF
#  DEPENDS           googletest # TODO: use this
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-memtailor-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
endif()

if(NOT MATHIC_FOUND)
ExternalProject_Add(build-mathic
  GIT_REPOSITORY    https://github.com/mahrud/mathic.git
  GIT_TAG           770fe83edb4edae061af613328bbe2d380ea26d6 # original: 023afcf
  PREFIX            libraries/mathic
  BINARY_DIR        libraries/mathic/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DPACKAGE_TESTS=OFF
  DEPENDS           build-memtailor
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-mathic-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
if(NOT MEMTAILOR_FOUND)
  ExternalProject_Add_StepDependencies(build-mathic build build-memtailor-install)
endif()
endif()

if(NOT MATHICGB_FOUND)
ExternalProject_Add(build-mathicgb
  GIT_REPOSITORY    https://github.com/mahrud/mathicgb.git
  GIT_TAG           557d3da746c6888ef05f29c2f095f0262080956d # original: bd634c8
  PREFIX            libraries/mathicgb
#  SOURCE_DIR        libraries/mathicgb/src
  BINARY_DIR        libraries/mathicgb/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DPACKAGE_TESTS=OFF
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
# Add this to the libraries target
add_dependencies(install-libraries build-mathicgb-install)
# Force cmake to rebuild makefiles when the stamp is updates
file(TOUCH ${CMAKE_BINARY_DIR}/libraries/stamp)
if(NOT MATHIC_FOUND)
  ExternalProject_Add_StepDependencies(build-mathicgb build build-mathic-install)
endif()
endif()

###############################################################################
## Build required programs

set(TARFILES
  # My machine download these:
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

# whether the package 4ti2 is installed
#4ti2-circuits 4ti2
if(NOT 4TI2_FOUND)
ExternalProject_Add(build-4ti2
  URL               ${M2_SOURCE_URL}/4ti2-1.6.9.tar.gz
  URL_HASH          SHA256=3053e7467b5585ad852f6a56e78e28352653943e7249ad5e5174d4744d174966
  PREFIX            libraries/4ti2
  SOURCE_DIR        libraries/4ti2/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      --with-gmp=no # TODO: does this line work?
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE_EXE} -j4
        COMMAND     ${CMAKE_STRIP}
                      src/groebner/4ti2gmp${CMAKE_EXECUTABLE_SUFFIX}
                      src/groebner/4ti2int32${CMAKE_EXECUTABLE_SUFFIX}
                      src/groebner/4ti2int64${CMAKE_EXECUTABLE_SUFFIX}
                      src/util/genmodel
                      src/util/gensymm
                      src/ppi/ppi
                      src/util/output
                      src/zsolve/zsolve
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
set(4TI2_FOUND 1)
endif()

#cohomcalg cohomcalg
#gfan gfan
# whether the package lrs is installed
#lrs lrslib
#csdp csdp
#normaliz normaliz
# whether the package nauty is installed
#nauty-complg nauty

#PROGLIST=" $PROGLIST "
#AC_ARG_WITH(unbuilt-programs,
#    [AS_HELP_STRING(--with-unbuilt-programs=...,list of programs not to build from downloaded source code (e.g., $PROGLIST))],
#    [for i in $withval
#     do case $PROGLIST in
#	     *" $i "*) eval BUILD_$i=no ;;
#	     *) AC_MSG_ERROR(unrecognized program name: $i) ;;
#	esac
#     done])

#############################################################################

get_target_property(LIBRARY_DEPENDENCIES install-libraries MANUALLY_ADDED_DEPENDENCIES)
string(REGEX REPLACE "(build-|-install)" "" BUILD_LIB_LIST "${LIBRARY_DEPENDENCIES}")

# TOOD
#     BUILDLIST         = ${BUILDLIST}
#     BUILDSUBLIST      = ${BUILDSUBLIST}
#     BUILDPROGLIST     = ${BUILDPROGLIST}
#     BUILD_ALWAYS      = ${BUILD_ALWAYS}
message("## External library information:
     BUILDLIBLIST      = ${BUILD_LIB_LIST}")

# TODO
#message("## Linker information:
#     BUILTLIBS         = ${BUILTLIBS}
#     LINALGLIBS        = ${LINALGLIBS}
#     LIBS              = ${LIBS}")

#############################################################################

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

#if test "$PYTHON" = yes
#then AC_LANG(C)
#     if test "$LIBPYTHON" = "$LIBPYTHONORIG"
#     then AC_SEARCH_LIBS(Py_Initialize,python2.7,,AC_MSG_ERROR(libpython2.7 not found))
#     else LIBS="$LIBPYTHON $LIBS"
#     fi
#     AC_CHECK_HEADER(python2.7/Python.h,,AC_MSG_ERROR(include file python2.7/Python.h not found))
#fi

#if test $BUILD_boost = no
#then AC_LANG(C++)
#     AC_CHECK_HEADER(boost/version.hpp,,BUILD_boost=yes)
#fi
