set(JOBS 4)

################################################################
## This target requires all external projects to be built and installed
add_custom_target(install-libraries ALL)
add_custom_target(install-programs ALL)

file(MAKE_DIRECTORY ${M2_HOST_PREFIX}/bin)

## FIXME: Hack to force CMake to reconfigure after library is reinstalled
file(TOUCH ${CMAKE_SOURCE_DIR}/cmake/check-libraries.cmake)

# TODO: add share/config.site to provide defaults for configure scripts

#################################################################################
## Setting a baseline for compile and link options for external projects

# TODO: is it okay to do this globally? Where is it better to do this?
add_compile_options(-I${M2_HOST_PREFIX}/include)
add_link_options(-L${M2_HOST_PREFIX}/lib)

get_property(COMPILE_OPTIONS DIRECTORY PROPERTY COMPILE_OPTIONS)
get_property(LINK_OPTIONS    DIRECTORY PROPERTY LINK_OPTIONS)

# TODO: configure option to disable building shared libraries are added individually
# is there a way to do all at once?
# --disable-shared --enable-static

## Preprocessor flags
string(REPLACE ";" " " CPPFLAGS "${COMPILE_OPTIONS}")

# NOTE: CMake does not easily support compiler dependent flags, so we put all preprocessor flags as compiler flags
## C compiler flags
set(CFLAGS   "${CPPFLAGS} -w -Wimplicit -Werror")

## C++ compiler flags
set(CXXFLAGS "${CXXFLAGS} -std=gnu++11 -Wno-mismatched-tags -w -Wno-deprecated-register")

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

# TODO:
# ntl       b90b36c9dd8954c9bc54410b1d57c00be956ae1db5a062945822bbd7a86ab4d2 ntl-10.5.0.tar.gz
# topcom    3f83b98f51ee859ec321bacabf7b172c25884f14848ab6c628326b987bd8aaab TOPCOM-0.17.8.tar.gz
#
# 4ti2 needs glpk, but maybe FindGLPK will suffice for all three
# glpk      e398be2e7cb8a98584325268704729872558a4a88555bc8a54139d017eb9ebae glpk-4.59.tar.gz
# mpc       6985c538143c1208dcb1ac42cedad6ff52e267b47e5f970183a3e75125b43c2e mpc-1.1.0.tar.gz
# mpfr      1d3be708604eae0e42d578ba93b390c2a145f17743a744d8f3f8c2ad5855a38a mpfr-4.0.2.tar.xz

include(ExternalProject) # populate at build time; FetchContent populates at configure time
set(M2_SOURCE_URL https://faculty.math.illinois.edu/Macaulay2/Downloads/OtherSourceCode)

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
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS}
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(USING_MPI AND NOT MPIR_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-mpir-install)
endif()


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
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS}
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT FLINT_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-flint-install)
endif()


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
  BUILD_COMMAND     cd factory-4.1.1 && ${MAKE_EXE} -j${JOBS} all prefix=${M2_HOST_PREFIX} ftmpl_inst.o
                    AM_DEFAULT_VERBOSITY=1 'WARNFLAGS=-Wno-uninitialized -Wno-write-strings -Wno-deprecated'
        COMMAND     cd factory-4.1.1 &&
                    ./bin/makeheader factory.template     factory.h     && cp factory.h     include/factory/ &&
                    ./bin/makeheader factoryconf.template factoryconf.h && cp factoryconf.h include/factory/
        COMMAND     cd factory-4.1.1 && ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX} all-recursive
  INSTALL_COMMAND   cd factory-4.1.1 && ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT FACTORY_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-factory-install)
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
  BUILD_COMMAND     ${MAKE_EXE} library -j${JOBS} prefix=${M2_HOST_PREFIX}
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
if(NOT FROBBY_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-frobby-install)
endif()


# https://www.inf.ethz.ch/personal/fukudak/cdd_home/
set(cddlib_SUBDIRS "lib-src lib-src-gmp")
ExternalProject_Add(build-cddlib
  URL               ${M2_SOURCE_URL}/cddlib-094h.tar.gz
  URL_HASH          SHA256=fe6d04d494683cd451be5f6fe785e147f24e8ce3ef7387f048e739ceb4565ab5
  PREFIX            libraries/cddlib
  SOURCE_DIR        libraries/cddlib/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
		      --includedir=${M2_HOST_PREFIX}/include/cdd
                      --disable-shared
                      SUBDIRS=${cddlib_SUBDIRS}
		      gmpdir=/nowhere
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
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX}
                      SUBDIRS=${cddlib_SUBDIRS}
		      gmpdir=/nowhere
  INSTALL_COMMAND   ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX} install
                      SUBDIRS=${cddlib_SUBDIRS}
		      gmpdir=/nowhere
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT CDD_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-cddlib-install)
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
if(NOT BDW_GC_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-bdwgc-install)
endif()


# TODO: out of source build has issues with detecting SIMD instructions
ExternalProject_Add(build-givaro
  GIT_REPOSITORY    https://github.com/linbox-team/givaro.git
  GIT_TAG           v4.0.3
  PREFIX            libraries/givaro
  SOURCE_DIR        libraries/givaro/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      --disable-shared
                      # --disable-simd # unrecognized option on 4.0.3?
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
if(NOT GIVARO_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-givaro-install)
endif()


# Note: fflas_ffpack is just header files, so we don't build it
# TODO: make autotune produces fflas-ffpack/fflas-ffpack-thresholds.h which could be useful
# TODO: try configuring with mkl blas (first replace s/CBLAS/BLAS/g in macros/mkl-check.m4 and macros/fflas-ffpack-blas.m4)
# Result: run into issues with e/lapack.h redeclaring cblas functions
#set(MKL_LIBS   "-L/opt/intel/mkl/lib/intel64_lin -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")
#set(MKL_CFLAGS "-I/opt/intel/mkl/include")
ExternalProject_Add(build-fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            libraries/fflas_ffpack
  SOURCE_DIR        libraries/fflas_ffpack/build
#  BINARY_DIR        libraries/fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} ./configure --prefix=${M2_HOST_PREFIX}
	              # --enable-openmp
		      # --with-blas-libs=${MKL_LIBS}
		      # --with-blas-cflags=${MKL_CFLAGS}
		      # --enable-precompilation # build errors
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
  BUILD_COMMAND     "" # we only use the fflas_ffpack header files, so no need to build it
#  BUILD_COMMAND     ${MAKE_EXE}
#        COMMAND     ${MAKE_EXE} autotune
  INSTALL_COMMAND   ${MAKE_EXE} install-data # only install headers and fflas-ffpack.pc
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT FFLAS_FFPACK_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-fflas_ffpack-install)
  if(NOT GIVARO_FOUND)
    # TODO: also add gmp/mpir?
    ExternalProject_Add_StepDependencies(build-fflas_ffpack build build-givaro-install) # lol
  endif()
endif()


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
if(NOT MEMTAILOR_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-memtailor-install)
endif()


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
if(NOT MATHIC_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-mathic-install)
  if(NOT MEMTAILOR_FOUND)
    ExternalProject_Add_StepDependencies(build-mathic build build-memtailor-install)
  endif()
endif()


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
if(NOT MATHICGB_FOUND)
  # Add this to the libraries target
  add_dependencies(install-libraries build-mathicgb-install)
  if(NOT MATHIC_FOUND)
    ExternalProject_Add_StepDependencies(build-mathicgb build build-mathic-install)
  endif()
endif()

###############################################################################
## Build required programs

# Everything possible:
# LIBLIST gc gdbm mpir mpfr ntl flint factory lapack frobby glpk cddlib fplll givaro linbox boost mpc qd mpack gtest
# PROGLIST 4ti2 gfan normaliz csdp nauty cddplus lrslib gftables topcom cohomcalg
# SUBLIST memtailor mathic mathicgb fflas_ffpack
# FIXME: gmp libtool pari are Missing below
# gfan normaliz csdp nauty cddplus lrslib topcom cohomcalg)

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
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS}
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
if(NOT 4TI2)
  # Add this to the programs target
  add_dependencies(install-programs build-4ti2-install)
endif()


# https://github.com/BenjaminJurke/cohomCalg/
# Warning: this no longer compiles with gcc version 4.8.5, and it doesn't help with
#   https://github.com/Macaulay2/M2/issues/977, so we may want to go back to the old version.
ExternalProject_Add(build-cohomcalg
  URL               ${M2_SOURCE_URL}/cohomCalg-0.32.tar.gz
  URL_HASH          SHA256=367c52b99c0b0a4794b215181439bf54abe4998872d3ef25d793bc13c4d40e42
  PREFIX            libraries/cohomcalg
  SOURCE_DIR        libraries/cohomcalg/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      LD=${CMAKE_CXX_COMPILER} # correct?
        COMMAND     ${CMAKE_STRIP} bin/cohomcalg
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different bin/cohomcalg ${M2_HOST_PREFIX}/bin
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT COHOMCALG)
  # Add this to the programs target
  add_dependencies(install-programs build-cohomcalg-install)
endif()


set(gfan_CC  "${CC}  ${CPPFLAGS}")
set(gfan_CXX "${CXX} ${CPPFLAGS}")
set(gfan_CLINKER  "${CC}  ${LDFLAGS}")
set(gfan_CCLINKER "${CXX} ${LDFLAGS}")
ExternalProject_Add(build-gfan
  URL               ${M2_SOURCE_URL}/gfan0.6.2.tar.gz
  URL_HASH          SHA256=a674d5e5dc43634397de0d55dd5da3c32bd358d05f72b73a50e62c1a1686f10a
  PREFIX            libraries/gfan
  SOURCE_DIR        libraries/gfan/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/gfan/patch-0.6.2
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX}
                      CC=${gfan_CC}
		      CXX=${gfan_CXX}
		      CLINKER=${gfan_CLINKER}
		      CCLINKER=${gfan_CCLINKER}
		      PREFIX=/nowhere
		      CDD_LINKOPTIONS=-lcddgmp
        COMMAND     ${CMAKE_STRIP} gfan${CMAKE_EXECUTABLE_SUFFIX}
  INSTALL_COMMAND   ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX} install
                      PREFIX=${M2_HOST_PREFIX}
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT GFAN)
  # Add this to the programs target
  add_dependencies(install-programs build-gfan-install)
endif()


# TODO: library or program?
# http://www-cgrl.cs.mcgill.ca/~avis/C/lrs.html
ExternalProject_Add(build-lrslib
  URL               ${M2_SOURCE_URL}/lrslib-062.tar.gz
  URL_HASH          SHA256=adf92f9c7e70c001340b9c28f414208d49c581df46b550f56ab9a360348e4f09
  PREFIX            libraries/lrslib
  SOURCE_DIR        libraries/lrslib/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX} lrs
                      LIBDIR=${M2_HOST_PREFIX}/lib
                      CPPFLAGS=${CPPFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
		      # TODO: TARGET_ARCH= RANLIB=true
        COMMAND     ${CMAKE_STRIP} lrs${CMAKE_EXECUTABLE_SUFFIX}
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different lrs ${M2_HOST_PREFIX}/bin
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT LRSLIB)
  # Add this to the programs target
  add_dependencies(install-programs build-lrslib-install)
endif()


set(csdp_CC  "${CC}  ${OpenMP_C_FLAGS}")
set(csdp_CXX "${CXX} ${OpenMP_CXX_FLAGS}")
set(csdp_LDFLAGS "${LDFLAGS} ${OpenMP_CXX_FLAGS}")
list(JOIN OpenMP_CXX_LIBRARIES " " csdp_OpenMP_CXX_LIBRARIES)
set(csdp_LDLIBS  "${LDLIBS}  ${csdp_OpenMP_CXX_LIBRARIES}")
list(JOIN LAPACK_LIBRARIES " " csdp_LAPACK_LIBRARIES)
set(csdp_LIBS     "-L../lib -lsdp ${csdp_LAPACK_LIBRARIES} -lm")
ExternalProject_Add(build-csdp
  URL               http://www.coin-or.org/download/source/Csdp/Csdp-6.2.0.tgz # TODO
  URL_HASH          SHA256=7f202a15f33483ee205dcfbd0573fdbd74911604bb739a04f8baa35f8a055c5b
  PREFIX            libraries/csdp
  SOURCE_DIR        libraries/csdp/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/csdp/patch-6.2.0
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX}
                      CC=${csdp_CC}
                      CXX=${csdp_CXX}
                      LDFLAGS=${csdp_LDFLAGS}
                      LDLIBS=${csdp_LDLIBS}
                      LIBS=${csdp_LIBS}
        COMMAND     ${CMAKE_STRIP} 
                      solver/csdp
                      theta/complement
		      theta/graphtoprob
		      theta/rand_graph
		      theta/theta
  INSTALL_COMMAND   ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT CSDP)
  # Add this to the programs target
  add_dependencies(install-programs build-csdp-install)
endif()


# TODO: see special variables OPENMP and NORMFLAGS for macOS from libraries/normaliz/Makefile.in
set(normaliz_CXXFLAGS "${CPPFLAGS} -Wall -O3 -Wno-unknown-pragmas -std=c++11 -I .. -I . ${OpenMP_CXX_FLAGS}")
set(normaliz_GMPFLAGS "${LDFLAGS} -lgmpxx -lgmp") # TODO: what about mpir?
ExternalProject_Add(build-normaliz
  URL               ${M2_SOURCE_URL}/normaliz-3.7.2.tar.gz
  URL_HASH          SHA256=436a870a1ab9a5e0c2330f5900d904dc460938c17428db1c729318dbd9bf27aa
  PREFIX            libraries/normaliz
  SOURCE_DIR        libraries/normaliz/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      --disable-shared
                      --cache-file=/dev/null
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
		      # OPENMP=
		      # NORMFLAGS=
		      # TARGET_ARCH=
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS}
                      CXX=${CMAKE_CXX_COMPILER}
		      # NORMFLAGS=
                      CXXFLAGS=${normaliz_CXXFLAGS}
                      RANLIB=${CMAKE_RANLIB}
		      GMPFLAGS=${normaliz_GMPFLAGS}
        COMMAND     ${CMAKE_STRIP} source/normaliz
  # TODO: do we need the libraries as well, or just the binary?
  INSTALL_COMMAND   ${MAKE_EXE} install
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT NORMALIZ)
  # Add this to the programs target
  add_dependencies(install-programs build-normaliz-install)
endif()


# URL = http://cs.anu.edu.au/~bdm/nauty
set(nauty_PROGRAMS
   NRswitchg addedgeg amtog biplabg catg complg copyg countg deledgeg
   directg dreadnaut dretog genbg geng genrang gentourng labelg listg
   multig newedgeg pickg planarg shortg showg)
set(nauty_STRIPFILES
   NRswitchg addedgeg amtog biplabg catg complg copyg countg deledgeg directg
   dreadnaut dretog genbg geng genrang gentourng labelg linegraphg listg multig
   newedgeg pickg planarg ranlabg shortg showg subdivideg watercluster2)
set(nauty_CHECKERS dreadtest dreadtestS dreadtestS1 dreadtest4K dreadtest1 dreadtestW1 dreadtestL1 dreadtestL)
ExternalProject_Add(build-nauty
  URL               ${M2_SOURCE_URL}/nauty27b11.tar.gz
  URL_HASH          SHA256=5d52211cec767d8d8e43483d96202be235f85696d1373c307291273463c812fa
  PREFIX            libraries/nauty
  SOURCE_DIR        libraries/nauty/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      --cache-file=/dev/null
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
  BUILD_COMMAND     ${MAKE_EXE} -j${JOBS} prefix=${M2_HOST_PREFIX}
        COMMAND     ${CMAKE_STRIP} ${nauty_STRIPFILES}
  # TODO: put nauty programs in a folder?
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${nauty_PROGRAMS} ${M2_HOST_PREFIX}/bin
  TEST_COMMAND      rm -f ${nauty_CHECKERS}
       COMMAND      ${MAKE_EXE} BIGTEST=0 checks
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT NAUTY)
  # Add this to the programs target
  add_dependencies(install-programs build-nauty-install)
endif()

install(DIRECTORY
  ${M2_HOST_PREFIX}/bin
  DESTINATION ${CMAKE_INSTALL_LIBEXECDIR}/Macaulay2
  USE_SOURCE_PERMISSIONS)


#############################################################################

get_target_property(LIBRARY_DEPENDENCIES install-libraries MANUALLY_ADDED_DEPENDENCIES)
get_target_property(PROGRAM_DEPENDENCIES install-programs  MANUALLY_ADDED_DEPENDENCIES)
if(NOT LIBRARY_DEPENDENCIES)
  set(LIBRARY_DEPENDENCIES N/A)
endif()
if(NOT PROGRAM_DEPENDENCIES)
  set(PROGRAM_DEPENDENCIES N/A)
endif()
string(REGEX REPLACE "(build-|-install)" "" BUILD_LIB_LIST  "${LIBRARY_DEPENDENCIES}")
string(REGEX REPLACE "(build-|-install)" "" BUILD_PROG_LIST "${PROGRAM_DEPENDENCIES}")

message("## External components that will be built:
     BUILDLIBLIST      = ${BUILD_LIB_LIST}
     BUILDPROGLIST     = ${BUILD_PROG_LIST}")
# TOOD: BUILDLIST BUILDSUBLIST BUILD_ALWAYS

# TODO: how to keep track of things we've built?
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
