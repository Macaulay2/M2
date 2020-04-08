# TODO: git clones can be heavy; switch to submodules or downloading tarfiles from github.
if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git")
  ## Update submodules as needed
  if(GIT_SUBMODULE)
    message(STATUS "Submodule update")
    execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      RESULT_VARIABLE GIT_SUBMOD_RESULT)
    if(NOT GIT_SUBMOD_RESULT EQUAL "0")
      message(FATAL_ERROR "git submodule update --init failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
    endif()
  endif()
endif()

################################################################
## This target builds external libraries that M2 relies on.
add_custom_target(build-libraries
  COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_SOURCE_DIR}/cmake/check-libraries.cmake)

## This target builds external programs that are distributed with M2.
add_custom_target(build-programs)

file(MAKE_DIRECTORY ${M2_HOST_PREFIX}/bin)
file(MAKE_DIRECTORY ${M2_INSTALL_PROGRAMSDIR}/bin)

## This target forces libraries and programs to run their configure and install targets
add_custom_target(clean-stamps
  COMMAND rm libraries/*/src/build-*-stamp/*-{configure,install})

# TODO: Accumulate information in usr-host/share/config.site to speed up configuration
# See: https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Cache-Files.html
set(CONFIGURE_CACHE ${M2_HOST_PREFIX}/share/config.site)

# We wrap some configure commands in this so they find mpir, mpfr, etc.
if(APPLE)
  set(SET_LD_LIBRARY_PATH DYLD_LIBRARY_PATH=${M2_HOST_PREFIX}/lib)
elseif(UNIX)
  set(SET_LD_LIBRARY_PATH   LD_LIBRARY_PATH=${M2_HOST_PREFIX}/lib)
endif()

#################################################################################
## Setting a baseline for compile and link options for external projects

get_property(COMPILE_OPTIONS DIRECTORY PROPERTY COMPILE_OPTIONS)
get_property(LINK_OPTIONS    DIRECTORY PROPERTY LINK_OPTIONS)

if(BUILD_SHARED_LIBS)
  set(shared_setting --enable-shared)
else()
  set(shared_setting --disable-shared)
endif()

## Preprocessor flags
string(REPLACE ";" " " CPPFLAGS "${CPPFLAGS} ${COMPILE_OPTIONS}")

# NOTE: CMake does not easily support compiler dependent flags,
# so we put all preprocessor flags as compiler flags. Also, since
# some components are old, we only require C++11 standard here.
## C compiler flags
set(CFLAGS   "${CPPFLAGS} -std=gnu11 -w -Wimplicit -Werror ${CFLAGS}")

## C++ compiler flags
set(CXXFLAGS "${CPPFLAGS} -std=gnu++11 -w -Wno-mismatched-tags -Wno-deprecated-register ${CXXFLAGS}")

## Linker flags
string(REPLACE ";" " " LDFLAGS "${LINK_OPTIONS} ${LDFLAGS}")

## Toolchain flags
# Possible assembly dialects are "", "_NASM", "_MASM", "-ATT"
# AS=${CMAKE_ASM${DIALECT}_COMPILER}

## Linear algebra library flags
# TODO: add option for Intel MKL (first replace s/CBLAS/BLAS/g in macros/mkl-check.m4 and macros/fflas-ffpack-blas.m4)
# Result: run into issues with e/lapack.h redeclaring cblas functions
#set(MKL_LIBS   "-L/opt/intel/mkl/lib/intel64_lin -lmkl_intel_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl")
#set(MKL_CFLAGS "-I/opt/intel/mkl/include")
if(APPLE)
  set(LA_LIBRARIES "-framework Accelerate")
else()
  list(JOIN LAPACK_LIBRARIES " " LA_LIBRARIES)
endif()

if(VERBOSE)
  message("## Library compile options:
     CFLAGS            = ${CFLAGS}
     CXXFLAGS          = ${CXXFLAGS}
     LDFLAGS           = ${LDFLAGS}")
endif()

#################################################################################
## Build required libraries, first those downloaded as a tarfile

include(ExternalProject) # configure, patch, build, and install at build time
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
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --enable-gmpcompat
                      --enable-cxx
                      --with-pic
                      --enable-shared # ${shared_setting} # TODO: get static mpir linking to work
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
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE_EXE} -j${PARALLEL_JOBS} install
  TEST_COMMAND      ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT MP_FOUND)
  if(MP_LIBRARY STREQUAL GMP)
    # gmp is a prerequisite
    message(FATAL "gmp integer package specified, but not found")
  elseif(MP_LIBRARY STREQUAL MPIR)
    # Add this to the libraries target
    add_dependencies(build-libraries build-mpir-install)
  endif()
endif()
# Making sure flint can find the gmp.h->mpir.h symlink
if(MPIR_FOUND AND NOT EXISTS ${M2_HOST_PREFIX}/include/gmp.h)
  configure_file(${MP_INCLUDE_DIRS}/mpir.h   ${M2_HOST_PREFIX}/include/gmp.h   COPYONLY)
  configure_file(${MP_INCLUDE_DIRS}/mpirxx.h ${M2_HOST_PREFIX}/include/gmpxx.h COPYONLY)
endif()


# TODO: Is this still relevant?
# mpfr puts pointers to gmp numbers in thread local variables, unless
# specially configured, so we shouldn't tell gmp to use libgc (we used to do that)
ExternalProject_Add(build-mpfr
  URL               ${M2_SOURCE_URL}/mpfr-4.0.2.tar.xz
  URL_HASH          SHA256=1d3be708604eae0e42d578ba93b390c2a145f17743a744d8f3f8c2ad5855a38a
  PREFIX            libraries/mpfr
  SOURCE_DIR        libraries/mpfr/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${SET_LD_LIBRARY_PATH} ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --with-gmp=${M2_HOST_PREFIX}
                      --disable-thread-safe
		      ${shared_setting}
                      --with-pic
                      # --enable-assert
                      # TARGET_ARCH=
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
  BUILD_COMMAND     ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} CPPFLAGS=${CPPFLAGS}
  INSTALL_COMMAND   ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} install
  TEST_COMMAND      ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT MPFR_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-mpfr-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-mpfr configure build-mpir-install)
  endif()
endif()


# http://shoup.net/ntl
# what is MakeDescCFLAGS=-O0
if(USING_MPIR)
  set(ntl_GMP_PREFIX GMP_PREFIX=${M2_HOST_PREFIX})
endif()
ExternalProject_Add(build-ntl
  URL               https://www.shoup.net/ntl/ntl-11.4.3.tar.gz
  URL_HASH          SHA256=b7c1ccdc64840e6a24351eb4a1e68887d29974f03073a1941c906562c0b83ad2
  PREFIX            libraries/ntl
  SOURCE_DIR        libraries/ntl/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND cd src && ${SET_LD_LIBRARY_PATH} ./configure
                      #-C --cache-file=${CONFIGURE_CACHE}
                      PREFIX=${M2_HOST_PREFIX}
                      ${ntl_GMP_PREFIX}
                      TUNE=generic # TODO: x86 and auto if NTL_WIZARD
                      NATIVE=off # TODO: on if not packaging?
                      SHARED=$<IF:$<BOOL:BUILD_SHARED_LIBS>,on,off>
                      NTL_STD_CXX14=on
                      NTL_NO_INIT_TRANS=on # TODO: still necessary?
                      CPPFLAGS=${CPPFLAGS} # TODO: add -DDEBUG if DEBUG
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CXX=${CMAKE_CXX_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     cd src && ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   cd src && ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} PREFIX=${M2_HOST_PREFIX} install
  TEST_COMMAND      cd src && ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test wizard
  )
ExternalProject_Add_Step(build-ntl wizard
  COMMENT           "Building NTL with NTL_WIZARD"
  COMMAND           ${SET_LD_LIBRARY_PATH} ./configure
                      #-C --cache-file=${CONFIGURE_CACHE}
                      PREFIX=${M2_HOST_PREFIX}
                      ${ntl_GMP_PREFIX}
                      TUNE=auto
                      NATIVE=on
                      SHARED=$<IF:$<BOOL:BUILD_SHARED_LIBS>,on,off>
                      NTL_STD_CXX14=on
                      NTL_NO_INIT_TRANS=on # TODO: still necessary?
                      CPPFLAGS=${CPPFLAGS} # TODO: add -DDEBUG if DEBUG
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CXX=${CMAKE_CXX_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
  COMMAND           ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS}
  WORKING_DIRECTORY libraries/ntl/build/src
  EXCLUDE_FROM_MAIN ON
  USES_TERMINAL ON
  )
if(NOT NTL_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-ntl-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-ntl configure build-mpir-install)
  endif()
endif()
if(AUTOTUNE)
  add_dependencies(build-ntl-install build-ntl-wizard)
endif()


ExternalProject_Add(build-flint
  GIT_REPOSITORY    https://github.com/mahrud/flint2.git
  GIT_TAG           HEAD
  PREFIX            libraries/flint2
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/flint2
  BINARY_DIR        libraries/flint2/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DCMAKE_POSITION_INDEPENDENT_CODE=ON
                    -DBUILD_TESTING=OFF # ${BUILD_TESTING} # Takes too long
                    -DBUILD_SHARED_LIBS=ON # ${BUILD_SHARED_LIBS} # TODO: get static flint building to work
                    -DIPO_SUPPORTED=OFF # TODO: because of clang; see https://github.com/wbhart/flint2/issues/644
                    -DHAVE_TLS=OFF
                    -DWITH_NTL=ON
                    -DWITH_MPIR=${USING_MPIR}
                    # Possible variables for the CMake build:
                    #-DHAS_FLAG_MPOPCNT
                    #-DHAS_FLAG_UNROLL_LOOPS
                    #-DBUILD_DOCS
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT FLINT_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-flint-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-flint configure build-mpir-install)
  endif()
  if(NOT MPFR_FOUND)
    ExternalProject_Add_StepDependencies(build-flint configure build-mpfr-install)
  endif()
  if(NOT NTL_FOUND)
    ExternalProject_Add_StepDependencies(build-flint configure build-ntl-install)
  endif()
endif()


# TODO: what is ftmpl_inst.o?
set(factory_CPPFLAGS "${CPPFLAGS} -DSING_NDEBUG -DOM_NDEBUG -Dmpz_div_2exp=mpz_fdiv_q_2exp -Dmpz_div_ui=mpz_fdiv_q_ui -Dmpz_div=mpz_fdiv_q")
set(factory_WARNFLAGS "-Wno-uninitialized -Wno-write-strings -Wno-deprecated")
# TODO: without this, factory finds flint, but not ntl. Why?
set(factory_NTL_HOME_PATH "${M2_HOST_PREFIX} ${NTL_INCLUDE_DIR}/..")
set(factory_FLINT_HOME_PATH "${M2_HOST_PREFIX} ${FLINT_INCLUDE_DIR}/..")
ExternalProject_Add(build-factory
  URL               ${M2_SOURCE_URL}/factory-4.1.1.tar.gz
  URL_HASH          SHA256=9dd84d11204e1457dac0a0d462a78d4cd4103c14cbf792b83d488aa529ad5724
  PREFIX            libraries/factory
  SOURCE_DIR        libraries/factory/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p0 < ${CMAKE_SOURCE_DIR}/libraries/factory/patch-4.1.1
  CONFIGURE_COMMAND cd factory-4.1.1 &&
                    autoreconf -vif && ${SET_LD_LIBRARY_PATH}
                    ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --disable-omalloc
                      --disable-doxygen-doc
                      ${shared_setting}
                      --enable-streamio
                      --without-Singular
                      --with-ntl=${factory_NTL_HOME_PATH}
                      --with-flint=${factory_FLINT_HOME_PATH}
                      # --enable-assertions
                      CPPFLAGS=${factory_CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     cd factory-4.1.1 && ${SET_LD_LIBRARY_PATH}
                    ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} all ftmpl_inst.o
                      AM_DEFAULT_VERBOSITY=1
                      WARNFLAGS=${factory_WARNFLAGS} &&
                    ./bin/makeheader factory.template     factory.h     &&
                    ./bin/makeheader factoryconf.template factoryconf.h &&
                    ${CMAKE_COMMAND} -E copy factory.h factoryconf.h include/factory &&
                    ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} all-recursive
  INSTALL_COMMAND   cd factory-4.1.1 && ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} install
  TEST_COMMAND      cd factory-4.1.1 && ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT FACTORY_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-factory-install)
  set(FACTORY_STREAMIO 1) # TODO: does this work?
  # TODO: repeat pkg_search_module(FACTORY      factory singular-factory IMPORTED_TARGET)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-factory configure build-mpir-install)
  endif()
  if(NOT MPFR_FOUND)
    ExternalProject_Add_StepDependencies(build-factory configure build-mpfr-install)
  endif()
  if(NOT NTL_FOUND)
    ExternalProject_Add_StepDependencies(build-factory configure build-ntl-install)
  endif()
  if(NOT FLINT_FOUND)
    ExternalProject_Add_StepDependencies(build-factory configure build-flint-install) # lol
  endif()
else()
  if(NOT EXISTS ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory/gftables)
    message(STATUS "Copying gftables in ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory/gftables")
    file(GLOB   GFTABLES    "${FACTORY_INCLUDE_DIR}/../share/factory/gftables/*")
    file(COPY ${GFTABLES} DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory/gftables)
  endif()
endif()


# TODO: version#"frobby version" is missing
# TODO: switch to https://github.com/Macaulay2/frobby
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
  BUILD_COMMAND     ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} library -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
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
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_HOST_PREFIX}/lib ${M2_HOST_PREFIX}/include
          COMMAND   ${CMAKE_COMMAND} -E copy bin/libfrobby.a ${M2_HOST_PREFIX}/lib/libfrobby.a
          COMMAND   ${CMAKE_COMMAND} -E copy src/frobby.h src/stdinc.h ${M2_HOST_PREFIX}/include
  TEST_COMMAND      ${MAKE_EXE} -j${PARALLEL_JOBS} test
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT FROBBY_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-frobby-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-frobby configure build-mpir-install)
  endif()
endif()


# https://www.inf.ethz.ch/personal/fukudak/cdd_home/
# topcom depends on cddlib, but includes setoper.h, rather than cdd/setoper.h
# TODO: anyway to change this?
set(cddlib_SUBDIRS "lib-src lib-src-gmp")
ExternalProject_Add(build-cddlib
  URL               ${M2_SOURCE_URL}/cddlib-094h.tar.gz
  URL_HASH          SHA256=fe6d04d494683cd451be5f6fe785e147f24e8ce3ef7387f048e739ceb4565ab5
  PREFIX            libraries/cddlib
  SOURCE_DIR        libraries/cddlib/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${SET_LD_LIBRARY_PATH} ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --includedir=${M2_HOST_PREFIX}/include/cdd
                      ${shared_setting}
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
  BUILD_COMMAND     ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
                      SUBDIRS=${cddlib_SUBDIRS}
                      gmpdir=/nowhere
  INSTALL_COMMAND   ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} install
                      SUBDIRS=${cddlib_SUBDIRS}
                      gmpdir=/nowhere
  TEST_COMMAND      ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT CDD_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-cddlib-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-cddlib configure build-mpir-install)
  endif()
endif()


ExternalProject_Add(build-glpk
  URL               ${M2_SOURCE_URL}/glpk-4.59.tar.gz
  URL_HASH          SHA256=e398be2e7cb8a98584325268704729872558a4a88555bc8a54139d017eb9ebae
  PREFIX            libraries/glpk
  SOURCE_DIR        libraries/glpk/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${SET_LD_LIBRARY_PATH} ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
  BUILD_COMMAND     ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} install-strip
  TEST_COMMAND      ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT GLPK_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-glpk-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-glpk configure build-mpir-install)
  endif()
endif()


ExternalProject_Add(build-mpsolve
  URL               https://numpi.dm.unipi.it/_media/software/mpsolve/mpsolve-3.1.8.tar.gz
  URL_HASH          SHA256=34740339d14cf8ca6d3f7da7ca12237b6da642623d14a6d6d5b5fc684c9c0fe5
  PREFIX            libraries/mpsolve
  SOURCE_DIR        libraries/mpsolve/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${SET_LD_LIBRARY_PATH} ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      --disable-examples
                      --disable-ui
                      --disable-documentation
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} install
  TEST_COMMAND      ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test # TODO: fails when building static library
  )
if(NOT MPSOLVE_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-mpsolve-install)
  if(NOT MPFR_FOUND)
    ExternalProject_Add_StepDependencies(build-mpsolve configure build-mpfr-install)
  endif()
endif()

#################################################################################
## Packages downloaded via git

# TODO: do we actually need it built?
ExternalProject_Add(build-googletest
  GIT_REPOSITORY    https://github.com/google/googletest.git
  GIT_TAG           release-1.10.0 # 42bc671f
  PREFIX            libraries/googletest
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/googletest
  BINARY_DIR        libraries/googletest/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX} -DBUILD_GMOCK=OFF # -DINSTALL_GTEST=OFF
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(BUILD_TESTING AND NOT GTEST_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-googletest-install)
endif()


## bdwgc
# Note: Starting with 8.0, libatomic_ops is not necessary for C11 or C++14.
# Currently cloning master for significant cmake support. Hopefully soon there will be a stable release
ExternalProject_Add(build-bdwgc
  GIT_REPOSITORY    https://github.com/ivmai/bdwgc.git
  GIT_TAG           master
  PREFIX            libraries/bdwgc
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/bdwgc
  BINARY_DIR        libraries/bdwgc/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
		    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
		    -Dbuild_tests=${BUILD_TESTING}
                    -Denable_cplusplus=ON
                    -Denable_threads=ON
                    -Denable_large_config=ON
                    -Dbuild_cord=OFF
                    -Denable_throw_bad_alloc_library=OFF
                    -Denable_gcj_support=OFF
                    -Denable_java_finalization=OFF
                    # -Denable_gc_debug=ON
                    # -Denable_gc_assertions=ON
                    # -Denable_parallel_mark=OFF
                    # -Dbuild_tests=ON
                    # -DGC_LARGE_ALLOC_WARN_INTERVAL=1
                    # -DGC_ABORT_ON_LEAK
                    # -DDBG_HDRS_ALL=1
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT BDWGC_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-bdwgc-install)
endif()


# TODO: out of source build has issues with detecting SIMD instructions
# TODO: separate source and binary directories
# FIXME: don't install givaro-config, can we not even build it?
ExternalProject_Add(build-givaro
  GIT_REPOSITORY    https://github.com/linbox-team/givaro.git
  GIT_TAG           v4.0.3
  PREFIX            libraries/givaro
  SOURCE_DIR        libraries/givaro/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${SET_LD_LIBRARY_PATH} ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      # --disable-simd # TODO: replaced with sse,sse2,sse3,ssse3,sse4.1,sse4.2avx,avx,avx2,fma,fma4
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
  BUILD_COMMAND     ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} -C src
  INSTALL_COMMAND   ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} -C src install
          COMMAND   ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} install-data
  TEST_COMMAND      ${SET_LD_LIBRARY_PATH} ${MAKE_EXE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT GIVARO_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-givaro-install)
  set(HAVE_GIVARO_isunit 1) # TODO: does this work?
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-givaro configure build-mpir-install)
  endif()
endif()


# Note: fflas_ffpack is just header files, so we don't build it
# instead we add an extra autotune target for generating fflas-ffpack-thresholds.h
# TODO: separate source and binary directories
ExternalProject_Add(build-fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            libraries/fflas_ffpack
  SOURCE_DIR        libraries/fflas_ffpack/build
#  BINARY_DIR        libraries/fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      # --enable-openmp
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
                      LIBS=${LA_LIBRARIES}
  BUILD_COMMAND     ""
  INSTALL_COMMAND   ${MAKE_EXE}  -j${PARALLEL_JOBS} install-data # only install headers and fflas-ffpack.pc
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install autotune
  )
ExternalProject_Add_Step(build-fflas_ffpack autotune
  COMMENT           "Generating fflas-ffpack-thresholds.h"
  COMMAND           ${MAKE_EXE} -j${PARALLEL_JOBS} autotune
  WORKING_DIRECTORY libraries/fflas_ffpack/build
  EXCLUDE_FROM_MAIN ON
  )
if(NOT FFLAS_FFPACK_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-fflas_ffpack-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-fflas_ffpack configure build-mpir-install)
  endif()
  # TODO: the requirement on Givaro version is for fflas_ffpack 2.3.2
  if(NOT GIVARO_FOUND OR GIVARO_VERSION VERSION_LESS 4.0.3)
    ExternalProject_Add_StepDependencies(build-fflas_ffpack configure build-givaro-install)
  endif()
endif()
if(AUTOTUNE)
  add_dependencies(build-fflas_ffpack-install build-fflas_ffpack-autotune)
endif()


# TODO: add testing
ExternalProject_Add(build-memtailor
  GIT_REPOSITORY    https://github.com/mahrud/memtailor.git
  GIT_TAG           af4a81f57fb585a541f5fefb517f2ad91b38cbe9 # original: e85453b
  PREFIX            libraries/memtailor
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/memtailor
  BINARY_DIR        libraries/memtailor/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DPACKAGE_TESTS=OFF
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT MEMTAILOR_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-memtailor-install)
endif()


# TODO: add testing
ExternalProject_Add(build-mathic
  GIT_REPOSITORY    https://github.com/mahrud/mathic.git
  GIT_TAG           770fe83edb4edae061af613328bbe2d380ea26d6 # original: 023afcf
  PREFIX            libraries/mathic
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/mathic
  BINARY_DIR        libraries/mathic/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DPACKAGE_TESTS=OFF
  DEPENDS           build-memtailor
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT MATHIC_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-mathic-install)
  if(NOT MEMTAILOR_FOUND)
    ExternalProject_Add_StepDependencies(build-mathic configure build-memtailor-install)
  endif()
endif()


# TODO: add testing
# TODO: g++ warning: tbb.h contains deprecated functionality.
# https://www.threadingbuildingblocks.org/docs/help/reference/appendices/deprecated_features.html
ExternalProject_Add(build-mathicgb
  GIT_REPOSITORY    https://github.com/mahrud/mathicgb.git
  GIT_TAG           557d3da746c6888ef05f29c2f095f0262080956d # original: bd634c8
  PREFIX            libraries/mathicgb
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/mathicgb
  BINARY_DIR        libraries/mathicgb/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DPACKAGE_TESTS=OFF
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
if(NOT MATHICGB_FOUND)
  # Add this to the libraries target
  add_dependencies(build-libraries build-mathicgb-install)
  if(NOT MATHIC_FOUND)
    ExternalProject_Add_StepDependencies(build-mathicgb configure build-mathic-install)
  endif()
endif()

###############################################################################
## Build required programs

# 4ti2 needs glpk and is used by the package FourTiTwo
set(4ti2_PROGRAMS
  4ti2int32  circuits  gensymm  groebner  markov    normalform  ppi     rays  zbasis
  4ti2int64  genmodel  graver   hilbert   minimize  output      qsolve  walk  zsolve)
list(TRANSFORM 4ti2_PROGRAMS PREPEND ${M2_HOST_PREFIX}/bin/ OUTPUT_VARIABLE 4ti2_PROGRAMS)
ExternalProject_Add(build-4ti2
  URL               ${M2_SOURCE_URL}/4ti2-1.6.9.tar.gz
  URL_HASH          SHA256=3053e7467b5585ad852f6a56e78e28352653943e7249ad5e5174d4744d174966
  PREFIX            libraries/4ti2
  SOURCE_DIR        libraries/4ti2/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
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
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE_EXE} -j${PARALLEL_JOBS} install-strip
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${4ti2_PROGRAMS} ${M2_INSTALL_PROGRAMSDIR}/bin
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON # FIXME
  STEP_TARGETS      install test
  )
if(NOT 4TI2)
  # Add this to the programs target
  add_dependencies(build-programs build-4ti2-install)
  if(NOT GLPK_FOUND)
    ExternalProject_Add_StepDependencies(build-4ti2 configure build-glpk-install)
  endif()
endif()


# https://github.com/BenjaminJurke/cohomCalg/
ExternalProject_Add(build-cohomcalg
  URL               ${M2_SOURCE_URL}/cohomCalg-0.32.tar.gz
  URL_HASH          SHA256=367c52b99c0b0a4794b215181439bf54abe4998872d3ef25d793bc13c4d40e42
  PREFIX            libraries/cohomcalg
  SOURCE_DIR        libraries/cohomcalg/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      LD=${CMAKE_CXX_COMPILER} # correct?
        COMMAND     ${CMAKE_STRIP} bin/cohomcalg
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different bin/cohomcalg ${M2_INSTALL_PROGRAMSDIR}/bin/
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON # FIXME
  STEP_TARGETS      install test
  )
if(NOT COHOMCALG)
  # Add this to the programs target
  add_dependencies(build-programs build-cohomcalg-install)
endif()


# gfan needs cddlib and is used by the packages gfanInterface and StatePolytope
set(gfan_CC  "${CMAKE_C_COMPILER}   ${CPPFLAGS}")
set(gfan_CXX "${CMAKE_CXX_COMPILER} ${CPPFLAGS}")
set(gfan_CLINKER  "${CMAKE_C_COMPILER}   ${LDFLAGS}")
set(gfan_CCLINKER "${CMAKE_CXX_COMPILER} ${LDFLAGS}")
ExternalProject_Add(build-gfan
  URL               ${M2_SOURCE_URL}/gfan0.6.2.tar.gz
  URL_HASH          SHA256=a674d5e5dc43634397de0d55dd5da3c32bd358d05f72b73a50e62c1a1686f10a
  PREFIX            libraries/gfan
  SOURCE_DIR        libraries/gfan/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/gfan/patch-0.6.2
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
                      CC=${gfan_CC}
                      CXX=${gfan_CXX}
                      CLINKER=${gfan_CLINKER}
                      CCLINKER=${gfan_CCLINKER}
                      PREFIX=/nowhere
                      CDD_LINKOPTIONS=-lcddgmp
        COMMAND     ${CMAKE_STRIP} gfan${CMAKE_EXECUTABLE_SUFFIX}
  INSTALL_COMMAND   ${MAKE_EXE} -j${PARALLEL_JOBS} PREFIX=${M2_INSTALL_PROGRAMSDIR} install
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON # FIXME
  STEP_TARGETS      install test
  )
if(NOT GFAN)
  # Add this to the programs target
  add_dependencies(build-programs build-gfan-install)
  if(NOT CDD_FOUND)
    ExternalProject_Add_StepDependencies(build-gfan configure build-cddlib-install)
  endif()
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
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} lrs
                      LIBDIR=${M2_HOST_PREFIX}/lib
                      CPPFLAGS=${CPPFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
                      # TODO: TARGET_ARCH= RANLIB=true
        COMMAND     ${CMAKE_STRIP} lrs${CMAKE_EXECUTABLE_SUFFIX}
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different lrs ${M2_INSTALL_PROGRAMSDIR}/bin/
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON # FIXME
  STEP_TARGETS      install test
  )
if(NOT LRSLIB)
  # Add this to the programs target
  add_dependencies(build-programs build-lrslib-install)
endif()


# TODO: do we need to make and strip theta/* if we don't use them?
set(csdp_CC  "${CMAKE_C_COMPILER}  ${OpenMP_C_FLAGS}")
set(csdp_CXX "${CMAKE_CXX_COMPILER} ${OpenMP_CXX_FLAGS}")
set(csdp_LDFLAGS "${LDFLAGS} ${OpenMP_CXX_FLAGS}")
list(JOIN OpenMP_CXX_LIBRARIES " " csdp_OpenMP_CXX_LIBRARIES)
set(csdp_LDLIBS  "${LDLIBS}  ${csdp_OpenMP_CXX_LIBRARIES}")
set(csdp_LIBS     "-L../lib -lsdp ${LA_LIBRARIES} -lm")
ExternalProject_Add(build-csdp
  URL               http://www.coin-or.org/download/source/Csdp/Csdp-6.2.0.tgz # TODO
  URL_HASH          SHA256=7f202a15f33483ee205dcfbd0573fdbd74911604bb739a04f8baa35f8a055c5b
  PREFIX            libraries/csdp
  SOURCE_DIR        libraries/csdp/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/csdp/patch-6.2.0
  CONFIGURE_COMMAND ""
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
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
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different solver/csdp ${M2_INSTALL_PROGRAMSDIR}/bin/
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON # FIXME
  STEP_TARGETS      install test
  )
if(NOT CSDP)
  # Add this to the programs target
  add_dependencies(build-programs build-csdp-install)
endif()


# normaliz needs libgmp, libgmpxx, boost and is used by the package Normaliz
# TODO: see special variables OPENMP and NORMFLAGS for macOS from libraries/normaliz/Makefile.in
set(normaliz_CXXFLAGS "${CPPFLAGS} -Wall -O3 -Wno-unknown-pragmas -std=c++11 -I .. -I . ")
if(NOT APPLE)
  # TODO: due to problem with -fopenmp on mac, skip this for apple
  set(normaliz_CXXFLAGS "${normaliz_CXXFLAGS} ${OpenMP_CXX_FLAGS}")
endif()
set(normaliz_GMPFLAGS "${LDFLAGS} -lgmpxx -lgmp") # TODO: what about mpir?
ExternalProject_Add(build-normaliz
  URL               https://github.com/Normaliz/Normaliz/releases/download/v3.8.4/normaliz-3.8.4.tar.gz
  URL_HASH          SHA256=795a0a752ef7bcc75e3307917c336436abfc836718c5cbf55043da6e7430cda3
  PREFIX            libraries/normaliz
  SOURCE_DIR        libraries/normaliz/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      --disable-flint
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
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS}
                      CXX=${CMAKE_CXX_COMPILER}
                      # NORMFLAGS=
                      CXXFLAGS=${normaliz_CXXFLAGS}
                      RANLIB=${CMAKE_RANLIB}
                      GMPFLAGS=${normaliz_GMPFLAGS}
        COMMAND     ${CMAKE_STRIP} source/normaliz
  # TODO: do we need the libraries as well, or just the binary?
  INSTALL_COMMAND   ${MAKE_EXE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different source/normaliz ${M2_INSTALL_PROGRAMSDIR}/bin/
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON # FIXME
  STEP_TARGETS      install test
  )
if(NOT NORMALIZ)
  # Add this to the programs target
  add_dependencies(build-programs build-normaliz-install)
  if(NOT MP_FOUND)
    ExternalProject_Add_StepDependencies(build-normaliz configure build-mpir-install)
  endif()
endif()


# nauty is used by the package Nauty
# URL = http://cs.anu.edu.au/~bdm/nauty
# TODO: do we not strip some files?
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
                      #-C --cache-file=${CONFIGURE_CACHE}
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
  BUILD_COMMAND     ${MAKE_EXE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
        COMMAND     ${CMAKE_STRIP} ${nauty_STRIPFILES}
  # TODO: put nauty programs in a folder?
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${nauty_PROGRAMS} ${M2_INSTALL_PROGRAMSDIR}/bin/
  TEST_COMMAND      rm -f ${nauty_CHECKERS}
       COMMAND      ${MAKE_EXE} BIGTEST=0 checks
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT NAUTY)
  # Add this to the programs target
  add_dependencies(build-programs build-nauty-install)
endif()


# http://www.rambau.wm.uni-bayreuth.de/TOPCOM/
set(topcom_PROGRAMS
  src-reg/checkregularity src/points2finetriang src/points2chiro src/chiro2circuits src/chiro2cocircuits
  src/points2allfinetriangs src/points2alltriangs src/points2ntriangs src/points2nfinetriangs
  src/points2finetriangs src/points2flips src/points2nallfinetriangs src/points2nalltriangs src/points2nflips
  src/points2triangs src/points2volume)
# TODO: any way to simplify this?
if(NOT CDD_FOUND)
  set(topcom_CPPFLAGS "${CPPFLAGS} -I${M2_HOST_PREFIX}/include/cdd")
else()
  set(topcom_CPPFLAGS "${CPPFLAGS} -I${CDD_INCLUDE_DIR}/cdd")
endif()
ExternalProject_Add(build-topcom
  URL               ${M2_SOURCE_URL}/TOPCOM-0.17.8.tar.gz
  URL_HASH          SHA256=3f83b98f51ee859ec321bacabf7b172c25884f14848ab6c628326b987bd8aaab
  PREFIX            libraries/topcom
  SOURCE_DIR        libraries/topcom/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/topcom/patch-0.17.8
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ./configure --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      CPPFLAGS=${topcom_CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     ${MAKE_EXE} -j1 # topcom doesn't like parallel builds
        COMMAND     ${CMAKE_STRIP} ${topcom_PROGRAMS}
  # TODO: put topcom programs in a folder?
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${topcom_PROGRAMS} ${M2_INSTALL_PROGRAMSDIR}/bin/
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON # FIXME
  STEP_TARGETS      install test
  )
if(NOT TOPCOM)
  # Add this to the programs target
  add_dependencies(build-programs build-topcom-install)
  if(NOT CDD_FOUND)
    ExternalProject_Add_StepDependencies(build-topcom configure build-cddlib-install)
  endif()
endif()

#############################################################################

if(BUILD_PROGRAMS)
  add_dependencies(build-programs
    build-4ti2-install
    build-cohomcalg-install
    build-gfan-install
    build-lrslib-install
    build-csdp-install
    build-normaliz-install
    build-nauty-install
    build-topcom-install
    )
else()
  # Make a symbolic link to the existing executable in the programs directory
  # TODO: more programs need to be symlinked
  foreach(program IN ITEMS 4TI2 COHOMCALG GFAN LRSLIB CSDP NORMALIZ NAUTY TOPCOM POLYMAKE)
    if(NOT ${program} MATCHES ${M2_INSTALL_PROGRAMSDIR})
      get_filename_component(program_name ${${program}} NAME)
      file(CREATE_LINK ${${program}} ${M2_INSTALL_PROGRAMSDIR}/bin/${program_name} SYMBOLIC)
    endif()
  endforeach()
endif()

if(BUILD_LIBRARIES)
  add_dependencies(build-libraries
    build-mpir-install
    build-mpfr-install
    build-ntl-install
    build-flint-install
    build-factory-install
    build-frobby-install
    build-cddlib-install
    build-glpk-install
    build-mpsolve-install
    build-googletest-install
    build-bdwgc-install
    build-givaro-install
    build-fflas_ffpack-install
    build-memtailor-install
    build-mathic-install
    build-mathicgb-install
    )
endif()

get_target_property(LIBRARY_DEPENDENCIES build-libraries MANUALLY_ADDED_DEPENDENCIES)
get_target_property(PROGRAM_DEPENDENCIES build-programs  MANUALLY_ADDED_DEPENDENCIES)
if(NOT LIBRARY_DEPENDENCIES)
  set(LIBRARY_DEPENDENCIES N/A)
endif()
if(NOT PROGRAM_DEPENDENCIES)
  set(PROGRAM_DEPENDENCIES N/A)
endif()
string(REGEX REPLACE "(build-|-install)" "" BUILD_LIB_LIST  "${LIBRARY_DEPENDENCIES}")
string(REGEX REPLACE "(build-|-install)" "" BUILD_PROG_LIST "${PROGRAM_DEPENDENCIES}")

message("## External components that will be built:
     Libraries         = ${BUILD_LIB_LIST}
     Programs          = ${BUILD_PROG_LIST}")
# TOOD: BUILDLIST BUILDSUBLIST BUILD_ALWAYS

message("## Library information:
     Linear Algebra    = ${LAPACK_LIBRARIES}
     MP Arithmetic     = ${MP_LIBRARIES}")
# TODO: how to keep track of things we've built?
#     BUILTLIBS         = ${BUILTLIBS}
#     LIBS              = ${LIBS}
