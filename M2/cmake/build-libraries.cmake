###############################################################################
## This script is responsible for dependencies between libraries and programs
## that we build and contains build instructions for them.
##
## - build all: cmake --build . --target build-libraries  build-programs
## - test all:  cmake --build . --target check-components check-components-slow (very slow)
## - clean all: cmake --build . --target clean-stamps

## Set the timestamp of the extracted content to the time of extraction
cmake_policy(SET CMP0135 NEW)

include(ExternalProject) # configure, patch, build, install, or test at build time
set(M2_SOURCE_URL https://macaulay2.com/Downloads/OtherSourceCode)

## This target builds external libraries that M2 relies on, then reruns cmake
add_custom_target(build-libraries COMMAND ${CMAKE_COMMAND} ${CMAKE_BINARY_DIR} USES_TERMINAL)

## This target builds external programs that are distributed with M2.
add_custom_target(build-programs)

## These target run the tests on the external components
add_custom_target(check-components)
add_custom_target(check-components-slow)

## This target forces libraries and programs to run their configure and install targets
add_custom_target(clean-stamps
  COMMAND rm libraries/*/src/build-*-stamp/build-*-{configure,build,install})

###############################################################################
## Set the default compile and link flags for external projects

# Preprocessor flags
set(CPPFLAGS "$ENV{CPPFLAGS}")
foreach(FLAG ${COMPILE_DEFINITIONS})
  set(CPPFLAGS "-D${FLAG} ${CPPFLAGS}")
endforeach()

# General compile flags
string(REPLACE ";" " " COMPILEFLAGS "${COMPILE_OPTIONS}")

# C compiler flags
set(CFLAGS   "${COMPILEFLAGS} -w -Wimplicit -Werror")

# C++ compiler flags
set(CXXFLAGS "${COMPILEFLAGS} -std=gnu++11 -w -Wno-mismatched-tags -Wno-deprecated-register")

# Linker flags
string(REPLACE ";" " " LDFLAGS "${LINK_OPTIONS}")

# Linear algebra library flags
if(APPLE)
  set(LA_LIBRARIES "-framework Accelerate")
else()
  list(JOIN LAPACK_LIBRARIES " " LA_LIBRARIES)
endif()

# Set the flags for building shared libraries
if(BUILD_SHARED_LIBS)
  set(shared_setting --enable-shared)
else()
  set(shared_setting --disable-shared)
endif()

# Set the flags for enabling assertions
if(CMAKE_BUILD_TYPE STREQUAL Debug)
  set(assertions_setting --enable-assertions)
  set(assert_setting --enable-assert)
elseif(CMAKE_BUILD_TYPE STREQUAL MinSizeRel)
  set(strip_setting --strip)
endif()

# Wrap configure and make commands so they find mpfr, etc.
if(APPLE)
  set(SET_LD_LIBRARY_PATH DYLD_LIBRARY_PATH=${M2_HOST_PREFIX}/lib:$ENV{DYLD_LIBRARY_PATH})
elseif(UNIX)
  set(SET_LD_LIBRARY_PATH   LD_LIBRARY_PATH=${M2_HOST_PREFIX}/lib:$ENV{LD_LIBRARY_PATH})
endif()

# Set the shared library path before every configure and make
set(CONFIGURE PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} ./configure)
set(CONFIGURE ${SET_LD_LIBRARY_PATH} ${CONFIGURE})
set(MAKE      ${SET_LD_LIBRARY_PATH} ${MAKE})
set(NINJA     ${SET_LD_LIBRARY_PATH} ninja)

# TODO: Accumulate information in usr-host/share/config.site to speed up configuration
# See: https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Cache-Files.html
set(CONFIGURE_CACHE ${M2_HOST_PREFIX}/share/config.site)

###############################################################################
## Helper functions for adding dependencies between components

# Ensure that _dependency is found before _name is configured
# _name:       lowercase name of a library or program; e.g. gmp, flint, cohomcalg
# _dependency: lowercase name of the library or program that ${_name} should depend on
FUNCTION (_ADD_STEP_DEPENDENCY _name _dependency)
  string(TOUPPER "${_dependency}" _condition)
  if(NOT ${_condition}_FOUND AND NOT ${_condition})
    ExternalProject_Add_StepDependencies(build-${_name} configure build-${_dependency}-install)
  endif()
ENDFUNCTION (_ADD_STEP_DEPENDENCY)

# If not _found_condition, add _name to the build-_component target
# _component:       either "libraries" or "programs"
# _name             lowercase name of a library or program; e.g. gmp, flint, cohomcalg
# _dependencies:    list of lowercase library names; e.g. "gmp mpfr"
# _found_condition: add dependency only if ${_found_condition} evaluates to false
FUNCTION (_ADD_COMPONENT_DEPENDENCY _component _name _dependencies _found_condition)
  if(NOT ${_found_condition})
    add_dependencies(build-${_component} build-${_name}-install)
    foreach(_dependency IN LISTS _dependencies)
      _ADD_STEP_DEPENDENCY(${_name} ${_dependency})
    endforeach()
  endif()
ENDFUNCTION (_ADD_COMPONENT_DEPENDENCY)

###############################################################################
## Pre-build actions

if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git")
  ## Update submodules as needed
  if(GIT_SUBMODULE)
    message(STATUS "Submodule update")
    execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      RESULT_VARIABLE GIT_SUBMOD_RESULT)
    if(NOT GIT_SUBMOD_RESULT EQUAL "0")
      message(WARNING "git submodule update failed, please checkout submodules manually")
    endif()
  endif()
endif()

# Create directories so copy instructions don't create files in their place
file(MAKE_DIRECTORY ${M2_HOST_PREFIX}/bin)
file(MAKE_DIRECTORY ${M2_HOST_PREFIX}/lib)
file(MAKE_DIRECTORY ${M2_HOST_PREFIX}/include)
file(MAKE_DIRECTORY ${M2_INSTALL_PROGRAMSDIR})
file(MAKE_DIRECTORY ${M2_INSTALL_LICENSESDIR})

###############################################################################
## Instructions for building required libraries
# See https://cmake.org/cmake/help/latest/module/ExternalProject.html

# http://eigen.tuxfamily.org/
if(CMAKE_BUILD_TYPE MATCHES "(Debug|Release|RelWithDebInfo)")
  set(EIGEN_BUILD_TYPE ${CMAKE_BUILD_TYPE})
else()
  set(EIGEN_BUILD_TYPE Release)
endif()
ExternalProject_Add(build-eigen
  URL               https://gitlab.com/libeigen/eigen/-/archive/3.4.0/eigen-3.4.0.tar.bz2
  URL_HASH          SHA256=b4c198460eba6f28d34894e3a5710998818515104d6e74e5cc331ce31e46e626
  PREFIX            libraries/eigen
  BINARY_DIR        libraries/eigen/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_BUILD_TYPE=${EIGEN_BUILD_TYPE}
                    -DBUILD_TESTING=${BUILD_TESTING}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
  INSTALL_COMMAND   ${CMAKE_COMMAND} --install . ${strip_setting}
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/eigen
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ../src/build-eigen/COPYING.MPL2 ${M2_INSTALL_LICENSESDIR}/eigen
  TEST_COMMAND      ${CMAKE_COMMAND} . -DEIGEN_LEAVE_TEST_IN_ALL_TARGET=ON
       COMMAND      ${CMAKE_COMMAND} --build .
       COMMAND      ${CMAKE_COMMAND} --build . --target test
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries eigen "" EIGEN3_FOUND)


# https://github.com/ivmai/bdwgc/
# TODO: add environment variables GC_LARGE_ALLOC_WARN_INTERVAL and GC_ABORT_ON_LEAK
# Note: Starting with 8.0, libatomic_ops is not necessary for C11 or C++14.
ExternalProject_Add(build-bdwgc
  PREFIX            libraries/bdwgc
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/bdwgc
  BINARY_DIR        libraries/bdwgc/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -Dbuild_tests=${BUILD_TESTING}
                    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_C_FLAGS=${CFLAGS}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
                    -Dbuild_cord=OFF
                    -Denable_threads=ON
                    -Denable_cplusplus=ON
                    -Denable_gcj_support=OFF
                    -Denable_large_config=ON
                    -Denable_java_finalization=OFF
                    -Denable_single_obj_compilation=ON
                    -Denable_parallel_mark=$<NOT:$<BOOL:${MEMDEBUG}>>
                    -Denable_gc_assertions=${MEMDEBUG}
                    -Denable_gc_debug=${MEMDEBUG}
                    -Ddisable_gc_debug=$<NOT:$<BOOL:${MEMDEBUG}>>
  INSTALL_COMMAND   ${CMAKE_COMMAND} --install . ${strip_setting}
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/bdwgc
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_SOURCE_DIR}/submodules/bdwgc/README.QUICK ${M2_INSTALL_LICENSESDIR}/bdwgc
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries bdwgc "" BDWGC_FOUND)


# https://www.mpfr.org/
# NOTE: mpfr puts pointers to gmp numbers in thread local variables, unless
# specially configured, so we shouldn't tell gmp to use libgc (we used to do that)
ExternalProject_Add(build-mpfr
  URL               https://www.mpfr.org/mpfr-current/mpfr-4.2.1.tar.xz
  URL_HASH          SHA256=277807353a6726978996945af13e52829e3abd7a9a5b7fb2793894e18f1fcbb2
  PREFIX            libraries/mpfr
  SOURCE_DIR        libraries/mpfr/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --with-gmp=${GMP_ROOT}
                      --disable-thread-safe
                      ${shared_setting}
                      ${assert_setting}
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} all
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/mpfr
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different README COPYING.LESSER ${M2_INSTALL_LICENSESDIR}/mpfr
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT MPFR_ROOT)
  set(MPFR_ROOT ${M2_HOST_PREFIX})
endif()
set(MPFR_INCLUDE_DIR ${MPFR_INCLUDE_DIRS}) # TODO: make this unnecessary in d/CMakeLists.txt
_ADD_COMPONENT_DEPENDENCY(libraries mpfr gmp MPFR_FOUND)


# http://perso.ens-lyon.fr/nathalie.revol/software.html
ExternalProject_Add(build-mpfi
  URL               ${M2_SOURCE_URL}/mpfi-1.5.4.tar.gz
  URL_HASH          SHA256=32e6ad529c97aa5ce03e28d01c921d1bce1a464fb4c57fbc248d7be21e652782
  PREFIX            libraries/mpfi
  SOURCE_DIR        libraries/mpfi/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --with-gmp=${GMP_ROOT}
                      --with-mpfr=${MPFR_ROOT}
                      ${shared_setting}
                      CC=${CMAKE_C_COMPILER}
                      CFLAGS=${CFLAGS}
                      CPPFLAGS=${CPPFLAGS}
                      LDFLAGS=${LDFLAGS}
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} all
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install-strip
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/mpfi
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING COPYING.LESSER ${M2_INSTALL_LICENSESDIR}/mpfi
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} -C tests check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries mpfi "gmp;mpfr" MPFI_FOUND)


# http://shoup.net/ntl
ExternalProject_Add(build-ntl
  URL               https://github.com/libntl/ntl/archive/refs/tags/v11.5.1.tar.gz
  URL_HASH          SHA256=ef578fa8b6c0c64edd1183c4c303b534468b58dd3eb8df8c9a5633f984888de5
  PREFIX            libraries/ntl
  SOURCE_DIR        libraries/ntl/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/ntl/patch-11.5.1
  CONFIGURE_COMMAND cd src && ${CONFIGURE} PREFIX=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      TUNE=generic
                      NATIVE=off
                      GMP_PREFIX=${GMP_ROOT}
                      SHARED=$<IF:$<BOOL:${BUILD_SHARED_LIBS}>,on,off>
                      NTL_STD_CXX14=on
                      NTL_NO_INIT_TRANS=on # TODO: still necessary?
                      CPPFLAGS=${CPPFLAGS} # TODO: add -DDEBUG if DEBUG
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CXX=${CMAKE_CXX_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     cd src && ${MAKE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   cd src && ${MAKE} -j${PARALLEL_JOBS} PREFIX=${M2_HOST_PREFIX} install &&
                    ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/ntl &&
                    ${CMAKE_COMMAND} -E copy_if_different ../doc/copying.txt ${M2_INSTALL_LICENSESDIR}/ntl
  TEST_COMMAND      cd src && ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test wizard
  )
ExternalProject_Add_Step(build-ntl wizard
  COMMENT           "Building NTL with NTL_WIZARD"
  COMMAND           ${CONFIGURE} PREFIX=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      TUNE=auto
                      NATIVE=on
                      GMP_PREFIX=${GMP_ROOT}
                      SHARED=$<IF:$<BOOL:${BUILD_SHARED_LIBS}>,on,off>
                      NTL_STD_CXX14=on
                      NTL_NO_INIT_TRANS=on # TODO: still necessary?
                      CPPFLAGS=${CPPFLAGS} # TODO: add -DDEBUG if DEBUG
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CXX=${CMAKE_CXX_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
  COMMAND           ${MAKE} -j${PARALLEL_JOBS}
  WORKING_DIRECTORY libraries/ntl/build/src
  EXCLUDE_FROM_MAIN ON
  USES_TERMINAL ON
  )
if(NOT NTL_ROOT)
  set(NTL_ROOT ${M2_HOST_PREFIX})
endif()
if(AUTOTUNE)
  add_dependencies(build-ntl-install build-ntl-wizard)
endif()
_ADD_COMPONENT_DEPENDENCY(libraries ntl gmp NTL_FOUND)


# https://github.com/Macaulay2/flint2
ExternalProject_Add(build-flint
  PREFIX            libraries/flint
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/flint
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ./bootstrap.sh &&
                    ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                    ${shared_setting}
                    "CPPFLAGS=${CPPFLAGS} -I${GMP_INCLUDE_DIRS}"
                    CFLAGS=${CFLAGS}
                    CXXFLAGS=${CXXFLAGS}
                    "LDFLAGS=${LDFLAGS} -L${GMP_LIBRARY_DIRS}"
                    CC=${CMAKE_C_COMPILER}
                    CXX=${CMAKE_CXX_COMPILER}
                    AR=${CMAKE_AR}
                    OBJDUMP=${CMAKE_OBJDUMP}
                    STRIP=${CMAKE_STRIP}
                    RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/flint
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_SOURCE_DIR}/submodules/flint/README.md ${M2_INSTALL_LICENSESDIR}/flint
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_SOURCE_DIR}/submodules/flint/COPYING ${M2_INSTALL_LICENSESDIR}/flint
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT FLINT_ROOT)
  set(FLINT_ROOT ${M2_HOST_PREFIX})
endif()
_ADD_COMPONENT_DEPENDENCY(libraries flint "gmp;mpfr;ntl" FLINT_FOUND)


# https://github.com/Singular/Sources/tree/spielwiese/factory
# https://service.mathematik.uni-kl.de/ftp/pub/Math/Singular/Factory/
# TODO: what is ftmpl_inst.o?
ExternalProject_Add(build-factory
  URL               https://macaulay2.com/Downloads/OtherSourceCode/factory-4.4.1.tar.gz
  URL_HASH          SHA256=345ec8ab2481135d18244e2a2ff6bc16e812a39a9eb5ac5d578956d8e0526e6e
  PREFIX            libraries/factory
  SOURCE_DIR        libraries/factory/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
#  PATCH_COMMAND     patch --batch < ${CMAKE_SOURCE_DIR}/libraries/factory/patch-4.4.0
  CONFIGURE_COMMAND autoreconf -vif &&
                    ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --disable-omalloc
                      --disable-doxygen-doc
                      ${shared_setting}
                      ${assertions_setting}
                      --enable-streamio
                      --without-Singular
                      --with-gmp=${GMP_ROOT}
                      --with-ntl=${NTL_ROOT}
                      --with-flint=${FLINT_ROOT}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER} &&
                    # Check to make sure Factory found NTL and Flint
                    set -x &&
                    grep [[^.define HAVE_NTL 1]] _config.h &&
                    grep [[^.define HAVE_FLINT 1]] _config.h
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} all
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install &&
                    ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/factory &&
                    ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/factory
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(GFTABLESDIR AND NOT EXISTS ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory/gftables)
  message(STATUS "Copying gftables from ${GFTABLESDIR}/gftables")
  file(COPY ${GFTABLESDIR}/gftables
    DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory FOLLOW_SYMLINK_CHAIN)
endif()
_ADD_COMPONENT_DEPENDENCY(libraries factory "gmp;ntl;flint" FACTORY_FOUND)


# https://github.com/Macaulay2/frobby (previously https://www.broune.com/frobby)
ExternalProject_Add(build-frobby
  PREFIX            libraries/frobby
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/frobby
  BINARY_DIR        libraries/frobby/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_TESTING=OFF # FIXME: ${BUILD_TESTING}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries frobby gmp FROBBY_FOUND)


# https://github.com/cddlib/cddlib
# https://www.inf.ethz.ch/personal/fukudak/cdd_home/
ExternalProject_Add(build-cddlib
  URL               https://github.com/cddlib/cddlib/releases/download/0.94m/cddlib-0.94m.tar.gz
  URL_HASH          SHA256=70dffdb3369b8704dc75428a1b3c42ab9047b81ce039f12f427e2eb2b1b0dee2
  PREFIX            libraries/cddlib
  SOURCE_DIR        libraries/cddlib/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      "CPPFLAGS=${CPPFLAGS} -I${GMP_INCLUDE_DIRS}"
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      "LDFLAGS=${LDFLAGS} -L${GMP_LIBRARY_DIRS}"
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C lib-src
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C lib-src install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/cddlib
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/cddlib
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT CDDLIB_ROOT)
  set(CDDLIB_ROOT ${M2_HOST_PREFIX})
  set(CDDLIB_LIBRARY_DIR ${CDDLIB_ROOT}/lib)
  set(CDDLIB_INCLUDE_DIR ${CDDLIB_ROOT}/include/cddlib)
endif()
#_ADD_COMPONENT_DEPENDENCY(libraries cddlib gmp CDDLIB_FOUND)


# https://github.com/algebraic-solving/msolve
ExternalProject_Add(build-msolve
  URL               https://github.com/algebraic-solving/msolve/archive/refs/tags/v0.7.5.tar.gz
  URL_HASH          SHA256=e88368abfd1e1918329ff9444164ca0e304835794fec68d192a63c845ae63128
  PREFIX            libraries/msolve
  SOURCE_DIR        libraries/msolve/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      $<$<BOOL:${OpenMP_FOUND}>:--enable-openmp>
                      "CPPFLAGS=${CPPFLAGS} -I${GMP_INCLUDE_DIRS} -I${MPFR_INCLUDE_DIRS} -I${FLINT_INCLUDE_DIR}"
                      CFLAGS=${CFLAGS}
		      "LDFLAGS=${LDFLAGS} -L${GMP_LIBRARY_DIRS} -L${MPFR_LIBRARIES} -L${FLINT_LIBRARIES}"
                      CC=${CMAKE_C_COMPILER}
		      "OPENMP_CFLAGS=${OpenMP_C_FLAGS} ${OpenMP_C_LDLIBS}"
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/msolve
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/msolve
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different msolve ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs msolve "gmp;mpfr;flint" MSOLVE_FOUND)


# https://numpi.dm.unipi.it/software/mpsolve
# Known issue: tests don't work with static library
ExternalProject_Add(build-mpsolve
  URL               ${M2_SOURCE_URL}/mpsolve-3.2.1.tar.gz
  URL_HASH          SHA256=3d11428ae9ab2e020f24cabfbcd9e4d9b22ec572cf70af0d44fe8dae1d51e78e
  PREFIX            libraries/mpsolve
  SOURCE_DIR        libraries/mpsolve/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/mpsolve/patch-3.2.1
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      --disable-debug # TODO: replace with --enable-debug-build conditionally
                      --disable-examples
                      --disable-ui
                      --disable-documentation
                      GMP_CFLAGS=-I${GMP_INCLUDE_DIRS}
                      GMP_LDFLAGS=-L${GMP_LIBRARY_DIRS}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C src/libmps
        COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C include
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C src/libmps install
          COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C include install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/mpsolve
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/mpsolve
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries mpsolve "gmp;mpfr" MPSOLVE_FOUND)


# https://casys.gricad-pages.univ-grenoble-alpes.fr/givaro/
string(REGEX REPLACE
  "./configure$" "${CMAKE_SOURCE_DIR}/submodules/givaro/autogen.sh" givaro_AUTOGEN "${CONFIGURE}")
set(givaro_LICENSEFILES COPYRIGHT Licence_CeCILL-B_V1-en.txt Licence_CeCILL-B_V1-fr.txt)
list(TRANSFORM givaro_LICENSEFILES PREPEND ${CMAKE_SOURCE_DIR}/submodules/givaro/)
ExternalProject_Add(build-givaro
  PREFIX            libraries/givaro
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/givaro
  BINARY_DIR        libraries/givaro/build
  CONFIGURE_COMMAND ${givaro_AUTOGEN} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      $<$<NOT:$<BOOL:${BUILD_NATIVE}>>:--without-archnative>
                      --with-gmp=${GMP_ROOT}
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C src
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C src install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/givaro
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${givaro_LICENSEFILES} ${M2_INSTALL_LICENSESDIR}/givaro
          COMMAND   ${MAKE} -j${PARALLEL_JOBS} install-data
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries givaro gmp GIVARO_FOUND)


# https://linbox-team.github.io/fflas-ffpack/
# NOTE: fflas_ffpack is just header files, so we don't build it
# instead we add an extra autotune target for generating fflas-ffpack-thresholds.h
string(REGEX REPLACE
  "./configure$" "${CMAKE_SOURCE_DIR}/submodules/fflas_ffpack/autogen.sh" fflas_ffpack_AUTOGEN "${CONFIGURE}")
set(fflas_ffpack_LICENSEFILES ${CMAKE_SOURCE_DIR}/submodules/fflas_ffpack/COPYING)
ExternalProject_Add(build-fflas_ffpack
  PREFIX            libraries/fflas_ffpack
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/fflas_ffpack
  BINARY_DIR        libraries/fflas_ffpack/build
  CONFIGURE_COMMAND ${fflas_ffpack_AUTOGEN} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      # --enable-precompilation # build errors
                      $<$<BOOL:${OpenMP_FOUND}>:--enable-openmp>
                      $<$<NOT:$<BOOL:${BUILD_NATIVE}>>:--without-archnative>
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
                      CBLAS_LIBS=${LA_LIBRARIES} # see macros/mkl-check.m4
                      "OMPFLAGS=${OpenMP_CXX_FLAGS} ${OpenMP_CXX_LDLIBS}"
  BUILD_COMMAND     true
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install-data # only headers and fflas-ffpack.pc
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/fflas_ffpack
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${fflas_ffpack_LICENSEFILES} ${M2_INSTALL_LICENSESDIR}/fflas_ffpack
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      autotune install test
  )
ExternalProject_Add_Step(build-fflas_ffpack autotune
  COMMENT           "Generating fflas-ffpack-thresholds.h"
  COMMAND           ${MAKE} -j${PARALLEL_JOBS} autotune
  WORKING_DIRECTORY libraries/fflas_ffpack/build
  EXCLUDE_FROM_MAIN ON
  )
if(AUTOTUNE)
  add_dependencies(build-fflas_ffpack-install build-fflas_ffpack-autotune)
endif()
if(NOT FFLAS_FFPACK_FOUND AND (NOT GIVARO_FOUND OR GIVARO_VERSION VERSION_LESS 4.0.3))
  # FIXME: the requirement on Givaro version is for fflas_ffpack 2.3.2
  _ADD_STEP_DEPENDENCY(fflas_ffpack givaro)
endif()
_ADD_COMPONENT_DEPENDENCY(libraries fflas_ffpack gmp FFLAS_FFPACK_FOUND)


# https://www.gnu.org/software/glpk/
ExternalProject_Add(build-glpk
  URL               https://ftp.gnu.org/gnu/glpk/glpk-5.0.tar.gz
  URL_HASH          SHA256=4a1013eebb50f728fc601bdd833b0b2870333c3b3e5a816eeba921d95bec6f15
  PREFIX            libraries/glpk
  SOURCE_DIR        libraries/glpk/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C src
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C src install-strip
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/glpk
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/glpk
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(NOT GLPK_ROOT)
  set(GLPK_ROOT ${M2_HOST_PREFIX})
endif()
_ADD_COMPONENT_DEPENDENCY(libraries glpk gmp GLPK_FOUND)


# https://github.com/google/googletest
ExternalProject_Add(build-googletest
  PREFIX            libraries/googletest
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/googletest
  BINARY_DIR        libraries/googletest/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX} -DBUILD_GMOCK=OFF
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
  EXCLUDE_FROM_ALL  ON
  STEP_TARGETS      install
  )
set(GOOGLETEST_FOUND ${GTEST_FOUND})
if(BUILD_TESTING)
  _ADD_COMPONENT_DEPENDENCY(libraries googletest "" GOOGLETEST_FOUND)
endif()


# https://github.com/Macaulay2/memtailor
ExternalProject_Add(build-memtailor
  PREFIX            libraries/memtailor
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/memtailor
  BINARY_DIR        libraries/memtailor/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_TESTING=OFF # FIXME: ${BUILD_TESTING}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
#_ADD_COMPONENT_DEPENDENCY(libraries memtailor googletest MEMTAILOR_FOUND)


# https://github.com/Macaulay2/mathic
ExternalProject_Add(build-mathic
  PREFIX            libraries/mathic
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/mathic
  BINARY_DIR        libraries/mathic/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_TESTING=OFF # FIXME: ${BUILD_TESTING}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
#_ADD_COMPONENT_DEPENDENCY(libraries mathic memtailor MATHIC_FOUND)


# https://github.com/Macaulay2/mathicgb
# TODO: g++ warning: tbb.h contains deprecated functionality.
# https://www.threadingbuildingblocks.org/docs/help/reference/appendices/deprecated_features.html
ExternalProject_Add(build-mathicgb
  PREFIX            libraries/mathicgb
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/mathicgb
  BINARY_DIR        libraries/mathicgb/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_TESTING=OFF # FIXME: ${BUILD_TESTING}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
                    -Dwith_tbb=${WITH_TBB}
                    -Denable_mgb=ON
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(EXISTS ${M2_HOST_PREFIX}/bin/mgb)
  execute_process(COMMAND mv ${M2_HOST_PREFIX}/bin/mgb ${M2_INSTALL_PROGRAMSDIR}/)
endif()
#_ADD_COMPONENT_DEPENDENCY(libraries mathicgb mathic MATHICGB_FOUND)


#############################################################################
## Instructions for building required programs

# https://github.com/4ti2/4ti2
# 4ti2 needs gmp and glpk and is used by the package FourTiTwo
set(4ti2_PROGRAMS
  4ti2gmp 4ti2int32 4ti2int64 circuits groebner markov minimize normalform
  qsolve rays walk zbasis zsolve hilbert graver ppi genmodel gensymm output)
list(TRANSFORM 4ti2_PROGRAMS PREPEND ${M2_HOST_PREFIX}/bin/ OUTPUT_VARIABLE 4ti2_PROGRAMS)
ExternalProject_Add(build-4ti2
  URL               https://github.com/4ti2/4ti2/releases/download/Release_1_6_10/4ti2-1.6.10.tar.gz
  URL_HASH          SHA256=f7c191beb14246b643e4fd5b18b53d9966693b9e6d3a569441a0e3ca14b1a86b
  PREFIX            libraries/4ti2
  SOURCE_DIR        libraries/4ti2/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     mkdir swig # needed because the tar doesn't have a swig directory but 4ti2's Makefile.am seems to need it
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --with-glpk=${GLPK_ROOT}
                      "CPPFLAGS=${CPPFLAGS} -I${GMP_INCLUDE_DIRS}"
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      "LDFLAGS=${LDFLAGS}  -L${GMP_LIBRARY_DIRS}"
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C src install-strip
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/4ti2
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different README ${M2_INSTALL_LICENSESDIR}/4ti2
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${4ti2_PROGRAMS} ${M2_INSTALL_PROGRAMSDIR}/
          COMMAND   ${CMAKE_COMMAND} -E remove -f ${4ti2_PROGRAMS}
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs 4ti2 "gmp;glpk" 4TI2)


# https://github.com/BenjaminJurke/cohomCalg
# cohomCalg is used by the package CohomCalg
ExternalProject_Add(build-cohomcalg
  URL               ${M2_SOURCE_URL}/cohomCalg-0.32.tar.gz
  URL_HASH          SHA256=367c52b99c0b0a4794b215181439bf54abe4998872d3ef25d793bc13c4d40e42
  PREFIX            libraries/cohomcalg
  SOURCE_DIR        libraries/cohomcalg/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      LD=${CMAKE_CXX_COMPILER} # set to g++ in Makefile
  INSTALL_COMMAND   ${CMAKE_STRIP} bin/cohomcalg
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/cohomcalg
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different LICENSE ${M2_INSTALL_LICENSESDIR}/cohomcalg
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different bin/cohomcalg ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${CMAKE_COMMAND} -E echo "Warning: No tests available for cohomCalg"
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs cohomcalg "" COHOMCALG)


# https://users-math.au.dk/~jensen/software/gfan/gfan.html
# gfan needs cddlib and is used by the packages gfanInterface and StatePolytopes
# TODO: would gfan benefit from enabling the USEFACTORY option?
ExternalProject_Add(build-gfan
  URL               ${M2_SOURCE_URL}/gfan0.6.2.tar.gz
  URL_HASH          SHA256=a674d5e5dc43634397de0d55dd5da3c32bd358d05f72b73a50e62c1a1686f10a
  PREFIX            libraries/gfan
  SOURCE_DIR        libraries/gfan/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/gfan/patch-0.6.2
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
                      cddnoprefix=yes
                      "GMP_LINKOPTIONS=-L${GMP_LIBRARY_DIRS} -lgmp"
                      "GMP_INCLUDEOPTIONS=-I${GMP_INCLUDE_DIRS}"
                      "OPTFLAGS=${CPPFLAGS} -DGMPRATIONAL -I${CDDLIB_INCLUDE_DIR}"
                      "CCLINKER=${CMAKE_CXX_COMPILER} ${LDFLAGS} -L${CDDLIB_LIBRARY_DIR}"
                      "CXX=${CMAKE_CXX_COMPILER}"
  INSTALL_COMMAND   ${CMAKE_STRIP} gfan
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/gfan
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different LICENSE COPYING ${M2_INSTALL_LICENSESDIR}/gfan
          COMMAND   ${MAKE} -j${PARALLEL_JOBS} PREFIX=${M2_INSTALL_PROGRAMSDIR}/.. install
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs gfan "gmp;cddlib" GFAN)


# http://www-cgrl.cs.mcgill.ca/~avis/C/lrs.html
# TODO: the shared library target doesn't work on Apple
ExternalProject_Add(build-lrslib
  URL               https://cgm.cs.mcgill.ca/~avis/C/lrslib/archive/lrslib-073.tar.gz
  URL_HASH          SHA256=c49a4ebd856183473d1d5a62785fcdfe1057d5d671d4b96f3a1250eb1afe4e83
  PREFIX            libraries/lrslib
  SOURCE_DIR        libraries/lrslib/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/lrslib/patch-073
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} lrs # all-shared
                      INCLUDEDIR=${GMP_INCLUDE_DIRS}
                      LIBDIR=${GMP_LIBRARY_DIRS}
                      CPPFLAGS=${CPPFLAGS}
                      "CFLAGS=${CFLAGS} -Ilrsarith-011"
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
  INSTALL_COMMAND   ${CMAKE_STRIP} lrs # lrs-shared liblrs.so.1.0.0
#          COMMAND   ${MAKE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/lrslib
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/lrslib
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different lrs ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${CMAKE_COMMAND} -E echo "Warning: No tests available for lrslib"
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs lrslib gmp LRSLIB)


# https://github.com/coin-or/Csdp
# TODO: what to do when OpenMP is not found
# TODO: set CFLAGS instead of CC, this is tricky due to csdp's Makefile
ExternalProject_Add(build-csdp
  URL               https://github.com/coin-or/Csdp/archive/releases/6.2.0.tar.gz
  URL_HASH          SHA256=3d341974af1f8ed70e1a37cc896e7ae4a513375875e5b46db8e8f38b7680b32f
  DOWNLOAD_NAME     csdp-6.2.0.tar.gz
  PREFIX            libraries/csdp
  SOURCE_DIR        libraries/csdp/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/csdp/patch-6.2.0
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
                      "CC=${CMAKE_C_COMPILER} ${OpenMP_C_FLAGS} ${CFLAGS}"
                      LDLIBS=${OpenMP_C_LDLIBS}
                      "LIBS=-L../lib -lsdp ${LA_LIBRARIES} -lm"
  INSTALL_COMMAND   ${CMAKE_STRIP} solver/csdp
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/csdp
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different LICENSE README ${M2_INSTALL_LICENSESDIR}/csdp
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different solver/csdp ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} -C test clean
       COMMAND      ${MAKE} -j${PARALLEL_JOBS} -C test
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs csdp "" CSDP)


# nauty is used by the package Nauty
# http://cs.anu.edu.au/~bdm/nauty
set(nauty_BINARIES
  dreadnaut NRswitchg addedgeg amtog biplabg catg complg converseg copyg countg cubhamg
  deledgeg delptg directg dretodot dretog edgetransg genbg genbgL geng genquarticg genrang
  genspecialg gentourng gentreeg hamheuristic labelg linegraphg listg multig newedgeg pickg
  planarg ranlabg shortg showg subdivideg twohamg vcolg watercluster2)
ExternalProject_Add(build-nauty
  URL               https://pallini.di.uniroma1.it/nauty2_8_9.tar.gz
  URL_HASH          SHA256=c97ab42bf48796a86a598bce3e9269047ca2b32c14fc23e07208a244fe52c4ee
  PREFIX            libraries/nauty
  SOURCE_DIR        libraries/nauty/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND rm -f aclocal.m4 && autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
  # TODO: put nauty programs in a folder?
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install includedir=${M2_HOST_PREFIX}/include/nauty
          COMMAND   ${CMAKE_STRIP} ${nauty_BINARIES}
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/nauty
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different LICENSE-2.0.txt ${M2_INSTALL_LICENSESDIR}/nauty
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${nauty_BINARIES} ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries nauty "" NAUTY_FOUND)


# https://www.normaliz.uni-osnabrueck.de/
# normaliz needs libgmp, libgmpxx, boost, and openmp, and is used by the package Normaliz
ExternalProject_Add(build-normaliz
  URL               https://github.com/Normaliz/Normaliz/releases/download/v3.10.4/normaliz-3.10.4.tar.gz
  URL_HASH          SHA256=9b424f966d553ae32e710b8ab674c7887ddcbf0e5ea08af7f8bc1b587bcbb2aa
  PREFIX            libraries/normaliz
  SOURCE_DIR        libraries/normaliz/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --disable-shared # TODO: for polymake --enable-shared
                      --without-flint # ${FLINT_ROOT}
		      --without-nauty
		      --without-cocoalib
		      --without-e-antic
                      "CPPFLAGS=${CPPFLAGS} -I${GMP_INCLUDE_DIRS}"
                      CFLAGS=${CFLAGS}
                      "CXXFLAGS=${CXXFLAGS} -std=gnu++14"
                      "LDFLAGS=${LDFLAGS}  -L${GMP_LIBRARY_DIRS}"
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      AR=${CMAKE_AR}
                      OBJDUMP=${CMAKE_OBJDUMP}
                      STRIP=${CMAKE_STRIP}
                      RANLIB=${CMAKE_RANLIB}
                      "OPENMP_CXXFLAGS=${OpenMP_CXX_FLAGS} ${OpenMP_CXX_LDLIBS}"
            COMMAND patch --fuzz=10 --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/normaliz/patch-libtool
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/normaliz
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/normaliz
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different source/normaliz ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries normaliz "gmp;nauty" NORMALIZ_FOUND)


# https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM/
set(topcom_PROGRAMS
  B_A B_A_center B_D checkregularity cocircuits2facets cross cube cyclic hypersimplex lattice
  chiro2allfinetriangs   chiro2dual              chiro2nallfinetriangs  chiro2placingtriang
  chiro2alltriangs       chiro2finetriang        chiro2nalltriangs      chiro2triangs
  chiro2circuits         chiro2finetriangs       chiro2nfinetriangs
  chiro2cocircuits       chiro2mintriang         chiro2ntriangs
  points2allfinetriangs  points2finetriang       points2nalltriangs     points2placingtriang
  points2alltriangs      points2finetriangs      points2nfinetriangs    points2triangs
  points2chiro           points2flips            points2nflips          points2volume
  points2facets          points2nallfinetriangs  points2ntriangs
  santos_22_triang       santos_dim4_triang      santos_triang)
list(TRANSFORM topcom_PROGRAMS PREPEND ${M2_HOST_PREFIX}/bin/ OUTPUT_VARIABLE topcom_PROGRAMS)
ExternalProject_Add(build-topcom
  URL               https://www.wm.uni-bayreuth.de/de/team/rambau_joerg/TOPCOM-Downloads/TOPCOM-1_1_2.tgz
  URL_HASH          SHA256=4fb10754ee5b76056441fea98f2c8dee5db6f2984d8c14283b49239ad4378ab6
  PREFIX            libraries/topcom
  SOURCE_DIR        libraries/topcom/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/topcom/patch-1.1.2
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      "CPPFLAGS=${CPPFLAGS} -I${GMP_INCLUDE_DIRS} -I${CDDLIB_INCLUDE_DIR}"
                      CFLAGS=${CFLAGS}
                      "CXXFLAGS=${CXXFLAGS} -std=c++17"
                      "LDFLAGS=${LDFLAGS} -L${GMP_LIBRARY_DIRS} -L${CDDLIB_LIBRARY_DIR}"
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     ${MAKE} -j1 # topcom doesn't like parallel builds
  INSTALL_COMMAND   ${MAKE} -j1 install-strip
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/topcom
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING README ${M2_INSTALL_LICENSESDIR}/topcom
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${topcom_PROGRAMS} ${M2_INSTALL_PROGRAMSDIR}/
          COMMAND   ${CMAKE_COMMAND} -E remove -f ${topcom_PROGRAMS}
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs topcom cddlib TOPCOM)

###############################################################################
## Experimental build scripts for other software programs.
## These programs are not built by default, but the build targets are provided.

# https://polymake.org/doku.php/install/install
# Note: polymake requires ~16G storage and permlib and perl-Term-ReadLine-Gnu on Fedora.
# TODO: the install target doesn't install polymake in usr-dist, only in usr-host
ExternalProject_Add(build-polymake
#  URL               https://polymake.org/lib/exe/fetch.php/download/polymake-4.1.tar.bz2 # Bundled version
#  URL_HASH          9ee571c08552672e990d0478f8c1ce13467b769c99049b8afd2fc39fe6ab35d6
  URL               https://polymake.org/lib/exe/fetch.php/download/polymake-4.1-minimal.tar.bz2 # Minimal version
  URL_HASH          SHA256=7e8d45bce800007e5c26ce5b7b5ac95731cbfc99df021e715abeb494ae550ac9
  PREFIX            libraries/polymake
  SOURCE_DIR        libraries/polymake/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --with-gmp=${GMP_ROOT}
                      --with-cdd=${CDDLIB_ROOT}
                      --with-flint=${FLINT_ROOT}
#                      --with-libnormaliz=${M2_HOST_PREFIX}
#                      --with-lrs=${M2_HOST_PREFIX}
#                      --with-lrs-include=${CMAKE_BINARY_DIR}/libraries/lrslib/build
                      --with-nauty-src=${CMAKE_BINARY_DIR}/libraries/nauty/build
                      --without-bliss
                      --without-java
#                      --without-ppl
#                      --without-scip
#                      --without-singular
#                      --without-soplex
#                      --without-sympol
                      CFLAGS=${CFLAGS}
                      "CXXFLAGS=${CPPFLAGS} -std=gnu++14 -w -Wno-mismatched-tags -Wno-deprecated-register"
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     ${NINJA} -C build/Opt
  INSTALL_COMMAND   ${NINJA} -C build/Opt install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/polymake
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/polymake
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${M2_HOST_PREFIX}/bin/polymake ${M2_INSTALL_PROGRAMSDIR}/
          COMMAND   ${CMAKE_COMMAND} -E copy_directory
            ${M2_HOST_PREFIX}/lib/polymake   ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2/lib
          COMMAND   ${CMAKE_COMMAND} -E copy_directory
            ${M2_HOST_PREFIX}/share/polymake ${M2_DIST_PREFIX}/${M2_INSTALL_DATAROOTDIR}
  TEST_COMMAND      ${MAKE} test
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  USES_TERMINAL_BUILD ON
  )
#_ADD_COMPONENT_DEPENDENCY(programs polymake "gmp;mpfr;cddlib;flint;normaliz;lrslib;nauty" POLYMAKE)


# http://homepages.math.uic.edu/~jan/download.html
ExternalProject_Add(build-phcpack
  URL               https://github.com/janverschelde/PHCpack/archive/refs/tags/v2.4.90.tar.gz
  URL_HASH          SHA256=7db1529b019a24e6fc2217ecccbcf3aa56f1b098bbe2f75e834f415e58c8bde7
  DOWNLOAD_NAME     PHCpack-2.4.90.tar.gz
  PREFIX            libraries/phcpack
  SOURCE_DIR        libraries/phcpack/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C src/Objects phc
                      CC=${CMAKE_C_COMPILER}
                      gpp=${CMAKE_CXX_COMPILER}
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/phcpack
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different LICENSE ${M2_INSTALL_LICENSESDIR}/phcpack
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different src/bin/phc ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} -C src/Objects testall
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  USES_TERMINAL_BUILD ON
  USES_TERMINAL_TEST ON
  )
#_ADD_COMPONENT_DEPENDENCY(libraries phcpack "???" PHCPACK)


# https://www3.nd.edu/~sommese/bertini/
# NOTE: Bertini's license is restrictive, so we don't build and distribute it by default.
ExternalProject_Add(build-bertini
  URL               https://bertini.nd.edu/BertiniSource_v1.6.tar.gz
  URL_HASH          SHA256=b742d4a55623092eb0c46f8ee644aa487e5decf4ad05eb9297306b599795a424
  PREFIX            libraries/bertini
  SOURCE_DIR        libraries/bertini/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --disable-shared
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COhMPILER}
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/bertini
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/bertini
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different bertini-serial ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
#_ADD_COMPONENT_DEPENDENCY(libraries bertini "gmp;mpfr" BERTINI)


###############################################################################
## Post-build actions

# Copy component licenses
if(EXISTS ${M2_HOST_PREFIX}/licenses)
  file(COPY ${M2_HOST_PREFIX}/licenses/ DESTINATION ${M2_INSTALL_LICENSESDIR}/)
endif()

# Copy dynamic libraries
if(EXISTS ${M2_HOST_PREFIX}/lib)
  file(COPY ${M2_HOST_PREFIX}/lib
    DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_LIBDIR}/Macaulay2
    FILES_MATCHING PATTERN "*.so*" PATTERN "*.dylib*"
    PATTERN "pkgconfig" EXCLUDE FOLLOW_SYMLINK_CHAIN)
endif()

# TODO: strip libraries and binaries

# TODO: fix RPATH on: zsolve, normaliz, 4ti2gmp, 4ti2nt32, 4ti2int64?

###############################################################################
## Populate the test-components target

# Make a list of built components to test
file(GLOB_RECURSE _install_stamp_list RELATIVE ${CMAKE_BINARY_DIR}/libraries
  "${CMAKE_BINARY_DIR}/libraries/*/src/build-*-stamp/build-*-install")
string(REGEX REPLACE "[a-z0-9_]+/src/build-[a-z0-9_]+-stamp/(build-[a-z0-9_]+-install)" "\\1"
  _installed_list "${_install_stamp_list}")
string(REGEX REPLACE "-install" "-test" TEST_TARGET_LIST  "${_installed_list}")

# Remove broken tests or those we skip
foreach(_test IN LISTS SKIP_TESTS)
  list(REMOVE_ITEM TEST_TARGET_LIST build-${_test}-test)
endforeach()
if(TEST_TARGET_LIST)
  add_dependencies(check-components-slow ${TEST_TARGET_LIST})
endif()

# Remove slow tests as well
foreach(_test IN LISTS SLOW_TESTS)
  list(REMOVE_ITEM TEST_TARGET_LIST build-${_test}-test)
endforeach()
if(TEST_TARGET_LIST)
  add_dependencies(check-components ${TEST_TARGET_LIST})
endif()

###############################################################################
## Report on the status

# Make a list of remaining components that we need
get_target_property(LIBRARY_DEPENDENCIES build-libraries MANUALLY_ADDED_DEPENDENCIES)
get_target_property(PROGRAM_DEPENDENCIES build-programs  MANUALLY_ADDED_DEPENDENCIES)
string(REGEX REPLACE "(build-|-install)" "" BUILD_LIB_LIST  "${LIBRARY_DEPENDENCIES}")
string(REGEX REPLACE "(build-|-install)" "" BUILD_PROG_LIST "${PROGRAM_DEPENDENCIES}")

# Report the next step
if(BUILD_LIB_LIST OR BUILD_PROG_LIST)
  message(CHECK_FAIL " Some components are missing")
  message("## Before building Macaulay2, rerun build-libraries and build-programs targets")
else()
  message(CHECK_PASS " Everything is in order! 🎉")
endif()

# Set empty lists to N/A
foreach(_list IN ITEMS
    BUILD_LIB_LIST INSTALLED_LIBRARIES BUILD_PROG_LIST INSTALLED_PROGRAMS)
  if(NOT ${_list})
    set(${_list} N/A)
  endif()
endforeach()

# Print some of that information
message("## External components
     Need to build:
       Libraries       = ${BUILD_LIB_LIST}
       Programs        = ${BUILD_PROG_LIST}
     Already built:
       Libraries       = ${INSTALLED_LIBRARIES}
       Programs        = ${INSTALLED_PROGRAMS}")

message("\n## Library information
     Linear Algebra    = ${LAPACK_LIBRARIES}
     GMP Arithmetic    = ${GMP_LIBRARIES}")

execute_process(COMMAND ${CMAKE_COMMAND} -E echo_append "     Optional libs     =")
foreach(_opt IN ITEMS OMP TBB FFI MPI XML PYTHON MYSQL)
  if(WITH_${_opt})
    execute_process(COMMAND ${CMAKE_COMMAND} -E echo_append " ${_opt}")
  endif()
endforeach()
message("\n")

# Report the default flags, but if verbose
if(VERBOSE)
  message("## Library compile options:
     CFLAGS            = ${CFLAGS}
     CXXFLAGS          = ${CXXFLAGS}
     LDFLAGS           = ${LDFLAGS}\n")
endif()

