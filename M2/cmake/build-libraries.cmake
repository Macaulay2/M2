###############################################################################
## This script is responsible for dependencies between libraries and programs
## that we build and contains build instructions for them.
##
## - build all: cmake --build . --target build-libraries  build-programs
## - test all:  cmake --build . --target check-components check-components-slow (very slow)
## - clean all: cmake --build . --target clean-stamps

include(ExternalProject) # configure, patch, build, install, or test at build time
set(M2_SOURCE_URL https://faculty.math.illinois.edu/Macaulay2/Downloads/OtherSourceCode)

## This target builds external libraries that M2 relies on.
add_custom_target(build-libraries
  COMMAND ${CMAKE_COMMAND} -E touch ${CMAKE_SOURCE_DIR}/cmake/check-libraries.cmake)

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
string(REPLACE ";" " " CPPFLAGS "$ENV{CPPFLAGS} ${COMPILE_OPTIONS}")

# C compiler flags
set(CFLAGS   "${CPPFLAGS} -w -Wimplicit -Werror")

# C++ compiler flags
set(CXXFLAGS "${CPPFLAGS} -std=gnu++11 -w -Wno-mismatched-tags -Wno-deprecated-register")

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

# Wrap configure and make commands so they find mpir, mpfr, etc.
if(APPLE)
  set(SET_LD_LIBRARY_PATH DYLD_LIBRARY_PATH=${M2_HOST_PREFIX}/lib)
elseif(UNIX)
  set(SET_LD_LIBRARY_PATH   LD_LIBRARY_PATH=${M2_HOST_PREFIX}/lib)
endif()

# Set the shared library path before every configure and make
set(CONFIGURE PKG_CONFIG_PATH=$ENV{PKG_CONFIG_PATH} ./configure)
set(CONFIGURE ${SET_LD_LIBRARY_PATH} ${CONFIGURE})
set(MAKE      ${SET_LD_LIBRARY_PATH} ${MAKE})
set(NINJA     ${SET_LD_LIBRARY_PATH} ${NINJA})

# TODO: Accumulate information in usr-host/share/config.site to speed up configuration
# See: https://www.gnu.org/software/autoconf/manual/autoconf-2.60/html_node/Cache-Files.html
set(CONFIGURE_CACHE ${M2_HOST_PREFIX}/share/config.site)

###############################################################################
## Helper functions for adding dependencies between components

# Ensure that _dependency is found before _name is configured
# _name:       lowercase name of a library or program; e.g. mp, flint, cohomcalg
# _dependency: lowercase name of the library or program that ${_name} should depend on
FUNCTION (_ADD_STEP_DEPENDENCY _name _dependency)
  if(${_dependency} STREQUAL mp)
    string(TOLOWER ${MP_LIBRARY} _dependency)
  endif()
  string(TOUPPER "${_dependency}" _condition)
  if(NOT ${_condition}_FOUND AND NOT ${_condition})
    ExternalProject_Add_StepDependencies(build-${_name} configure build-${_dependency}-install)
  endif()
ENDFUNCTION (_ADD_STEP_DEPENDENCY)

# If not _found_condition, add _name to the build-_component target
# _component:       either "libraries" or "programs"
# _name             lowercase name of a library or program; e.g. mp, flint, cohomcalg
# _dependencies:    list of lowercase library names; e.g. "mp mpfr"
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
      message(FATAL_ERROR "git submodule update --init failed with ${GIT_SUBMOD_RESULT}, please checkout submodules")
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
ExternalProject_Add(build-eigen
  URL               https://gitlab.com/libeigen/eigen/-/archive/3.3.7/eigen-3.3.7.tar.gz
  URL_HASH          SHA256=d56fbad95abf993f8af608484729e3d87ef611dd85b3380a8bad1d5cbc373a57
  PREFIX            libraries/eigen
  BINARY_DIR        libraries/eigen/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
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
# FIXME: fix a commit to use instead of master
ExternalProject_Add(build-bdwgc
#  GIT_REPOSITORY    https://github.com/ivmai/bdwgc.git
#  GIT_TAG           master
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


# https://github.com/wbhart/mpir
ExternalProject_Add(build-mpir
  URL               ${M2_SOURCE_URL}/mpir-3.0.0.tar.bz2
  URL_HASH          SHA256=52f63459cf3f9478859de29e00357f004050ead70b45913f2c2269d9708675bb
  PREFIX            libraries/mpir
  SOURCE_DIR        libraries/mpir/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  PATCH_COMMAND     patch --batch -p1 < ${CMAKE_SOURCE_DIR}/libraries/mpir/patch-3.0.0
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --enable-gmpcompat
                      --enable-cxx
                      --with-pic
                      --enable-static # FIXME: ${shared_setting}
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/mpir
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different README INSTALL COPYING COPYING.LIB ${M2_INSTALL_LICENSESDIR}/mpir
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
# Alias
if(NOT MP_FOUND)
  if(MP_LIBRARY STREQUAL GMP)
    # gmp is a prerequisite
    message(FATAL_ERROR "gmp integer package specified, but not found")
  elseif(MP_LIBRARY STREQUAL MPIR)
    # Add this to the libraries target
    _ADD_COMPONENT_DEPENDENCY(libraries mpir "" MPIR_FOUND)
  endif()
endif()


# https://www.mpfr.org/
# NOTE: mpfr puts pointers to gmp numbers in thread local variables, unless
# specially configured, so we shouldn't tell gmp to use libgc (we used to do that)
ExternalProject_Add(build-mpfr
  URL               ${M2_SOURCE_URL}/mpfr-4.0.2.tar.xz
  URL_HASH          SHA256=1d3be708604eae0e42d578ba93b390c2a145f17743a744d8f3f8c2ad5855a38a
  PREFIX            libraries/mpfr
  SOURCE_DIR        libraries/mpfr/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --with-gmp=${MP_ROOT}
                      --disable-thread-safe
                      --with-pic
                      ${shared_setting}
                      ${assert_setting}
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} all
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/mpfr
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different README COPYING.LESSER ${M2_INSTALL_LICENSESDIR}/mpfr
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
set(MPFR_INCLUDE_DIR ${MPFR_INCLUDE_DIRS}) # TODO: make this unnecessary in e/CMakeLists.txt
_ADD_COMPONENT_DEPENDENCY(libraries mpfr mp MPFR_FOUND)


# http://shoup.net/ntl
ExternalProject_Add(build-ntl
  URL               https://www.shoup.net/ntl/ntl-11.4.3.tar.gz
  URL_HASH          SHA256=b7c1ccdc64840e6a24351eb4a1e68887d29974f03073a1941c906562c0b83ad2
  PREFIX            libraries/ntl
  SOURCE_DIR        libraries/ntl/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND cd src && ${CONFIGURE} PREFIX=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      TUNE=generic
                      NATIVE=off
                      GMP_PREFIX=${MP_ROOT}
                      SHARED=on # FIXME: $<IF:$<BOOL:${BUILD_SHARED_LIBS}>,on,off>
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
                      GMP_PREFIX=${MP_ROOT}
                      SHARED=on # FIXME: $<IF:$<BOOL:${BUILD_SHARED_LIBS}>,on,off>
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
if(AUTOTUNE)
  add_dependencies(build-ntl-install build-ntl-wizard)
endif()
_ADD_COMPONENT_DEPENDENCY(libraries ntl mp NTL_FOUND)


# https://github.com/wbhart/flint2
ExternalProject_Add(build-flint
#  GIT_REPOSITORY    https://github.com/mahrud/flint2.git
#  GIT_TAG           HEAD
  PREFIX            libraries/flint2
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/flint2
  BINARY_DIR        libraries/flint2/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_C_FLAGS=${CFLAGS}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
                    -DCMAKE_POSITION_INDEPENDENT_CODE=ON
                    -DBUILD_SHARED_LIBS=ON # FIXME: ${BUILD_SHARED_LIBS} # TODO: get static flint building to work
                    -DIPO_SUPPORTED=OFF # TODO: because of clang; see https://github.com/wbhart/flint2/issues/644
                    -DHAVE_TLS=OFF
                    -DWITH_NTL=ON
                    -DWITH_MPIR=${USING_MPIR}
                    # TODO: force SIMD flags off for distribution
                    #-DHAS_FLAG_MPOPCNT
                    #-DHAS_FLAG_UNROLL_LOOPS
  INSTALL_COMMAND   ${CMAKE_COMMAND} --install . ${strip_setting}
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/flint2
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_SOURCE_DIR}/submodules/flint2/README ${M2_INSTALL_LICENSESDIR}/flint2
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_SOURCE_DIR}/submodules/flint2/LICENSE ${M2_INSTALL_LICENSESDIR}/flint2
  TEST_COMMAND      ${CMAKE_COMMAND} . -DBUILD_TESTING=ON
       COMMAND      ${CMAKE_COMMAND} --build .
       COMMAND      ${CMAKE_COMMAND} --build . --target test
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries flint "mp;mpfr;ntl" FLINT_FOUND)


# https://github.com/Singular/Sources/tree/spielwiese/factory
# TODO: what is ftmpl_inst.o?
set(factory_CPPFLAGS "${CPPFLAGS} -Dmpz_div_2exp=mpz_fdiv_q_2exp -Dmpz_div_ui=mpz_fdiv_q_ui -Dmpz_div=mpz_fdiv_q")
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
                    autoreconf -vif &&
                    ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --disable-omalloc
                      --disable-doxygen-doc
                      ${shared_setting}
                      ${assertions_setting}
                      --enable-streamio
                      --without-Singular
                      --with-ntl=${factory_NTL_HOME_PATH}
                      --with-flint=${factory_FLINT_HOME_PATH}
                      CPPFLAGS=${factory_CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     cd factory-4.1.1 && ${MAKE} -j${PARALLEL_JOBS} all
  INSTALL_COMMAND   cd factory-4.1.1 && ${MAKE} -j${PARALLEL_JOBS} install &&
                    ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/factory &&
                    ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/factory
  TEST_COMMAND      cd factory-4.1.1 && ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
if(FACTORY_FOUND)
  if(NOT EXISTS ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory/gftables)
    message(STATUS "Copying gftables in ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory/gftables")
    file(GLOB   GFTABLES    "${FACTORY_INCLUDE_DIR}/../share/factory/gftables/*")
    file(COPY ${GFTABLES} DESTINATION ${M2_DIST_PREFIX}/${M2_INSTALL_DATADIR}/Core/factory/gftables)
  endif()
endif()
_ADD_COMPONENT_DEPENDENCY(libraries factory "mp;mpfr;ntl;flint" FACTORY_FOUND)


# https://www.broune.com/frobby/
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
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} library -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
                      GMP_INC_DIR=${MP_ROOT}/include
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
  INSTALL_COMMAND   ${CMAKE_COMMAND} -E copy bin/libfrobby.a ${M2_HOST_PREFIX}/lib/libfrobby.a
          COMMAND   ${CMAKE_COMMAND} -E copy src/frobby.h src/stdinc.h ${M2_HOST_PREFIX}/include/
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/frobby
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/frobby
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} test
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries frobby mp FROBBY_FOUND)


# https://github.com/cddlib/cddlib
# https://www.inf.ethz.ch/personal/fukudak/cdd_home/
ExternalProject_Add(build-cddlib
  URL               https://github.com/cddlib/cddlib/releases/download/0.94j/cddlib-0.94j.tar.gz
  URL_HASH          SHA256=27d7fcac2710755a01ef5381010140fc57c95f959c3c5705c58539d8c4d17bfb
  PREFIX            libraries/cddlib
  SOURCE_DIR        libraries/cddlib/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C lib-src
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C lib-src install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/cddlib
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/cddlib
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries cddlib mp CDDLIB_FOUND)


# https://numpi.dm.unipi.it/software/mpsolve
ExternalProject_Add(build-mpsolve
  URL               https://numpi.dm.unipi.it/_media/software/mpsolve/mpsolve-3.1.8.tar.gz
  URL_HASH          SHA256=34740339d14cf8ca6d3f7da7ca12237b6da642623d14a6d6d5b5fc684c9c0fe5
  PREFIX            libraries/mpsolve
  SOURCE_DIR        libraries/mpsolve/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C src/libmps
        COMMAND     ${MAKE} -j${PARALLEL_JOBS} -C include
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C src/libmps install
          COMMAND   ${MAKE} -j${PARALLEL_JOBS} -C include install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/mpsolve
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different README ${M2_INSTALL_LICENSESDIR}/mpsolve
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test # TODO: fails when building static library
  )
_ADD_COMPONENT_DEPENDENCY(libraries mpsolve "mp;mpfr" MPSOLVE_FOUND)


# https://casys.gricad-pages.univ-grenoble-alpes.fr/givaro/
# TODO: get out-of-tree build working: https://github.com/linbox-team/givaro/issues/154
if(NOT BUILD_NATIVE)
  set(givaro_SIMD sse sse2 sse3 ssse3 sse4.1 sse4.2avx avx avx2 fma fma4)
  list(TRANSFORM givaro_SIMD PREPEND "--disable-")
endif()
set(givaro_LICENSEFILES COPYRIGHT Licence_CeCILL-B_V1-en.txt Licence_CeCILL-B_V1-fr.txt)
ExternalProject_Add(build-givaro
  GIT_REPOSITORY    https://github.com/linbox-team/givaro.git
  GIT_TAG           v4.0.3
  PREFIX            libraries/givaro
  SOURCE_DIR        libraries/givaro/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      ${shared_setting}
                      ${givaro_SIMD}
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
_ADD_COMPONENT_DEPENDENCY(libraries givaro mp GIVARO_FOUND)


# https://linbox-team.github.io/fflas-ffpack/
# NOTE: fflas_ffpack is just header files, so we don't build it
# instead we add an extra autotune target for generating fflas-ffpack-thresholds.h
# TODO: separate source and binary directories
# TODO: combine with OpenMP when it is present
ExternalProject_Add(build-fflas_ffpack
  GIT_REPOSITORY    https://github.com/Macaulay2/fflas-ffpack.git
  GIT_TAG           712cef0e
  PREFIX            libraries/fflas_ffpack
  SOURCE_DIR        libraries/fflas_ffpack/build
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
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
  BUILD_COMMAND     true
  INSTALL_COMMAND   ${MAKE} -j${PARALLEL_JOBS} install-data # only headers and fflas-ffpack.pc
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/fflas_ffpack
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/fflas_ffpack
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
_ADD_COMPONENT_DEPENDENCY(libraries fflas_ffpack mp FFLAS_FFPACK_FOUND)


# https://www.gnu.org/software/glpk/
ExternalProject_Add(build-glpk
  URL               ${M2_SOURCE_URL}/glpk-4.59.tar.gz
  URL_HASH          SHA256=e398be2e7cb8a98584325268704729872558a4a88555bc8a54139d017eb9ebae
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
_ADD_COMPONENT_DEPENDENCY(libraries glpk mp GLPK_FOUND)


# https://github.com/google/googletest
ExternalProject_Add(build-googletest
#  GIT_REPOSITORY    https://github.com/google/googletest.git
#  GIT_TAG           release-1.10.0 # 42bc671f
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
#  GIT_REPOSITORY    https://github.com/mahrud/memtailor.git
#  GIT_TAG           ece5f0b292abd97b48ba103dc4566fc708e8a968
  PREFIX            libraries/memtailor
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/memtailor
  BINARY_DIR        libraries/memtailor/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_TESTING=${BUILD_TESTING}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries memtailor googletest MEMTAILOR_FOUND)


# https://github.com/Macaulay2/mathic
ExternalProject_Add(build-mathic
#  GIT_REPOSITORY    https://github.com/mahrud/mathic.git
#  GIT_TAG           f96f18604d6a1308baac3a7f14b3d8a18f636461
  PREFIX            libraries/mathic
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/mathic
  BINARY_DIR        libraries/mathic/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_TESTING=${BUILD_TESTING}
                    -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER}
                    -DCMAKE_CXX_FLAGS=${CXXFLAGS}
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(libraries mathic memtailor MATHIC_FOUND)


# https://github.com/Macaulay2/mathicgb
# TODO: use TBB when it is present
# TODO: g++ warning: tbb.h contains deprecated functionality.
# https://www.threadingbuildingblocks.org/docs/help/reference/appendices/deprecated_features.html
ExternalProject_Add(build-mathicgb
#  GIT_REPOSITORY    https://github.com/mahrud/mathicgb.git
#  GIT_TAG           1da73abd34d90572ee765c8be133dce23b07dfb7
  PREFIX            libraries/mathicgb
  SOURCE_DIR        ${CMAKE_SOURCE_DIR}/submodules/mathicgb
  BINARY_DIR        libraries/mathicgb/build
  CMAKE_ARGS        -DCMAKE_INSTALL_PREFIX=${M2_HOST_PREFIX}
                    -DCMAKE_SYSTEM_PREFIX_PATH=${M2_HOST_PREFIX}
                    -DCMAKE_MODULE_PATH=${CMAKE_SOURCE_DIR}/cmake
                    -DBUILD_SHARED_LIBS=${BUILD_SHARED_LIBS}
                    -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
                    -DBUILD_TESTING=${BUILD_TESTING}
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
_ADD_COMPONENT_DEPENDENCY(libraries mathicgb mathic MATHICGB_FOUND)


#############################################################################
## Instructions for building required programs

# https://github.com/4ti2/4ti2
# 4ti2 needs gmp and glpk and is used by the package FourTiTwo
set(4ti2_PROGRAMS
  4ti2gmp 4ti2int32 4ti2int64 circuits groebner markov minimize normalform
  qsolve rays walk zbasis zsolve hilbert graver ppi genmodel gensymm output)
list(TRANSFORM 4ti2_PROGRAMS PREPEND ${M2_HOST_PREFIX}/bin/ OUTPUT_VARIABLE 4ti2_PROGRAMS)
ExternalProject_Add(build-4ti2
  URL               https://github.com/4ti2/4ti2/releases/download/Release_1_6_9/4ti2-1.6.9.tar.gz
  URL_HASH          SHA256=3053e7467b5585ad852f6a56e78e28352653943e7249ad5e5174d4744d174966
  PREFIX            libraries/4ti2
  SOURCE_DIR        libraries/4ti2/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
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
_ADD_COMPONENT_DEPENDENCY(programs 4ti2 "mp;glpk" 4TI2)


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
                      CXX=${CMAKE_CXX_COMPILER}
                      LD=${CMAKE_CXX_COMPILER} # correct?
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
set(gfan_OPTFLAGS "${CPPFLAGS} -DGMPRATIONAL") # overriding the flags defined in Makefile
if(CDDLIB_FOUND)
  set(gfan_OPTFLAGS "${gfan_OPTFLAGS} -I${CDDLIB_INCLUDE_DIR}")
endif()
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
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
                      cddnoprefix=yes
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
                      OPTFLAGS=${gfan_OPTFLAGS}
                      CLINKER=${gfan_CLINKER}
                      CCLINKER=${gfan_CCLINKER}
  INSTALL_COMMAND   ${CMAKE_STRIP} gfan
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/gfan
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different LICENSE COPYING ${M2_INSTALL_LICENSESDIR}/gfan
          COMMAND   ${MAKE} -j${PARALLEL_JOBS} PREFIX=${M2_INSTALL_PROGRAMSDIR}/../ install
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs gfan "mp;cddlib;factory" GFAN)


# http://www-cgrl.cs.mcgill.ca/~avis/C/lrs.html
# TODO: update to v70a which is 4 years more recent
ExternalProject_Add(build-lrslib
  URL               ${M2_SOURCE_URL}/lrslib-062.tar.gz
  URL_HASH          SHA256=adf92f9c7e70c001340b9c28f414208d49c581df46b550f56ab9a360348e4f09
  PREFIX            libraries/lrslib
  SOURCE_DIR        libraries/lrslib/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX} lrs
                      LIBDIR=${M2_HOST_PREFIX}/lib
                      CPPFLAGS=${CPPFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      RANLIB=${CMAKE_RANLIB}
                      # TODO: TARGET_ARCH= RANLIB=true
  INSTALL_COMMAND   ${CMAKE_STRIP} lrs
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/lrslib
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/lrslib
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different lrs ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${CMAKE_COMMAND} -E echo "Warning: No tests available for cohomCalg"
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs lrslib mp LRSLIB)


# https://github.com/coin-or/Csdp
# TODO: what to do when OpenMP is not found
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
  CONFIGURE_COMMAND true
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS} prefix=${M2_HOST_PREFIX}
                      CC=${csdp_CC}
                      CXX=${csdp_CXX}
                      LDFLAGS=${csdp_LDFLAGS}
                      LDLIBS=${csdp_LDLIBS}
                      LIBS=${csdp_LIBS}
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
  URL               ${M2_SOURCE_URL}/nauty27b11.tar.gz
  URL_HASH          SHA256=5d52211cec767d8d8e43483d96202be235f85696d1373c307291273463c812fa
  PREFIX            libraries/nauty
  SOURCE_DIR        libraries/nauty/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
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
  INSTALL_COMMAND   ${CMAKE_STRIP} ${nauty_BINARIES}
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/nauty
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different nauty.h ${M2_INSTALL_LICENSESDIR}/nauty
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${nauty_BINARIES} ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs nauty "" NAUTY)


# https://www.normaliz.uni-osnabrueck.de/
# normaliz needs libgmp, libgmpxx, boost and is used by the package Normaliz
# TODO: see special variables OPENMP and NORMFLAGS for macOS from libraries/normaliz/Makefile.in
# TODO: what to do when OpenMP is not found
set(normaliz_CXXFLAGS "${CPPFLAGS} -Wall -O3 -Wno-unknown-pragmas -std=c++11 -I .. -I . ")
if(NOT APPLE)
  # TODO: due to problem with -fopenmp on mac, skip this for apple
  set(normaliz_CXXFLAGS "${normaliz_CXXFLAGS} ${OpenMP_CXX_FLAGS}")
endif()
set(normaliz_GMPFLAGS "${LDFLAGS} -lgmpxx -lgmp")
ExternalProject_Add(build-normaliz
  URL               https://github.com/Normaliz/Normaliz/releases/download/v3.8.4/normaliz-3.8.4.tar.gz
  URL_HASH          SHA256=795a0a752ef7bcc75e3307917c336436abfc836718c5cbf55043da6e7430cda3
  PREFIX            libraries/normaliz
  SOURCE_DIR        libraries/normaliz/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND autoreconf -vif
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      --disable-shared
                      --without-flint
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
  BUILD_COMMAND     ${MAKE} -j${PARALLEL_JOBS}
                      CXX=${CMAKE_CXX_COMPILER}
                      # NORMFLAGS=
                      CXXFLAGS=${normaliz_CXXFLAGS}
                      RANLIB=${CMAKE_RANLIB}
                      GMPFLAGS=${normaliz_GMPFLAGS}
  # TODO: do we need the libraries as well, or just the binary?
  INSTALL_COMMAND   ${CMAKE_STRIP} source/normaliz
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/normaliz
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/normaliz
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different source/normaliz ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs normaliz "mp;nauty" NORMALIZ)


# http://www.rambau.wm.uni-bayreuth.de/TOPCOM/
# topcom needs cddlib
set(topcom_PROGRAMS
  src-reg/checkregularity src/points2finetriang src/points2chiro src/chiro2circuits src/chiro2cocircuits
  src/points2allfinetriangs src/points2alltriangs src/points2ntriangs src/points2nfinetriangs
  src/points2finetriangs src/points2flips src/points2nallfinetriangs src/points2nalltriangs src/points2nflips
  src/points2triangs src/points2volume)
set(topcom_CPPFLAGS "${CPPFLAGS}")
if(CDDLIB_FOUND)
  set(topcom_CPPFLAGS "${topcom_CPPFLAGS} -I${CDDLIB_INCLUDE_DIR}")
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
            COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
                      CPPFLAGS=${topcom_CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     ${MAKE} -j1 # topcom doesn't like parallel builds
  # TODO: put topcom programs in a folder?
  INSTALL_COMMAND   ${CMAKE_STRIP} ${topcom_PROGRAMS}
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/topcom
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING README ${M2_INSTALL_LICENSESDIR}/topcom
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${topcom_PROGRAMS} ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} -j${PARALLEL_JOBS} check
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
_ADD_COMPONENT_DEPENDENCY(programs topcom cddlib TOPCOM)


# TODO: this is still experimental
# https://polymake.org
# Polymake needs gmp, mpfr, cdd, lrs, libnormaliz, nauty and jReality
#set(polymake_CPPFLAGS "${CPPFLAGS}")
#if(CDDLIB_FOUND)
#  set(polymake_CPPFLAGS "${polymake_CPPFLAGS} -I${CDDLIB_INCLUDE_DIR}")
#endif()
ExternalProject_Add(build-polymake
  URL               https://polymake.org/lib/exe/fetch.php/download/polymake-4.0r1-minimal.tar.bz2
  URL_HASH          SHA256=b29de50dda6f657f2e82ef6acff62df1b51128a20c5d53bd97226ea22fdc3b52
  PREFIX            libraries/polymake
  SOURCE_DIR        libraries/polymake/build
  DOWNLOAD_DIR      ${CMAKE_SOURCE_DIR}/BUILD/tarfiles
  BUILD_IN_SOURCE   ON
  CONFIGURE_COMMAND ${CONFIGURE} --prefix=${M2_HOST_PREFIX}
                      #-C --cache-file=${CONFIGURE_CACHE}
		      --with-gmp=${MP_ROOT}
                      CPPFLAGS=${CPPFLAGS}
                      CFLAGS=${CFLAGS}
                      CXXFLAGS=${CXXFLAGS}
                      LDFLAGS=${LDFLAGS}
                      CC=${CMAKE_C_COMPILER}
                      CXX=${CMAKE_CXX_COMPILER}
  BUILD_COMMAND     ${MAKE}
  INSTALL_COMMAND   ninja -C build/Opt install
          COMMAND   ${CMAKE_COMMAND} -E make_directory ${M2_INSTALL_LICENSESDIR}/polymake
          COMMAND   ${CMAKE_COMMAND} -E copy_if_different COPYING ${M2_INSTALL_LICENSESDIR}/polymake
#          COMMAND   ${CMAKE_COMMAND} -E copy_if_different polymake ${M2_INSTALL_PROGRAMSDIR}/
  TEST_COMMAND      ${MAKE} test
  EXCLUDE_FROM_ALL  ON
  TEST_EXCLUDE_FROM_MAIN ON
  STEP_TARGETS      install test
  )
#_ADD_COMPONENT_DEPENDENCY(programs polymake "cddlib;lrs;normaliz;nauty" POLYMAKE)

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
    PATTERN "pkgconfig" EXCLUDE)
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
     MP Arithmetic     = ${MP_LIBRARIES}\n")

# Report the default flags, but if verbose
if(VERBOSE)
  message("## Library compile options:
     CFLAGS            = ${CFLAGS}
     CXXFLAGS          = ${CXXFLAGS}
     LDFLAGS           = ${LDFLAGS}\n")
endif()

# Report the next step
if(BUILD_LIB_LIST OR BUILD_PROG_LIST)
  message(CHECK_FAIL " Some components are missing")
  message("## Rerun build-libraries and build-programs targets first")
else()
  message(CHECK_PASS " Everything is in order! 🎉")
endif()
