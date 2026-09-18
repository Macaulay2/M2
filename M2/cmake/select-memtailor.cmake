# Prefer CMake metadata, with pkg-config as a fallback. Both carry the
# MEMT_DEBUG ABI definition; a library/header search alone cannot establish it.
set(MEMTAILOR_PROVIDER AUTO CACHE STRING "memtailor provider: AUTO, SYSTEM, or BUNDLED")
set_property(CACHE MEMTAILOR_PROVIDER PROPERTY STRINGS AUTO SYSTEM BUNDLED)
if(NOT MEMTAILOR_PROVIDER MATCHES "^(AUTO|SYSTEM|BUNDLED)$")
  message(FATAL_ERROR "MEMTAILOR_PROVIDER must be AUTO, SYSTEM, or BUNDLED")
endif()

set(_memtailor_external FALSE)
set(_memtailor_reason "requested bundled library")
if(NOT MEMTAILOR_PROVIDER STREQUAL "BUNDLED")
  set(_memtailor_target "")
  find_package(memtailor 1.4 CONFIG QUIET)
  if(TARGET memtailor::memtailor)
    set(_memtailor_target memtailor::memtailor)
    set(_memtailor_origin "CMake: ${memtailor_DIR}")
    set(_memtailor_version "${memtailor_VERSION}")
  else()
    find_package(PkgConfig QUIET)
    if(PKG_CONFIG_FOUND)
      # Refresh flags if the installed package changed since configuration.
      unset(M2_MEMTAILOR_FOUND CACHE)
      pkg_check_modules(M2_MEMTAILOR QUIET IMPORTED_TARGET memtailor>=1.4)
      if(TARGET PkgConfig::M2_MEMTAILOR)
        set(_memtailor_target PkgConfig::M2_MEMTAILOR)
        set(_memtailor_origin "pkg-config: ${M2_MEMTAILOR_PREFIX}")
        set(_memtailor_version "${M2_MEMTAILOR_VERSION}")
      endif()
    endif()
  endif()
  set(_memtailor_reason "no memtailor 1.4 CMake or pkg-config package found")
  if(_memtailor_target)
    # Test the effective usage requirements, including definitions conveyed as
    # compiler options by pkg-config, rather than guessing from M2's build type.
    include(CMakePushCheckState)
    include(CheckCXXSourceCompiles)
    cmake_push_check_state(RESET)
    set(CMAKE_REQUIRED_LIBRARIES ${_memtailor_target})
    set(CMAKE_REQUIRED_QUIET TRUE)
    # Recheck when reconfiguring: the installation or its flags may have changed.
    unset(_memtailor_debug_on CACHE)
    unset(_memtailor_debug_off CACHE)
    check_cxx_source_compiles("#include <memtailor.h>
#ifndef MEMT_DEBUG
#error MEMT_DEBUG is off
#endif
int main() { libmemtailorIsPresent(); }" _memtailor_debug_on)
    if(_memtailor_debug_on)
      set(_memtailor_debug ON)
      set(_memtailor_external TRUE)
    else()
      check_cxx_source_compiles("#include <memtailor.h>
#ifdef MEMT_DEBUG
#error MEMT_DEBUG is on
#endif
int main() { libmemtailorIsPresent(); }" _memtailor_debug_off)
      set(_memtailor_debug OFF)
      set(_memtailor_external ${_memtailor_debug_off})
    endif()
    cmake_pop_check_state()
    set(_memtailor_reason "installed memtailor failed its compile/link check")
    if(_memtailor_external)
      message(STATUS "memtailor: detected ${_memtailor_version} (${_memtailor_origin}; MEMT_DEBUG=${_memtailor_debug})")
      # Bundled mathic/mathicgb explicitly enable MEMT_DEBUG in Debug builds.
      if((CMAKE_BUILD_TYPE MATCHES "Debug" OR "Debug" IN_LIST CMAKE_CONFIGURATION_TYPES)
          AND NOT _memtailor_debug)
        set(_memtailor_external FALSE)
        set(_memtailor_reason "installed memtailor has MEMT_DEBUG=OFF, but Debug builds require it ON")
      endif()
    endif()
  endif()
  if(NOT _memtailor_external AND MEMTAILOR_PROVIDER STREQUAL "SYSTEM")
    message(FATAL_ERROR "Cannot use system memtailor: ${_memtailor_reason}")
  endif()
endif()

if(_memtailor_external)
  add_library(memtailor ALIAS ${_memtailor_target})
  set(_bundled_memtailor_targets "")
  message(STATUS "memtailor: using system library (MEMT_DEBUG=${_memtailor_debug})")
else()
  add_subdirectory(memtailor)
  set(_bundled_memtailor_targets memtailor)
  message(STATUS "memtailor: bundled (${_memtailor_reason})")
endif()

# The parent directory also exports the bundled libraries from the build tree.
set(_bundled_memtailor_targets "${_bundled_memtailor_targets}" PARENT_SCOPE)
