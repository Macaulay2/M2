# Prefer the installed CMake package: its target carries the MEMT_DEBUG ABI
# definition. A library/header search alone cannot establish that ABI.
set(MEMTAILOR_PROVIDER AUTO CACHE STRING "memtailor provider: AUTO, SYSTEM, or BUNDLED")
set_property(CACHE MEMTAILOR_PROVIDER PROPERTY STRINGS AUTO SYSTEM BUNDLED)
if(NOT MEMTAILOR_PROVIDER MATCHES "^(AUTO|SYSTEM|BUNDLED)$")
  message(FATAL_ERROR "MEMTAILOR_PROVIDER must be AUTO, SYSTEM, or BUNDLED")
endif()

set(_memtailor_external FALSE)
set(_memtailor_reason "requested bundled library")
if(NOT MEMTAILOR_PROVIDER STREQUAL "BUNDLED")
  find_package(memtailor 1.4 CONFIG QUIET)
  set(_memtailor_reason "no memtailor 1.4 CMake package found")
  if(TARGET memtailor::memtailor)
    set(_memtailor_external TRUE)
    # The bundled mathic/mathicgb explicitly enable MEMT_DEBUG in Debug builds.
    # Do not mix their debug layouts with a release memtailor library.
    get_target_property(_memtailor_definitions memtailor::memtailor INTERFACE_COMPILE_DEFINITIONS)
    if((CMAKE_BUILD_TYPE MATCHES "Debug" OR "Debug" IN_LIST CMAKE_CONFIGURATION_TYPES)
        AND NOT "MEMT_DEBUG" IN_LIST _memtailor_definitions)
      set(_memtailor_external FALSE)
      set(_memtailor_reason "installed memtailor does not provide the MEMT_DEBUG ABI required by Debug builds")
    endif()
  endif()
  if(NOT _memtailor_external AND MEMTAILOR_PROVIDER STREQUAL "SYSTEM")
    message(FATAL_ERROR "Cannot use system memtailor: ${_memtailor_reason}")
  endif()
endif()

if(_memtailor_external)
  add_library(memtailor ALIAS memtailor::memtailor)
  set(_bundled_memtailor_targets "")
  message(STATUS "memtailor: system ${memtailor_VERSION} (${memtailor_DIR})")
else()
  add_subdirectory(memtailor)
  set(_bundled_memtailor_targets memtailor)
  message(STATUS "memtailor: bundled (${_memtailor_reason})")
endif()

# The parent directory also exports the bundled libraries from the build tree.
set(_bundled_memtailor_targets "${_bundled_memtailor_targets}" PARENT_SCOPE)
