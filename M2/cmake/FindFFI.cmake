# Attempts to discover ffi library with a linkable ffi_call function.
# See https://sourceware.org/libffi/
#
# This module defines an imported target FFI:ffi when called:
#  find_package(FFI)
# FFI_ROOT may be set to define search paths for the ffi library.
#
# Once done this will define
#
#  FFI_FOUND		- system has the FFI library with correct version
#  FFI_INCLUDE_DIRS	- the FFI include directory
#  FFI_LIBRARIES	- the FFI library
#  FFI_VERSION		- FFI version
#  HAVE_FFI_CALL	- whether ffi_call is linkable

if(NOT FFI_FOUND)
  # First look for hints through pkg-config
  find_package(PkgConfig QUIET)
  if(PKG_CONFIG_FOUND)
    pkg_check_modules(FFI QUIET libffi)
  endif()
  unset(FFI_INCLUDE_DIRS CACHE)
  unset(FFI_LIBRARIES CACHE)

  # Then find the paths using CMake routines
  find_path(FFI_INCLUDE_DIRS ffi.h HINTS ${FFI_INCLUDEDIR})
  find_library(FFI_LIBRARIES ffi   HINTS ${FFI_LIBDIR})

  # Ensure that ffi_call is linkable
  if(FFI_LIBRARIES)
    include(CMakePushCheckState)
    include(CheckCSourceCompiles)
    cmake_push_check_state()
    list(APPEND CMAKE_REQUIRED_INCLUDES  "${FFI_INCLUDE_DIRS}")
    list(APPEND CMAKE_REQUIRED_LIBRARIES "${FFI_LIBRARIES}")
    check_c_source_compiles([[struct ffi_cif;
      typedef struct ffi_cif ffi_cif;
      void ffi_call(ffi_cif *cif, void (*fn)(void), void *rvalue, void **avalue);
      int main() { ffi_call(0, 0, 0, 0); }]] HAVE_FFI_CALL)
    cmake_pop_check_state()
  endif()

  if(NOT FFI_VERSION)
    set(FFI_VERSION "version not specified")
  endif()

  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(FFI DEFAULT_MSG FFI_INCLUDE_DIRS FFI_LIBRARIES HAVE_FFI_CALL)

  mark_as_advanced(FFI_LIBRARIES FFI_INCLUDE_DIRS FFI_LIBRARIES HAVE_FFI_CALL)
endif()

if(FFI_FOUND)
  add_library(FFI::ffi UNKNOWN IMPORTED)
  set_target_properties(FFI::ffi PROPERTIES IMPORTED_LOCATION "${FFI_LIBRARIES}")
  set_target_properties(FFI::ffi PROPERTIES INTERFACE_INCLUDE_DIRSECTORIES "${FFI_INCLUDE_DIRS}")
endif()
