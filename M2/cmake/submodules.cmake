# check-libraries.cmake has already applied BUILD_LIBRARIES overrides.
# The bundled engine sources are always needed. ExternalProject sources are
# needed only for missing libraries, or to keep testing a previous local build.
set(_m2_required_submodules submodules/memtailor submodules/mathic submodules/mathicgb)
foreach(_name IN ITEMS bdwgc flint frobby givaro fflas_ffpack googletest)
  string(TOUPPER "${_name}" _upper)
  set(_m2_build_${_name} FALSE)
  if(NOT ${_upper}_FOUND AND (NOT _name STREQUAL "googletest" OR BUILD_TESTING))
    set(_m2_build_${_name} TRUE)
  endif()
  if(EXISTS "${CMAKE_BINARY_DIR}/libraries/${_name}/src/build-${_name}-stamp/build-${_name}-install")
    set(_m2_build_${_name} TRUE)
  endif()
  if(_m2_build_${_name})
    list(APPEND _m2_required_submodules submodules/${_name})
  endif()
endforeach()

if(GIT_SUBMODULE AND GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git")
  message(STATUS "Updating required submodules: ${_m2_required_submodules}")
  execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive --
      ${_m2_required_submodules}
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    RESULT_VARIABLE _m2_submodule_result)
  if(NOT _m2_submodule_result EQUAL 0)
    message(FATAL_ERROR "Could not update required submodules (${_m2_submodule_result})")
  endif()
endif()

# Support offline builds and source archives containing the required sources.
foreach(_path IN LISTS _m2_required_submodules)
  file(GLOB _m2_submodule_files "${CMAKE_SOURCE_DIR}/${_path}/*")
  list(FILTER _m2_submodule_files EXCLUDE REGEX "/\\.(git|nogit)$")
  if(NOT _m2_submodule_files)
    message(FATAL_ERROR "Required submodule ${_path} is missing. Enable GIT_SUBMODULE or run git submodule update --init --recursive -- ${_path} from ${CMAKE_SOURCE_DIR} before configuring.")
  endif()
endforeach()
