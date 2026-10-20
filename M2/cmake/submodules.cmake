# check-libraries.cmake has already applied BUILD_LIBRARIES overrides.
# The engine and editor sources are always needed. ExternalProject sources are
# needed only for missing libraries, or to keep testing a previous local build.
function(_m2_select_fallback _name)
  string(TOUPPER "${_name}" _upper)
  set(_build FALSE)
  if(NOT ${_upper}_FOUND AND (NOT _name STREQUAL "googletest" OR BUILD_TESTING))
    set(_build TRUE)
  endif()
  if(EXISTS "${CMAKE_BINARY_DIR}/libraries/${_name}/src/build-${_name}-stamp/build-${_name}-install")
    set(_build TRUE)
  endif()
  set(_m2_build_${_name} ${_build} PARENT_SCOPE)
endfunction()

set(_m2_gitmodules "${CMAKE_SOURCE_DIR}/../.gitmodules")
if(NOT EXISTS "${_m2_gitmodules}")
  if(EXISTS "${CMAKE_SOURCE_DIR}/../.git")
    message(FATAL_ERROR "${_m2_gitmodules} is missing from this Git checkout")
  endif()
  # A source archive may omit .gitmodules. Set the fallback guards from the
  # existing library options; ExternalProject_Add checks their source paths.
  foreach(_library IN LISTS LIBRARY_OPTIONS)
    string(TOLOWER "${_library}" _name)
    if(_name STREQUAL "gtest")
      set(_name googletest)
    endif()
    _m2_select_fallback("${_name}")
  endforeach()
  return()
endif()
file(STRINGS "${_m2_gitmodules}" _m2_gitmodule_paths REGEX "^[ \t]*path[ \t]*=")
# Optional sources use their directory name as the *_FOUND prefix, except
# googletest, which is detected as GTest. Other submodules are always needed.
set(_m2_library_options)
foreach(_library IN LISTS LIBRARY_OPTIONS)
  string(TOUPPER "${_library}" _library_upper)
  list(APPEND _m2_library_options "${_library_upper}")
endforeach()
set(_m2_required_submodules)
set(_m2_update_submodules)
foreach(_entry IN LISTS _m2_gitmodule_paths)
  string(REGEX REPLACE "^[ \t]*path[ \t]*=[ \t]*" "" _path "${_entry}")
  string(STRIP "${_path}" _path)
  if(NOT _path MATCHES "^M2/")
    continue()
  endif()
  string(REGEX REPLACE "^M2/" "" _path "${_path}")

  if(_path MATCHES "^submodules/([^/]+)$")
    set(_name "${CMAKE_MATCH_1}")
    string(TOUPPER "${_name}" _upper)
    set(_option_name "${_upper}")
    if(_name STREQUAL "googletest")
      set(_option_name GTEST)
    endif()
    if(NOT _option_name IN_LIST _m2_library_options)
      # Engine sources have no system-library alternative.
      set(_m2_build_${_name} TRUE)
    else()
      _m2_select_fallback("${_name}")
    endif()
    if(_m2_build_${_name})
      list(APPEND _m2_required_submodules "${_path}")
      list(APPEND _m2_update_submodules "${_path}")
    endif()
  else()
    # Keep the editor submodule's existing warning/skip behavior when absent.
    list(APPEND _m2_update_submodules "${_path}")
  endif()
endforeach()
if(NOT _m2_update_submodules)
  message(FATAL_ERROR "No M2 submodule paths found in ${_m2_gitmodules}")
endif()

if(GIT_SUBMODULE AND GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git")
  message(STATUS "Updating required submodules: ${_m2_update_submodules}")
  execute_process(COMMAND ${GIT_EXECUTABLE} submodule update --init --recursive --
      ${_m2_update_submodules}
    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
    RESULT_VARIABLE _m2_submodule_result)
  if(NOT _m2_submodule_result EQUAL 0)
    message(FATAL_ERROR "Could not update required submodules (${_m2_submodule_result})")
  endif()
endif()

# Support offline builds and source archives containing the required sources.
# Retain the editors' existing warning/skip behavior if Emacs sources are absent.
foreach(_path IN LISTS _m2_required_submodules)
  file(GLOB _m2_submodule_files "${CMAKE_SOURCE_DIR}/${_path}/*")
  # Older non-Git configurations may have left .nogit placeholders in empty
  # submodule directories. Neither those nor .git metadata count as sources.
  list(FILTER _m2_submodule_files EXCLUDE REGEX "/\\.(git|nogit)$")
  if(NOT _m2_submodule_files)
    message(FATAL_ERROR "Required submodule ${_path} is missing. Enable GIT_SUBMODULE or run git submodule update --init --recursive -- ${_path} from ${CMAKE_SOURCE_DIR} before configuring.")
  endif()
endforeach()
