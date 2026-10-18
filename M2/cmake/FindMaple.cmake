#[=======================================================================[.rst:
FindMaple
---------
Find a usable command-line Maple, optionally with the Convex component.

Inputs: MAPLE_EXECUTABLE, MAPLE_CONVEX_DIR (extra Maple library directory),
MAPLE_PROBE_TIMEOUT (seconds). Results: Maple_FOUND, MAPLE_WORKS,
Maple_Convex_FOUND. No Maple installation is required or downloaded.
#]=======================================================================]

find_program(MAPLE_EXECUTABLE NAMES maple cmaple)
set(MAPLE_CONVEX_DIR "" CACHE PATH "Additional Maple library directory containing convex")
set(MAPLE_PROBE_TIMEOUT 10 CACHE STRING "Timeout in seconds for each Maple execution probe")
mark_as_advanced(MAPLE_EXECUTABLE MAPLE_CONVEX_DIR MAPLE_PROBE_TIMEOUT)

# Recheck at every configure: a cached executable may have lost its license.
set(MAPLE_WORKS FALSE)
set(Maple_Convex_FOUND FALSE)
if(MAPLE_EXECUTABLE AND NOT CMAKE_CROSSCOMPILING)
  set(_maple_probe_dir "${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/Maple")
  file(MAKE_DIRECTORY "${_maple_probe_dir}")
  file(WRITE "${_maple_probe_dir}/probe.mpl"
    "printf(\"M2_MAPLE_%d\\n\", 6*7):\nquit;\n")
  execute_process(COMMAND "${MAPLE_EXECUTABLE}" -q
    INPUT_FILE "${_maple_probe_dir}/probe.mpl"
    OUTPUT_VARIABLE _maple_output ERROR_VARIABLE _maple_error
    RESULT_VARIABLE _maple_status TIMEOUT "${MAPLE_PROBE_TIMEOUT}")
  # An executable (or a license failure returning zero) is not enough. Require
  # computed output, not text that could appear in an echoed input statement.
  if("${_maple_status}" STREQUAL "0" AND
      _maple_output MATCHES "(^|[\r\n])M2_MAPLE_42([\r\n]|$)")
    set(MAPLE_WORKS TRUE)
  endif()
  file(WRITE "${_maple_probe_dir}/probe.log"
    "Status: ${_maple_status}\n${_maple_output}\n${_maple_error}")

  if(MAPLE_WORKS AND "Convex" IN_LIST Maple_FIND_COMPONENTS)
    # Quote the directory as a Maple string, not a shell argument.
    string(REPLACE "\\" "\\\\" _maple_convex_dir "${MAPLE_CONVEX_DIR}")
    string(REPLACE "\"" "\\\"" _maple_convex_dir "${_maple_convex_dir}")
    set(_maple_library_path "")
    if(MAPLE_CONVEX_DIR)
      set(_maple_library_path "libname := libname, \"${_maple_convex_dir}\":\n")
    endif()
    file(WRITE "${_maple_probe_dir}/convex.mpl"
      "${_maple_library_path}try\nwith(convex):\nprintf(\"M2_CONVEX_%d\\n\", 6*7):\ncatch:\nend try:\nquit;\n")
    execute_process(COMMAND "${MAPLE_EXECUTABLE}" -q
      INPUT_FILE "${_maple_probe_dir}/convex.mpl"
      OUTPUT_VARIABLE _maple_output ERROR_VARIABLE _maple_error
      RESULT_VARIABLE _maple_status TIMEOUT "${MAPLE_PROBE_TIMEOUT}")
    if("${_maple_status}" STREQUAL "0" AND
        _maple_output MATCHES "(^|[\r\n])M2_CONVEX_42([\r\n]|$)")
      set(Maple_Convex_FOUND TRUE)
    endif()
    file(WRITE "${_maple_probe_dir}/convex.log"
      "Status: ${_maple_status}\n${_maple_output}\n${_maple_error}")
  endif()
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(Maple
  REQUIRED_VARS MAPLE_EXECUTABLE MAPLE_WORKS HANDLE_COMPONENTS)
