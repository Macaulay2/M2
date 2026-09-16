###############################################################################
# gcc/gcov code coverage (configure with -DGCOV=ON; the instrumentation flags
# themselves are added per target by _ADD_GCOV below).  Coverage data (.gcda)
# accumulates whenever a coverage-built binary exits, so run ctest or M2 itself
# first; these targets only clear and report on it.  Restrict a report with, e.g.,
#   COVERAGE_SOURCES='Macaulay2/e/matrix.cpp' ninja coverage-report

# coverage-report re-runs this file with `cmake -P`, so that COVERAGE_SOURCES is
# read when the report is generated; everything else is passed in with -D
if(COVERAGE_REPORT)
  set(_filters "")
  if(DEFINED ENV{COVERAGE_SOURCES})
    separate_arguments(_sources UNIX_COMMAND "$ENV{COVERAGE_SOURCES}")
    foreach(_source IN LISTS _sources)
      # gcovr resolves a relative --filter against its own working directory
      if(NOT IS_ABSOLUTE ${_source})
	set(_source ${SOURCE_DIR}/${_source})
      endif()
      list(APPEND _filters --filter ${_source})
    endforeach()
  endif()
  separate_arguments(_gcovr_options UNIX_COMMAND "${GCOVR_OPTIONS}")
  separate_arguments(_merge_mode    UNIX_COMMAND "${MERGE_MODE}")
  file(MAKE_DIRECTORY ${COVERAGE_DIR})
  # the build tree is a positional search path rather than --object-directory,
  # so that gcovr runs gcov in each data file's own directory and can resolve
  # every source
  execute_process(COMMAND ${GCOVR} --root ${SOURCE_DIR} ${BINARY_DIR}
    --gcov-executable "${GCOV_EXECUTABLE}"
    # the libraries' configure scripts leave .gcno files behind for conftest.c
    # sources they deleted, which gcov cannot resolve
    --exclude-directories ${BINARY_DIR}/libraries
    ${_merge_mode} ${_filters} ${_gcovr_options}
    --html-details ${COVERAGE_INDEX} --print-summary
    COMMAND_ERROR_IS_FATAL ANY)
  return()
endif()

if(GCOV)
  set(_gcov_cmake ${CMAKE_CURRENT_LIST_FILE})

  # -O0 keeps the line attribution meaningful; -fprofile-abs-path records
  # absolute source paths, so gcovr can resolve objects built in a parent dir
  function(_ADD_GCOV _target)
    target_compile_options(${_target} PRIVATE --coverage -O0
      $<$<NOT:$<CXX_COMPILER_ID:AppleClang,Clang>>:-fprofile-abs-path>)
    target_link_options(${_target} PRIVATE --coverage)
  endfunction()

  find_program(GCOVR NAMES gcovr)
  set(GCOVR_OPTIONS "" CACHE STRING
    "Extra options passed to gcovr, e.g. --filter Macaulay2/e/")

  # gcovr shells out to gcov, which segfaults or errors out on .gcno files
  # written by a different compiler version, so derive it from the compiler
  # instead of picking up whatever gcov comes first on the path
  set(GCOV_EXECUTABLE "" CACHE STRING
    "gcov program used by the coverage-report target (default: from the compiler)")
  if(NOT GCOV_EXECUTABLE)
    get_filename_component(_cxx_dir ${CMAKE_CXX_COMPILER} DIRECTORY)
    get_filename_component(_cxx_name ${CMAKE_CXX_COMPILER} NAME)
    if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
      # clang writes gcov-format data that only llvm's own shim reads back
      find_program(_gcov NAMES llvm-cov HINTS ${_cxx_dir} NO_CACHE)
      if(_gcov)
	set(_gcov "${_gcov} gcov")
      endif()
    else()
      # e.g. g++-mp-15 -> gcov-mp-15, x86_64-linux-gnu-g++-11 -> ...-gcov-11
      string(REGEX REPLACE "g\\+\\+|gcc|c\\+\\+" "gcov" _gcov_name ${_cxx_name})
      find_program(_gcov NAMES ${_gcov_name} gcov HINTS ${_cxx_dir} NO_CACHE)
    endif()
    set(GCOV_EXECUTABLE "${_gcov}")
  endif()
  if(NOT GCOV_EXECUTABLE)
    message(WARNING "no gcov matching ${CMAKE_CXX_COMPILER} found; "
      "the coverage-report target will likely fail")
    set(GCOV_EXECUTABLE gcov)
  endif()
  message(STATUS "Using GCOV_EXECUTABLE = ${GCOV_EXECUTABLE}")

  set(_coverage_dir ${CMAKE_BINARY_DIR}/coverage)
  set(_coverage_index ${_coverage_dir}/index.html)

  # print the path as an OSC 8 hyperlink, so terminals make it clickable
  string(ASCII 27 _esc)
  cmake_host_system_information(RESULT _host QUERY HOSTNAME)
  set(_coverage_link "coverage report: \
${_esc}]8;;file://${_host}${_coverage_index}${_esc}\\${_coverage_index}${_esc}]8;;${_esc}\\")

  # not engine-scoped: the M2 binary may write .gcda anywhere in the tree
  add_custom_target(coverage-reset
    COMMENT "Deleting accumulated .gcda coverage counters"
    COMMAND find ${CMAKE_BINARY_DIR} -name "*.gcda" -delete
    VERBATIM)

  if(NOT GCOVR)
    message(WARNING "gcovr not found; the coverage-report target will not be created")
  else()
    # --merge-mode-functions needs gcovr 6.0; older gcovr doesn't report
    # function coverage at all, so there is nothing to merge
    execute_process(COMMAND ${GCOVR} --version
      OUTPUT_VARIABLE _gcovr_version_output ERROR_QUIET
      OUTPUT_STRIP_TRAILING_WHITESPACE)
    if(_gcovr_version_output MATCHES "gcovr ([0-9]+\\.[0-9]+)")
      set(_gcovr_version ${CMAKE_MATCH_1})
    endif()
    if(_gcovr_version VERSION_GREATER_EQUAL 6.0)
      # gcov may report a function on several lines (e.g. inlines at -O0)
      set(_gcovr_merge_mode --merge-mode-functions=merge-use-line-min)
    else()
      set(_gcovr_merge_mode "")
      message(STATUS "gcovr ${_gcovr_version} predates 6.0; coverage-report \
will not merge functions reported on several lines")
    endif()

    add_custom_target(coverage-report
      COMMENT "Generating gcov coverage report"
      COMMAND ${CMAKE_COMMAND} -DCOVERAGE_REPORT=ON
        -DGCOVR=${GCOVR} -DGCOV_EXECUTABLE=${GCOV_EXECUTABLE}
        -DGCOVR_OPTIONS=${GCOVR_OPTIONS} -DMERGE_MODE=${_gcovr_merge_mode}
        -DSOURCE_DIR=${CMAKE_SOURCE_DIR} -DBINARY_DIR=${CMAKE_BINARY_DIR}
        -DCOVERAGE_DIR=${_coverage_dir} -DCOVERAGE_INDEX=${_coverage_index}
        -P ${_gcov_cmake}
      COMMAND ${CMAKE_COMMAND} -E echo "${_coverage_link}"
      USES_TERMINAL VERBATIM)
  endif()
endif()
