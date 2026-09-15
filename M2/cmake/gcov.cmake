###############################################################################
# gcc/gcov code coverage (configure with -DGCOV=ON; the instrumentation flags
# themselves are set in configure.cmake).  Coverage data (.gcda) accumulates
# whenever a coverage-built binary exits, so run ctest or M2 itself first; these
# targets only clear and report on it.

if(GCOV)
  find_program(GCOVR NAMES gcovr)
  set(GCOVR_OPTIONS "" CACHE STRING
    "Extra options passed to gcovr, e.g. --filter Macaulay2/e/")
  separate_arguments(_gcovr_options UNIX_COMMAND "${GCOVR_OPTIONS}")

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
    add_custom_target(coverage-report
      COMMENT "Generating gcov coverage report"
      COMMAND ${CMAKE_COMMAND} -E make_directory ${_coverage_dir}
      # the build tree is a positional search path rather than
      # --object-directory, so that gcovr runs gcov in each data file's own
      # directory and can resolve every source
      COMMAND ${GCOVR} --root ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR}
        --gcov-executable "${GCOV_EXECUTABLE}"
        # the libraries' configure scripts leave .gcno files behind for
        # conftest.c sources they deleted, which gcov cannot resolve
        --exclude-directories ${CMAKE_BINARY_DIR}/libraries
        # gcov may report a function on several lines (e.g. inlines at -O0)
        --merge-mode-functions=merge-use-line-min ${_gcovr_options}
        --html-details ${_coverage_index} --print-summary
      COMMAND ${CMAKE_COMMAND} -E echo "${_coverage_link}"
      USES_TERMINAL VERBATIM)
  endif()
endif()
