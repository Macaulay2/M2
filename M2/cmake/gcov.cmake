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
      COMMAND ${CMAKE_COMMAND} -E make_directory ${_coverage_dir}
      # the build tree is a positional search path rather than
      # --object-directory, so that gcovr runs gcov in each data file's own
      # directory and can resolve every source
      COMMAND ${GCOVR} --root ${CMAKE_SOURCE_DIR} ${CMAKE_BINARY_DIR}
        # the libraries' configure scripts leave .gcno files behind for
        # conftest.c sources they deleted, which gcov cannot resolve
        --exclude-directories ${CMAKE_BINARY_DIR}/libraries
        ${_gcovr_merge_mode} ${_gcovr_options}
        --html-details ${_coverage_index} --print-summary
      COMMAND ${CMAKE_COMMAND} -E echo "${_coverage_link}"
      USES_TERMINAL VERBATIM)
  endif()
endif()
