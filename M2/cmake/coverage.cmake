###############################################################################
# See https://clang.llvm.org/docs/SourceBasedCodeCoverage.html
find_program(LLVM_PROFDATA	NAMES llvm-profdata)
find_program(LLVM_COV		NAMES clang-format)

set(_coverage_dir ${CMAKE_BINARY_DIR}/coverage)

FUNCTION (_ADD_COVERAGE _target)
  if(NOT CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    message(FATAL_ERROR "Clang compiler is required for code coverage")
  endif()
  message("## Setting up source-based code coverage flags for target ${_target} ...")

  target_compile_options(${_target} PRIVATE
    -fprofile-instr-generate
    -fcoverage-mapping)
  target_link_options(${_target} PRIVATE
    -fprofile-instr-generate
    -fcoverage-mapping)

  get_target_property(TYPE ${_target} TYPE)

  if(TYPE STREQUAL EXECUTABLE)
    message("## Run the following before running the ${_target} executable:
     export LLVM_PROFILE_FILE=\"${_coverage_dir}/${_target}-%p.profraw\"")

    add_custom_target(coverage-merge-${_target}
      COMMENT "Merging raw coverage data"
      COMMAND llvm-profdata merge ${_target}-*.profraw -o ${_target}.profdata #  -sparse
      WORKING_DIRECTORY ${_coverage_dir}
      )

    add_custom_target(coverage-show-${_target}
      COMMENT "Showing coverage data"
      COMMAND llvm-cov show $<TARGET_FILE:${_target}> -instr-profile=${_target}.profdata
      WORKING_DIRECTORY ${_coverage_dir}
      USES_TERMINAL
      )

    add_custom_target(coverage-report-${_target}
      COMMENT "Reporting coverage data"
      COMMAND llvm-cov report $<TARGET_FILE:${_target}> -instr-profile=${_target}.profdata
      WORKING_DIRECTORY ${_coverage_dir}
      USES_TERMINAL
      )
  endif()
ENDFUNCTION (_ADD_COVERAGE)
