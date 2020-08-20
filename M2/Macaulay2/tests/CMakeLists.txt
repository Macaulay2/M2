###############################################################################
## This directory contains the rules for adding various tests to CTest.
##
## - list tests:      ctest -N
## - build and test:  ctest --build-and-test
## - rerun matching tests:
##     ctest -R schorder --rerun-failed -V

# TODO: some targets have different resource limits
set(SUBDIRS normal slow gigantic goals quarantine engine threads)

add_custom_target(M2-tests)
add_dependencies(M2-tests M2-tests-ComputationsBook) # M2-tests-rationality

###############################################################################
## Default resource limits
# TODO: add memory, virtual memory, and stack size
# the tests aren't run through bash, so ulimit wouldn't work
if(DEBUG)
  set(CTEST_TEST_TIMEOUT 1400)
  #MLIMIT ?= 1200000
  #VLIMIT ?= 2000000
else()
  set(CTEST_TEST_TIMEOUT 900)
  ## one test doesn't work with only 150MB RAM, ext-total.m2, on a 64 bit intel machine:
  #MLIMIT ?=  800000
  #VLIMIT ?= 2000000
endif()
## make the stack limit always the same as it would be under MacOS
#SLIMIT ?= 8192

# TODO: get this to work
# See https://gitlab.kitware.com/cmake/community/-/wikis/doc/ctest/Testing-With-CTest
if(MEMDEBUG)
  find_program(MEMORYCHECK_COMMAND valgrind)
  set( MEMORYCHECK_COMMAND_OPTIONS "--trace-children=yes --leak-check=full --show-reachable=yes --track-origins=yes" )
endif()

###############################################################################
## M2 arguments and commands that will be passed to M2 -e ${STR}
set(GC_MAXIMUM_HEAP_SIZE 400M CACHE STRING "maximum collected heap size")
set(M2 ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
# gbTrace, errorDepth, and debugLevel are set in packages/CMakeLists.txt
set(M2_ARGS
  -q --stop --silent --no-debug --no-randomize # --no-prompts --no-...
  -e errorDepth=${errorDepth} -e debugLevel=${debugLevel} -e gbTrace=${gbTrace})
set(M2_PATH "join(path,{\"${CMAKE_CURRENT_SOURCE_DIR}/\",\"${CMAKE_SOURCE_DIR}/packages/\"})")
# Read the file, then exit. Note: a "restart" command causes the test to not halt
set(M2_TEST_TEMPLATE [[input("@CMAKE_CURRENT_SOURCE_DIR@/@_group@/@_testfile@")]])

# Allow GC_MAXIMUM_HEAP_SIZE to be set via environment variable
foreach(_var in ITEMS GC_MAXIMUM_HEAP_SIZE)
  if(DEFINED ENV{${_var}})
    message("## Set via environment:   ${_var}\t= $ENV{${_var$}} (default: ${${_var}})")
    set(${_var} "$ENV{${_var}}")
  endif()
endforeach()

## Regular expressions for various test errors
## TODO: add WILL_FAIL tests
set(M2_FAIL_REGEX
  [[internal error;non-zero status;assertion failed]]
  "--back trace--"
  "\nCommand terminated;\nGC;\n0x;\nout of mem"
  "user.*system.*elapsed;\n[0-9]+\.[0-9]+user"
  [[\.m2:[0-9][0-9]*:[0-9][0-9]*:\([0-9][0-9]*\):]] # TODO: this one catches commented errors
  )

###############################################################################
## Adding tests for all subdirectories

foreach(_group IN LISTS SUBDIRS)

  ## eg: LU.m2 ...
  file(GLOB ${_group}_TESTS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/${_group} "${_group}/*.m2")

  foreach(_testfile IN LISTS ${_group}_TESTS)
    ## ctest target for testing the package
    set(_testname ${_group}/${_testfile})

    ## configure the test string
    string(CONFIGURE "${M2_TEST_TEMPLATE}" M2_TEST_STRING)

    add_test(NAME ${_testname}
      COMMAND ${M2} ${M2_ARGS} -e path=${M2_PATH} -e ${M2_TEST_STRING} -e "exit 0"
      WORKING_DIRECTORY	${CMAKE_CURRENT_SOURCE_DIR}/${_group}
      )

    set_tests_properties(${_testname} PROPERTIES
      FAIL_REGULAR_EXPRESSION "${M2_FAIL_REGEX}"
      ENVIRONMENT "GC_MAXIMUM_HEAP_SIZE=${GC_MAXIMUM_HEAP_SIZE}"
      TIMEOUT ${CTEST_TEST_TIMEOUT} # TODO: this shouldn't be necessary
      )
  endforeach()

endforeach()

###############################################################################
## ComputationsBook targets:
# - M2-tests-ComputationsBook-${_chapter}
# - M2-tests-ComputationsBook-${_chapter}-changes
# - M2-tests-ComputationsBook-${_chapter}-diff
# Tests:
# - ComputationsBook/${_chapter}

add_custom_target(M2-tests-ComputationsBook)

set(CHAPTERS completeIntersections constructions d-modules exterior-algebra geometry
  monomialIdeals preface programming schemes solving toricHilbertScheme varieties)

set(M2_ARGS -q --stop --print-width 0)

foreach(_chapter IN LISTS CHAPTERS)

  ## configure the test string
  set(_testfile test.m2)
  set(_group ComputationsBook/${_chapter})

  set(book_patterns    ${CMAKE_CURRENT_SOURCE_DIR}/${_group}/../patterns)
  set(chapter_patterns ${CMAKE_CURRENT_SOURCE_DIR}/${_group}/patterns)

  string(CONFIGURE "${M2_TEST_TEMPLATE}" M2_TEST_STRING)
  set(M2_PATH "join({\"${CMAKE_CURRENT_BINARY_DIR}/${_group}\",\"${CMAKE_CURRENT_SOURCE_DIR}/${_group}\"},path)")

  if(NOT EXISTS ${CMAKE_CURRENT_BINARY_DIR}/${_group})
    file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${_group})
  endif()

  ###############################################################################
  # we depend on "diff" accepting some options that perhaps only GNU diff does
  add_custom_target(M2-tests-ComputationsBook-${_chapter}
    COMMAND ${M2} ${M2_ARGS} -e path=${M2_PATH} -e ${M2_TEST_STRING} -e "exit 0" > test.out
    COMMAND sed -f ${book_patterns} -f ${chapter_patterns} > test.out.trim < test.out
    COMMAND sed -f ${book_patterns} -f ${chapter_patterns} > test.out.expected.trim
      < ${CMAKE_CURRENT_SOURCE_DIR}/${_group}/test.out.expected
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${_group}
    VERBATIM
    )
  add_custom_target(M2-tests-ComputationsBook-${_chapter}-diff
    COMMAND diff -B -b -u test.out.expected.trim test.out.trim
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${_group}
    DEPENDS M2-tests-ComputationsBook-${_chapter}
    VERBATIM
    )
  add_custom_target(M2-tests-ComputationsBook-${_chapter}-changes
    COMMAND diff -u chapter.m2 test.m2 || true
    COMMAND diff -u chapter.out.expected test.out.expected || true
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${_group}
    VERBATIM
    )

  add_dependencies(M2-tests-ComputationsBook M2-tests-ComputationsBook-${_chapter})

  ###############################################################################
  add_test(NAME ${_group}
    COMMAND diff -B -b -u
      ${CMAKE_CURRENT_BINARY_DIR}/${_group}/test.out.expected.trim
      ${CMAKE_CURRENT_BINARY_DIR}/${_group}/test.out.trim
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${_group}
    )

  # TODO: capture output from the chapter's code with M2 version 0.9.2 as a standard of comparison.
  # this requires an old binary, but we run it only once and save the output
  #set(OLDM2 M2-0.9.2)
  #set(OLDM2CMD ${LIMIT} time nice -18 ${OLDM2} -s -q -e${PATHJOIN})
  #add_custom_command(OUTPUT test.oldout test.oldvalues
  #  COMMAND ${OLDM2CMD} -e'load "$(chapter_srcdir)/../capture.m2"; input "$<"; exit 0'  >test.oldout.tmp </dev/null
  #  COMMAND mv test.oldout.tmp test.oldout
  #  DEPENDS chapter.m2 ../capture.m2
  #  )
  #add_custom_target(ComputationsBook-capture
  #  DEPENDS test.oldout test.oldvalues
  #  )

endforeach()

# TODO: add_subdirectory(rationality)

###############################################################################
##### STILL PROCESSING

#status: status-files
#status-files: $(TESTFILES)
#	@ egrep -n '^--status:' $(TESTFILES) /dev/null || true

#%.out : %.m2
#	@ echo testing: $<
#	@ $(LIMIT) \
#		echo "--*- compilation -*-" >$*.errors; \
#		if (echo 'input "$<"'; echo 'collectGarbage()'; echo exit 0) | \
#		   GC_MAXIMUM_HEAP_SIZE=400M time @pre_exec_prefix@/bin/M2 $(ARGS) >>$*.errors 2>&1 ; \
#		then mv $*.errors $@ ; \
#		else a=$$?; \
#		     <$*.errors $(FILTER) ; \
#		     echo "$*.errors:0: error output left here for the errors above:" >&2 ; \
#		     egrep -n '^--status:' $< /dev/null || true ; \
#		     exit $$a ; \
#		fi

#%.out : %.m2-input
#	@ echo testing: $<
#	@ egrep -n '^--status:' $< /dev/null || true
#	@ $(LIMIT) \
#		echo "--*- compilation -*-" >$*.errors; \
#		if GC_MAXIMUM_HEAP_SIZE=400M time @pre_exec_prefix@/bin/M2 $(ARGS) <$< >>$*.errors 2>&1 ; \
#		then mv $*.errors $@ ; \
#		else a=$$?; \
#		     echo "$*.errors:0: error output left here; some errors follow" >&2 ; \
#		     <$*.errors $(FILTER) ; \
#		     exit $$a ; \
#		fi

#review:
#	@ echo error: `ls *.errors | wc -l` errors occurred
#	@ echo summary of errors:
#	@ for i in *.errors ; \
#	  do echo `basename $$i .errors`.m2: ; \
#	     <$$i $(FILTER) ; \
#	  done

#clean:; rm -f *.okay *.out core *.errors
