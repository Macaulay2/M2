set(CMAKE_VERBOSE_MAKEFILE OFF)

## Try:
# cd Macaulay2/tests
# ctest -N
# ctest --build-and-test
# ctest --build-run-dir
# ctest -R schorder --rerun-failed -V

set(GC_MAXIMUM_HEAP_SIZE 400M CACHE STRING "maximum collected heap size")

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

include(CTest)

###############################################################################

# TODO: some targets have different resource limits
# TODO: different rules: ComputationsBook rationality
set(SUBDIRS normal slow gigantic goals quarantine engine)

# set(M2_gbTrace    5)
# set(M2_errorDepth 0)

## Arguments for M2-binary
set(M2_ARGS
  -q --stop --silent --no-debug --no-randomize # --no-prompts --no-...
  -e errorDepth=${M2_errorDepth} -e debugLevel=${M2_debugLevel} -e gbTrace=${M2_gbTrace}
  )
set(M2_PATH "join(path,{\"${CMAKE_CURRENT_SOURCE_DIR}/\",\"${CMAKE_SOURCE_DIR}/packages/\"})")

## Regular expressions for various test errors
## TODO: add WILL_FAIL tests
set(M2_FAIL_REGEX
  [[internal error;non-zero status;assertion failed]]
  "\nCommand terminated;\nGC;\n0x;\nout of mem"
  "user.*system.*elapsed;\n[0-9]+\.[0-9]+user"
  [[/.*:[0-9][0-9]*:[0-9][0-9]*:\([0-9][0-9]*\):]] # TODO: this one catches commented errors
  )

###############################################################################

foreach(GROUP IN LISTS SUBDIRS)

  ## eg: LU.m2 ...
  file(GLOB ${GROUP}_TESTS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/${GROUP} "${GROUP}/*.m2")

  foreach(TESTFILE IN LISTS ${GROUP}_TESTS)
    ## ctest target for testing the package
    set(TESTNAME ${GROUP}/${TESTFILE})

    # Read the file, then exit. Note: a "restart" command causes the test to not halt
    set(M2_TEST_STRING "input(\"${CMAKE_CURRENT_SOURCE_DIR}/${GROUP}/${TESTFILE}\")$<SEMICOLON>exit(0)")

    add_test(NAME ${TESTNAME}
      COMMAND M2-binary ${M2_ARGS} -e path=${M2_PATH} -e ${M2_TEST_STRING}
      WORKING_DIRECTORY	${CMAKE_CURRENT_SOURCE_DIR}/${GROUP}
      )

    set_tests_properties(${TESTNAME} PROPERTIES
      FAIL_REGULAR_EXPRESSION "${M2_FAIL_REGEX}"
      ENVIRONMENT "GC_MAXIMUM_HEAP_SIZE=${GC_MAXIMUM_HEAP_SIZE}"
      TIMEOUT ${CTEST_TEST_TIMEOUT} # TODO: this shouldn't be necessary
      )
  endforeach()

endforeach()

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
