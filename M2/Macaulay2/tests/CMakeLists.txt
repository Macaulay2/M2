enable_testing()

set(CMAKE_VERBOSE_MAKEFILE OFF)

#TARGETS = all check status

set(SUBDIRS normal) #ComputationsBook normal slow gigantic goals quarantine)
# engine rationality

# set(M2_gbTrace    5)
# set(M2_errorDepth 0)

## Arguments for M2-binary and installPackage
set(M2_ARGS
  -q --stop --silent
  --no-debug --no-randomize --print-width 140
  -e errorDepth=${M2_errorDepth} -e debugLevel=${M2_debugLevel} -e gbTrace=${M2_gbTrace}
  )
set(M2_PATH "join(path,{\"${CMAKE_CURRENT_SOURCE_DIR}/\",\"${CMAKE_SOURCE_DIR}/packages/\"})")

foreach(GROUP IN LISTS SUBDIRS)

  add_custom_target(test-${GROUP}) # DEPENDS Core)

  ## eg: LU.m2 ...
  file(GLOB ${GROUP}_TESTS RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}/${GROUP} "${GROUP}/*.m2")

  foreach(TESTFILE IN LISTS ${GROUP}_TESTS)
    ## ctest target for testing the package
    set(TESTNAME ${GROUP}-${TESTFILE})

    set(M2_TEST_STRING "input(\"${CMAKE_CURRENT_SOURCE_DIR}/${GROUP}/${TESTFILE}\")$<SEMICOLON>exit(0)")

    add_test(NAME ${TESTNAME}
      COMMAND M2-binary ${M2_ARGS} -e path=${M2_PATH} -e ${M2_TEST_STRING}
      #WORKING_DIRECTORY	${CMAKE_CURRENT_BINARY_DIR}/${GROUP}
      )

#    add_dependencies(test-${GROUP} ${TESTNAME})
  endforeach()

endforeach()

###############################################################################
##### STILL PROCESSING

#define do-in-subdirs
#$(foreach d,$(SUBDIRS),
#	$(eval .PHONY: $1-in-subdirs)
#	$(eval .PHONY $1 $1-in-subdirs:$1-in-$d)
#	$(eval $1-in-$d:; $$(MAKE) -C $d $1 $2))
#endef
#$(foreach t,$(TARGETS),$(eval $(call do-in-subdirs,$t)))
#$(foreach t,$(CLEANTARGETS),$(eval $(call do-in-subdirs,$t,DEPENDS=no)))

#check-deferred:
#	-@ if egrep -nHw 'defer|deferred|disable|disabled' @abs_srcdir@/*.m2 >/dev/null 2>&1 ; \
#	  then echo "make: warning: tests in the following files have been deferred:" >&2 ; \
#	       egrep -nHw 'defer|deferred|disable|disabled' @abs_srcdir@/*.m2 >&2 ; \
#	       false ; \
#	  fi

#docStructure.out: docStructure.m2

#MIKE_TESTS := $(wildcard @srcdir@/mt-*.m2 @srcdir@/gigantic/mt-*.m2)
#MIKE_RESULTS := $(patsubst @srcdir@/%.m2, %.out, $(MIKE_TESTS))
#mike-tests: $(MIKE_RESULTS) ; grep sys $^ 

#PAT := 'internal error|:[0-9][0-9]*:[0-9][0-9]*:\([0-9][0-9]*\):|^GC|^0x|^out of mem|non-zero status|^Command terminated|user.*system.*elapsed|^[0-9]+\.[0-9]+user'
#FILTER := egrep -a $(PAT)

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

#ifeq @DEBUG@ no
#TLIMIT ?= 90
## one test doesn't work with only 150MB RAM, ext-total.m2, on a 64 bit intel machine:
#MLIMIT ?=  800000
#VLIMIT ?= 2000000
#else
## give the debug version more time and space
#TLIMIT ?= 140
#MLIMIT ?= 1200000
#VLIMIT ?= 2000000
#endif
## make the stack limit always the same as it would be under MacOS
#SLIMIT ?= 8192

#LIMIT :=
#ifeq (@ULIMIT_T@,yes)
#LIMIT += ulimit -t $(TLIMIT) ;
#endif
#ifeq (@ULIMIT_M@,yes)
#LIMIT += ulimit -m $(MLIMIT) ;
#endif
#ifeq (@ULIMIT_V@,yes)
#LIMIT += ulimit -v $(VLIMIT) ;
#endif
#ifeq (@ULIMIT_S@,yes)
#LIMIT += ulimit -s $(SLIMIT) ;
#endif
