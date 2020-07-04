###############################################################################
## In this directory we link the object files compiled in the other directories
## to make the executable, "M2-binary". We also make and install the M2 wrapper,
## whose function is to set (DY)LD_LIBRARY_PATH appropriately so that programs
## called by M2-binary can find the shared libraries they need.

# TODO: plenty of Darwin specific linker flags in Makefile.in

## Configure startup.c
configure_file(startup.c.cmake startup.c @ONLY)

###############################################################################
## Generate TAGS file
# TODO: the order is somewhat different, is it a problem?

if(ETAGS)
  set(TAGS TAGS)
  set_source_files_properties(TAGS PROPERTIES GENERATED true)
  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/TAGS
    COMMENT "Generating bin/TAGS file"
    COMMAND ${ETAGS} -o TAGS timestamp.cpp main.cpp ${CMAKE_CURRENT_BINARY_DIR}/startup.c
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()

###############################################################################
## Generate man pages for M2 and M2-binary

configure_file(../man/M2.1.in M2.1 @ONLY)

set(MAN ${M2_DIST_PREFIX}/${M2_INSTALL_MANDIR}/man1/M2.1.gz)
add_custom_command(OUTPUT ${MAN}
  COMMENT "Creating man page for M2 and M2-binary"
  COMMAND gzip -nf9 M2.1
  COMMAND mv M2.1.gz ${M2_DIST_PREFIX}/${M2_INSTALL_MANDIR}/man1/M2.1.gz)

# Creating symlink to prevent lintian binary-without-manpage warning
file(MAKE_DIRECTORY ${M2_DIST_PREFIX}/${M2_INSTALL_MANDIR}/man1)
file(CREATE_LINK M2.1.gz ${M2_DIST_PREFIX}/${M2_INSTALL_MANDIR}/man1/M2-binary.1.gz SYMBOLIC)

# Preview the man page
add_custom_target(man COMMAND zcat ${MAN} | groff -man -Tascii DEPENDS ${MAN})

###############################################################################
## Build M2-binary

add_executable(M2-binary timestamp.cpp main.cpp
  ${CMAKE_CURRENT_BINARY_DIR}/startup.c ${TAGS} ${MAN})
target_link_libraries(M2-binary
  Boost::${Boost_stacktrace}
  M2-engine M2-interpreter M2-supervisor ${CMAKE_DL_LIBS})

# Force using libboost_stacktrace library
target_compile_definitions(M2-binary PUBLIC BOOST_STACKTRACE_LINK)

# Set where the executable should be store and get its RPATH
set_target_properties(M2-binary PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR})
get_target_property(INSTALL_RPATH M2-binary INSTALL_RPATH)

# Export the target
install(TARGETS M2-binary EXPORT Macaulay2
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  COMPONENT ALL)

###############################################################################
## After linking, print a list of all shared libraries M2-binary uses

find_program(LDD ldd)
find_program(OTOOL otool)
if(LDD)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E echo "-- Linked libraries:"
    COMMAND ${SET_LD_LIBRARY_PATH} ${LDD} $<TARGET_FILE:M2-binary>
    COMMAND ${CMAKE_COMMAND} -E echo "-- INSTALL_RPATH: ${INSTALL_RPATH}")
elseif(OTOOL)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E echo "-- Linked libraries:"
    COMMAND ${SET_LD_LIBRARY_PATH} ${OTOOL} -L $<TARGET_FILE:M2-binary>
    COMMAND ${CMAKE_COMMAND} -E echo "-- INSTALL_RPATH: ${INSTALL_RPATH}")
endif()

# TODO: strip args
# objdump -x M2-binary | grep RPATH
# objcopy --only-keep-debug Macaulay2/bin/M2-binary M2.debug-info
# see https://github.com/Macaulay2/M2/issues/212

###############################################################################
# Build the M2 wrapper, which sets the library path, mainly for normaliz, etc.
# TODO: find a way to make this obsolete

if(APPLE)
  set(EXPORT_STRING [[DYLD_LIBRARY_PATH=`dirname "$0"`/../@CMAKE_INSTALL_LIBDIR@/Macaulay2/lib:$DYLD_LIBRARY_PATH]])
else()
  set(EXPORT_STRING   [[LD_LIBRARY_PATH=`dirname "$0"`/../@CMAKE_INSTALL_LIBDIR@/Macaulay2/lib:$LD_LIBRARY_PATH]])
endif()

string(CONFIGURE "#!/bin/sh\n${EXPORT_STRING} `dirname \"$0\"`/M2@EXE@ \"$@\"" M2_CONTENT @ONLY)
file(MAKE_DIRECTORY ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR})
execute_process(
  COMMAND ${CMAKE_COMMAND} -E echo "${M2_CONTENT}"
  OUTPUT_FILE ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
execute_process(
  COMMAND chmod +x ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
