###############################################################################
# Safe C Compiler (SCC)
# In this directory we compile "scc1", the D to C translator, which we want to
# run on the "build" machine, in the sense of autoconf.

set(HEADERS scc.h grammar.h keywords.h
  scc1.h readfile.h error.h dictionary.h list.h cprint.h type.h chk.h compat.h debugging.h)
set(SOURCES
  scc1.c readfile.c error.c dictionary.c list.c cprint.c type.c chk.c compat.c debugging.c)

# Compile the grammar
find_package(BISON REQUIRED QUIET)
BISON_TARGET(scc-grammar
  ${CMAKE_CURRENT_SOURCE_DIR}/grammar.y
  ${CMAKE_CURRENT_BINARY_DIR}/grammar.c
  DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/grammar.h
  COMPILE_FLAGS -yvdlt)

# Generate TAGS file
if(ETAGS)
  set(TAGS TAGS)
  set_source_files_properties(TAGS PROPERTIES GENERATED true)
  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/TAGS
    COMMENT "Generating c/TAGS file"
    COMMAND ${ETAGS} -o TAGS ${HEADERS} ${SOURCES} grammar.y scc-core.h scc-core.c
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endif()

# Compile scc1
add_executable(scc1 ${HEADERS} ${SOURCES} grammar.c ${TAGS})

target_include_directories(scc1 PUBLIC
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:include/Macaulay2>)

if(BDWGC_FOUND AND GDBM_FOUND)
  target_link_libraries(scc1 Threads::Threads
    ${BDWGC_LIBRARIES} ${GDBM_LIBRARY} ${CMAKE_DL_LIBS})
  target_include_directories(scc1 PUBLIC
    $<BUILD_INTERFACE:${BDWGC_INCLUDE_DIR}>
    $<BUILD_INTERFACE:${GDBM_INCLUDE_DIR}>)
endif()

target_compile_options(scc1 PRIVATE
  $<$<COMPILE_LANG_AND_ID:C,GNU>:-Wno-implicit-fallthrough>) # FIXME: caused by type.c:175:26
