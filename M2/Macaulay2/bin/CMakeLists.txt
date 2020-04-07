# In this directory we link the object files compiled in the other directories to
# make the executable, "M2-binary".  We also make and install the shell script wrapper,
# "M2", whose function is to set LD_LIBRARY_PATH appropriately.

set(CMAKE_VERBOSE_MAKEFILE OFF)

# TODO: do we still want to use M2${EXE} defined in cmake/config.cmake?
# TODO: move main.c here
add_executable(M2-binary timestamp.cpp ../d/main.c)

set_target_properties(M2-binary PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR})
get_target_property(INSTALL_RPATH M2-binary INSTALL_RPATH)

# NOTE: is d/debug.o extra?
target_link_libraries(M2-binary
  M2-regex M2-interpreter M2-engine M2-system M2-kernel
  Threads::Threads ${LIBGDBM} ${CMAKE_DL_LIBS}) # TODO: handle the latter two differently

foreach(LIB IN LISTS LIBRARY_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-binary ${${LIB}_LIBRARY})
  endif()
endforeach()

foreach(LIB IN LISTS LIBRARIES_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-binary ${${LIB}_LIBRARIES})
  endif()
endforeach()

foreach(LIB IN LISTS PKGLIB_LIST)
  if(${LIB}_FOUND)
    target_link_libraries(M2-binary PkgConfig::${LIB})
  endif()
endforeach()

###############################################################################

find_program(LDD ldd)
find_program(OTOOL otool)
if(LDD)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMENT "## Linked libraries:"
    COMMAND ${SET_LD_LIBRARY_PATH} ${LDD} $<TARGET_FILE:M2-binary>)
elseif(OTOOL)
  add_custom_command(TARGET M2-binary POST_BUILD
    COMMENT "## Linked libraries:"
    COMMAND ${SET_LD_LIBRARY_PATH} ${OTOOL} -L $<TARGET_FILE:M2-binary>)
endif()
add_custom_command(TARGET M2-binary POST_BUILD
  COMMAND ${CMAKE_COMMAND} -E echo "## INSTALL_RPATH:" ${INSTALL_RPATH})

# TODO: strip args
# objdump -x M2-binary | grep RPATH
# objcopy --only-keep-debug Macaulay2/bin/M2-binary M2.debug-info

###############################################################################

file(CREATE_LINK M2-binary ${CMAKE_BINARY_DIR}/M2 SYMBOLIC)
file(CREATE_LINK M2-binary ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2 SYMBOLIC)
