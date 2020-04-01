# TODO: should we add [all,check,clean,distclean]-in-$dir?

## eg: Style Macaulay2Doc ...
## This will be used by d/ and packages/
file(STRINGS "packages/\=distributed-packages" DISTRIBUTED_PACKAGES REGEX "^[a-zA-Z0-9]+$")
list(JOIN DISTRIBUTED_PACKAGES " " DISTRIBUTED_PACKAGES)

# TODO: consider using:
#source_group(TREE . PREFIX "Macaulay2" FILES ../include/M2/config.h ...)

# TODO:
#add_subdirectory(util)
add_subdirectory(regex)
add_subdirectory(c)
add_subdirectory(e-includes)
add_subdirectory(d)
add_subdirectory(e)
add_subdirectory(system)
add_subdirectory(kernel)
add_subdirectory(bin)
add_subdirectory(m2)
add_subdirectory(man)
add_subdirectory(packages)
add_subdirectory(emacs)
#add_subdirectory(html-check-links)
#add_subdirectory(book)

if(BUILD_TESTING)
  add_subdirectory(tests)
endif()

configure_file(README.in ${M2_INSTALL_PREFIX}/${M2_INSTALL_DOCDIR}/README @ONLY)
file(COPY LAYOUT COPYING-GPL-2 COPYING-GPL-3 DESTINATION ${M2_INSTALL_PREFIX}/${M2_INSTALL_DOCDIR})

install(DIRECTORY
  ${M2_INSTALL_PREFIX}/${M2_INSTALL_MANDIR}
  ${M2_INSTALL_PREFIX}/${M2_INSTALL_DATADIR}
  ${M2_INSTALL_PREFIX}/${M2_INSTALL_INFODIR}
  ${M2_INSTALL_PREFIX}/${M2_INSTALL_DATAROOTDIR}/doc
  ${M2_INSTALL_PREFIX}/${M2_INSTALL_DATAROOTDIR}/emacs
  DESTINATION ${CMAKE_INSTALL_FULL_DATAROOTDIR}
  COMPONENT common)

install(DIRECTORY ${M2_HOST_PREFIX}/bin
  DESTINATION ${CMAKE_INSTALL_LIBEXECDIR}/Macaulay2
  USE_SOURCE_PERMISSIONS
  COMPONENT programs)

install(FILES
  ${M2_INSTALL_PREFIX}/${M2_INSTALL_BINDIR}/M2
  ${M2_INSTALL_PREFIX}/${M2_INSTALL_BINDIR}/M2-binary
  DESTINATION ${CMAKE_INSTALL_FULL_BINDIR}
  PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE)

# /usr/lib64/Macaulay2
# /usr/libexec/Macaulay2
