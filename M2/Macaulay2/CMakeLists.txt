# TODO: should we add [all,check,clean,distclean]-in-$dir?

set(PACKAGE_VERSION ${Macaulay2_VERSION})
configure_file(README.in README)

## eg: Style Macaulay2Doc ...
## This will be used by d/ and packages/
file(STRINGS "packages/\=distributed-packages" DISTRIBUTED_PACKAGES REGEX "^[a-zA-Z0-9]+$")
list(JOIN DISTRIBUTED_PACKAGES " " DISTRIBUTED_PACKAGES)

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
#add_subdirectory(tests)
#add_subdirectory(book)

# TODO: TYPE DOC is easier but doesn't work because of the common prefix :/ How to make it work?
# FIXME: This happens at install, but the emacs target requires these to exist. See Macaulay2Doc/overview3.m2
#install(FILES COPYING-GPL-2 COPYING-GPL-3 DESTINATION ${M2_COMMON_PREFIX}/${CMAKE_INSTALL_DOCDIR})
execute_process(
  COMMAND ${CMAKE_COMMAND} -E make_directory ${M2_COMMON_PREFIX}/${CMAKE_INSTALL_DOCDIR}/
  COMMAND ${CMAKE_COMMAND} -E copy_if_different COPYING-GPL-2 COPYING-GPL-3 ${M2_COMMON_PREFIX}/${CMAKE_INSTALL_DOCDIR}/
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
