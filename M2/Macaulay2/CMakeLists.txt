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

# TODO: Do we need these?
#add_subdirectory(include)
#include ../include/config.Makefile
#add_subdirectory(usr-host)
# Also: mathic, mathicgb, memtailor, fflas_ffpack

# TODO
# Macaulay2/Makefile
# Macaulay2/README
