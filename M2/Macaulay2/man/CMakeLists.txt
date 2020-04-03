# TODO: add all documentation building here?

set(CMAKE_VERBOSE_MAKEFILE OFF)

configure_file(M2.1.in ${M2_DIST_PREFIX}/${M2_INSTALL_MANDIR}/M2.1 @ONLY)

# TODO
# view: M2.1; <M2.1 tbl | groff -S -Wall -mtty-char -man -Tascii | col -b
