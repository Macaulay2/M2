set(CMAKE_VERBOSE_MAKEFILE OFF)

configure_file(M2.1.in M2.1 @ONLY)

set(CMAKE_INSTALL_MANDIR ${M2_COMMON_PREFIX}/${CMAKE_INSTALL_MANDIR}/man1)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/M2.1 TYPE MAN)

# TODO
# view: M2.1; <M2.1 tbl | groff -S -Wall -mtty-char -man -Tascii | col -b
