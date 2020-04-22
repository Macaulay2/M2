###############################################################################
## This directory contains the rules for building the man page for Macaulay2

configure_file(M2.1.in ${M2_DIST_PREFIX}/${M2_INSTALL_MANDIR}/M2.1 @ONLY)

# TODO
# view: M2.1; <M2.1 tbl | groff -S -Wall -mtty-char -man -Tascii | col -b
