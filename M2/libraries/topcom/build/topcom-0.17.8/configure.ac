dnl Process this file with autoconf to produce a configure script.
AC_INIT(TOPCOM, 0.17.8)
AC_CONFIG_SRCDIR(.)

dnl Init automake.
AM_INIT_AUTOMAKE

dnl Checks for programs.
AC_PROG_CC
AC_PROG_INSTALL
AC_PROG_CXX
AC_PROG_RANLIB

dnl Checks for libraries.
dnl Replace `main' with a function in -lg:
dnl AC_CHECK_LIB(g, main)
dnl Replace `main' with a function in -lstdc:
dnl AC_CHECK_LIB(stdc, main)
dnl Replace `main' with a function in -lstdc++:
dnl AC_CHECK_LIB(stdc++, main)
dnl Make libgmp:
dnl make -C external gmp
dnl Make libcddgmp:
dnl make -C external cdd
dnl Check for soplex:
#AC_CHECK_LIB(soplex, main, , [echo soplex not found: --soplex will not work.])

dnl Checks for header files.
AC_HEADER_STDC

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

dnl Checks for library functions.

AC_OUTPUT(wrap-gmp-gmpxx/Makefile lib-src/Makefile lib-src-reg/Makefile src/Makefile src-reg/Makefile examples/Makefile Makefile)
