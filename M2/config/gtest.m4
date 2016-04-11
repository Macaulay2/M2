# Copyright (C) 2012 Canonical, Ltd.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice (including the next
# paragraph) shall be included in all copies or substantial portions of the
# Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# Checks whether the gtest source is available on the system. Allows for
# adjusting the include and source path. Sets have_gtest=yes if the source is
# present. Sets GTEST_CPPFLAGS and GTEST_SOURCE to the preprocessor flags and
# source location respectively.
AC_DEFUN([CHECK_GTEST],
[
  AC_ARG_WITH([gtest-source-path],
              [AS_HELP_STRING([--with-gtest-source-path],
                              [location of the Google test sources, defaults to /usr/src/gtest])],
              [GTEST_SOURCE="$withval"; GTEST_CPPFLAGS="-I$withval/include";
               case "$withval" in
                  /*) ;;
                  *) AC_MSG_ERROR([gtest source path must be an absolute path ('$withval')]) ;;
               esac
              ],
              [GTEST_SOURCE="/usr/src/gtest"])

  AC_ARG_WITH([gtest-include-path],
              [AS_HELP_STRING([--with-gtest-include-path],
                              [location of the Google test headers])],
              [GTEST_CPPFLAGS="-I$withval";
               case "$withval" in
                  /*) ;;
                  *) AC_MSG_ERROR([gtest-include-path must be an absolute path ('$withval')]) ;;
               esac
               ])

  GTEST_CPPFLAGS="$GTEST_CPPFLAGS -I$GTEST_SOURCE"

  AC_LANG_PUSH([C++])

  tmp_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$CPPFLAGS $GTEST_CPPFLAGS"

  AC_CHECK_HEADER([gtest/gtest.h])

  CPPFLAGS="$tmp_CPPFLAGS"

  AC_LANG_POP

  AC_CHECK_FILES([$GTEST_SOURCE/src/gtest-all.cc]
                 [$GTEST_SOURCE/src/gtest_main.cc],
                 [have_gtest_source=yes],
                 [have_gtest_source=no])

  AS_IF([test "x$ac_cv_header_gtest_gtest_h" = xyes -a \
              "x$have_gtest_source" = xyes],
        [have_gtest=yes]
        [AC_SUBST(GTEST_CPPFLAGS)]
        [AC_SUBST(GTEST_SOURCE)],
        [have_gtest=no])
]) # CHECK_GTEST
