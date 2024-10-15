# Here we introduce a macro that is derived from AC_SEARCH_LIBS, but modified
# so we can specify libraries as absolute paths, such as /usr/local/lib/libfoo.a or -lfoo.
# The point of this is so we can link with the static version of the library instead
# of the dynamic version.  We need something like this, because the BSD "ld" command
# provided by Mac OS does not offer a command line option to achieve it.

# =============================================================================

# -- Macro: SEARCH_LIBRARIES (FUNCTION, SEARCH-LIBS, [ACTION-IF-FOUND],
#          [ACTION-IF-NOT-FOUND], [OTHER-LIBRARIES])
#     Search for a library defining FUNCTION if it's not already
#     available.  This equates to calling
#     `AC_LINK_IFELSE([AC_LANG_CALL([], [FUNCTION])])' first with no
#     libraries, then for each library listed in SEARCH-LIBS.

#     Prepend `LIBRARY' to `LIBS' for the first library found to
#     contain FUNCTION, and run ACTION-IF-FOUND.  If the function is not
#     found, run ACTION-IF-NOT-FOUND.

#     If linking with LIBRARY results in unresolved symbols that would
#     be resolved by linking with additional libraries, give those
#     libraries as the OTHER-LIBRARIES argument, separated by spaces:
#     e.g., `-lXt -lX11'.  Otherwise, this macro fails to detect that
#     FUNCTION is present, because linking the test program always fails
#     with unresolved symbols.

#     The result of this test is cached in the `ac_cv_search_FUNCTION'
#     variable as `none required' if FUNCTION is already available, as
#     `no' if no library containing FUNCTION was found, otherwise as the
#     `LIBRARY' option that needs to be prepended to `LIBS'.

# =============================================================================

# SEARCH_LIBRARIES(FUNCTION, SEARCH-LIBS,
#                [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND],
#                [OTHER-LIBRARIES])
# --------------------------------------------------------
# Search for a library defining FUNC, if it's not already available.
AC_DEFUN([SEARCH_LIBRARIES],
[AS_VAR_PUSHDEF([ac_Search], [ac_cv_search_$1])dnl
AC_CACHE_CHECK([for library containing $1], [ac_Search],
[ac_func_search_save_LIBS=$LIBS
AC_LANG_CONFTEST([AC_LANG_CALL([], [$1])])
for ac_lib in '' $2; do
  if test -z "$ac_lib"; then
    ac_res="none required"
  else
    ac_res=$ac_lib
    LIBS="$ac_lib $5 $ac_func_search_save_LIBS"
  fi
  AC_LINK_IFELSE([], [AS_VAR_SET([ac_Search], [$ac_res])])
  AS_VAR_SET_IF([ac_Search], [break])
done
AS_VAR_SET_IF([ac_Search], , [AS_VAR_SET([ac_Search], [no])])
rm conftest.$ac_ext
LIBS=$ac_func_search_save_LIBS])
AS_VAR_COPY([ac_res], [ac_Search])
AS_IF([test "$ac_res" != no],
  [test "$ac_res" = "none required" || LIBS="$ac_res $LIBS"
  $3],
      [$4])
AS_VAR_POPDEF([ac_Search])dnl
])
