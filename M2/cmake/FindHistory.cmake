# Locale GNU history library

find_path(HISTORY_INCLUDE_DIR
  NAMES readline/history.h
  PATHS ${READLINE_ROOT_DIR}/include ${READLINE_INCLUDE_DIR}
  )

find_library(HISTORY_LIBRARY
  NAMES history
  PATHS ${READLINE_ROOT_DIR}/lib
  )

# handle the QUIETLY and REQUIRED arguments and set XXX_FOUND to TRUE if all listed variables are TRUE
INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(History DEFAULT_MSG HISTORY_LIBRARY HISTORY_INCLUDE_DIR)

mark_as_advanced(HISTORY_INCLUDE_DIR HISTORY_LIBRARY)
