## This file's single purpose is to set the ISSUE parameter
# with format [FLAVOR]-[RELEASE] which can be overriden
# with -DISSUE=Catch-22 and unset with -UISSUE

if(NOT DEFINED ISSUE)
  find_program(LSB_RELEASE	lsb_release)
  find_program(SW_VERS		sw_vers)
  find_file(OS_RELEASE		os-release		HINTS /etc /usr/lib)
  find_file(ISSUE_FILE		issue			HINTS /etc)
  find_file(FREEBSD_VERSION	freebsd-version		HINTS /etc)
  find_file(SYSTEM_RELEASE	system-release		HINTS /etc)
  find_file(SYSTEM_RELEASE_CPE	system-release-cpe	HINTS /etc)

  # TODO: check each one of these
  if(SW_VERS)
    execute_process(COMMAND ${SW_VERS} -productName
      OUTPUT_VARIABLE ISSUE_FLAVOR
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND ${SW_VERS} -productVersion
      OUTPUT_VARIABLE ISSUE_RELEASE
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  elseif(LSB_RELEASE)
    execute_process(COMMAND ${LSB_RELEASE} -s --id
      OUTPUT_VARIABLE ISSUE_FLAVOR
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND ${LSB_RELEASE} -s --release
      OUTPUT_VARIABLE ISSUE_RELEASE
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  elseif(OS_RELEASE)
    file(READ ${OS_RELEASE} _os_release)
    string(REGEX MATCH "ID=([A-Za-z ]*)\n"      _ "${_os_release}")
    set(ISSUE_FLAVOR  "${CMAKE_MATCH_1}")
    string(REGEX MATCH "VERSION_ID=([0-9.]*)\n" _ "${_os_release}")
    set(ISSUE_RELEASE "${CMAKE_MATCH_1}")
  elseif(FREEBSD_VERSION)
    set(ISSUE_FLAVOR FreeBSD)
    execute_process(COMMAND ${FREEBSD_VERSION}
      OUTPUT_VARIABLE ISSUE_RELEASE
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  elseif(SYSTEM_RELEASE)
    file(READ ${SYSTEM_RELEASE} _system_release)
    string(REGEX MATCH "([A-Za-z ]*)[ \t]*([0-9.]*).*" _system_release "${_system_release}")
    set(ISSUE_FLAVOR  "${CMAKE_MATCH_1}")
    set(ISSUE_RELEASE "${CMAKE_MATCH_2}")
  elseif(SYSTEM_RELEASE_CPE)
    file(READ ${SYSTEM_RELEASE_CPE} _system_release_cpe)
    string(REGEX MATCH "cpe:[^:]*:[^:]*:([A-Za-z ]*):([0-9.]*)" _system_release_cpe "${_system_release_cpe}")
    set(ISSUE_FLAVOR  "${CMAKE_MATCH_1}")
    set(ISSUE_RELEASE "${CMAKE_MATCH_2}")
  elseif(ISSUE_FILE)
    file(READ ${ISSUE_FILE} _issue)
    string(REGEX MATCH "([A-Za-z ]*)[ \t]*([0-9.]*).*" _issue "${_issue}")
    set(ISSUE_FLAVOR  "${CMAKE_MATCH_1}")
    set(ISSUE_RELEASE "${CMAKE_MATCH_2}")
  else()
    message("## cound not determine issue")
    # TODO: what does this resolve to in macOS?
    set(ISSUE         ${CMAKE_SYSTEM})
    set(ISSUE_FLAVOR  ${CMAKE_SYSTEM_NAME})
    set(ISSUE_RELEASE ${CMAKE_SYSTEM_VERSION})
  endif()

  if(ISSUE_FLAVOR MATCHES "Mac OS X")
    set(ISSUE_FLAVOR MacOS)
  elseif(ISSUE_FLAVOR MATCHES "(debian|Debian).*")
    set(ISSUE_FLAVOR Debian)
  elseif(ISSUE_FLAVOR MATCHES "(ubuntu|Ubuntu)")
    set(ISSUE_FLAVOR Ubuntu)
  elseif(ISSUE_FLAVOR MATCHES "FedoraCore.*")
    set(ISSUE_FLAVOR FedoraCore)
  elseif(ISSUE_FLAVOR MATCHES "(fedora|Fedora).*")
    set(ISSUE_FLAVOR Fedora)
  elseif(ISSUE_FLAVOR MATCHES "RedHatEnterprise.*")
    set(ISSUE_FLAVOR RedHatEnterprise)
  elseif(ISSUE_FLAVOR MATCHES "RedHat.*")
    set(ISSUE_FLAVOR RedHat)
  elseif(ISSUE_FLAVOR MATCHES "Scientific.*")
    set(ISSUE_FLAVOR ScientificLinux)
  elseif(ISSUE_FLAVOR MATCHES "Raspbian.*")
    set(ISSUE_FLAVOR Raspbian)
  elseif(ISSUE_FLAVOR MATCHES ".*openSUSE")
    set(ISSUE_FLAVOR openSUSE)
  elseif(ISSUE_FLAVOR MATCHES "SUSE LINUX")
    set(ISSUE_FLAVOR SuseLinux)
  elseif(ISSUE_FLAVOR MATCHES "arch")
    set(ISSUE         ArchLinux)
    set(ISSUE_FLAVOR  ArchLinux)
    set(ISSUE_RELEASE none)
  else()
    message("## could not recognize issue flavor: ${ISSUE_FLAVOR}")
  endif()

  string(REPLACE " " "-" ISSUE_FLAVOR "${ISSUE_FLAVOR}")

  if(NOT ISSUE_RELEASE MATCHES "(none|unknown)")
    set(ISSUE "${ISSUE_FLAVOR}-${ISSUE_RELEASE}")
  endif()
endif()
