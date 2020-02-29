set(CMAKE_VERBOSE_MAKEFILE OFF)

## Issue can be set via command line:
# $ cmake -DISSUE=Catch-22 .
## Unset cached issue with cmake -UISSUE .

# TODO: look into this:
# https://github.com/GPUOpen-LibrariesAndSDKs/RadeonProRenderUSD/blob/master/cmake/macros/PlatformIntrospection.cmake

# TODO: which of these are still relevant? Can CMake replace any?
if("${ISSUE}" STREQUAL "")
  find_program(SW_VERS      sw_vers)
  find_program(LSB_RELEASE  lsb_release)
  find_file(OS_RELEASE      os-release      HINTS /etc /usr/lib)
  find_file(FREEBSD_VERSION freebsd-version HINTS /etc)
  find_file(SYSTEM_RELEASE  system-release  HINTS /etc)
  find_file(ISSUE_FILE      issue           HINTS /etc)

  # TODO: check each one of these
  if(EXISTS ${SW_VERS})
    execute_process(COMMAND ${SW_VERS} -productName
      OUTPUT_VARIABLE ISSUE_FLAVOR
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND ${SW_VERS} -productVersion
      OUTPUT_VARIABLE ISSUE_RELEASE
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  elseif(EXISTS ${LSB_RELEASE})
    execute_process(COMMAND ${LSB_RELEASE} -s --id
      OUTPUT_VARIABLE ISSUE_FLAVOR
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND ${LSB_RELEASE} -s --release
      OUTPUT_VARIABLE ISSUE_RELEASE
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  elseif(EXISTS ${OS_RELEASE_FOUND})
    execute_process(COMMAND . ${OS_RELEASE} COMMAND echo $ID
      OUTPUT_VARIABLE ISSUE_FLAVOR
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
    execute_process(COMMAND . ${OS_RELEASE} COMMAND echo $VERSION_ID
      OUTPUT_VARIABLE ISSUE_RELEASE
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  elseif(EXISTS ${FREEBSD_VERSION_FOUND})
    set(ISSUE_FLAVOR FreeBSD)
    execute_process(COMMAND ${FREEBSD_VERSION}
      OUTPUT_VARIABLE ISSUE_RELEASE
      ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  elseif(EXISTS ${SYSTEM_RELEASE_FOUND})
    # TODO
    #system-release
    #     then ISSUE_FLAVOR=[`</etc/system-release  head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
    #          ISSUE_RELEASE=[`</etc/system-release head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
  elseif(EXISTS ${ISSUE_FILE_FOUND})
    # TODO
    file(READ ${ISSUE_FILE} ISSUE_FILE_CONTENT)
    #string(REGEX REPLACE ISSUE_FLAVOR=[`</etc/issue           head -1 | sed 's/^\([A-Za-z ]*\).*/\1/' | sed 's/ //g' `]
    #string(REGEX REPLACE ISSUE_RELEASE=[`</etc/issue          head -1 | sed 's/[^0-9]*\([0-9.]*\).*/\1/'`]
  else()
    message("## cound not determine issue")
    # TODO: what does this resolve to in macOS?
    set(ISSUE         ${CMAKE_SYSTEM})
    set(ISSUE_FLAVOR  ${CMAKE_SYSTEM_NAME})
    set(ISSUE_RELEASE ${CMAKE_SYSTEM_VERSION})
  endif()

  if(${ISSUE_FLAVOR} MATCHES "Mac OS X")
    set(ISSUE_FLAVOR MacOS)
  elseif(${ISSUE_FLAVOR} MATCHES "(debian|Debian).*")
    set(ISSUE_FLAVOR Debian)
  elseif(${ISSUE_FLAVOR} MATCHES "(ubuntu|Ubuntu)")
    set(ISSUE_FLAVOR Ubuntu)
  elseif(${ISSUE_FLAVOR} MATCHES "FedoraCore.*")
    set(ISSUE_FLAVOR FedoraCore)
  elseif(${ISSUE_FLAVOR} MATCHES "Fedora.*")
    set(ISSUE_FLAVOR Fedora)
  elseif(${ISSUE_FLAVOR} MATCHES "RedHatEnterprise.*")
    set(ISSUE_FLAVOR RedHatEnterprise)
  elseif(${ISSUE_FLAVOR} MATCHES "RedHat.*")
    set(ISSUE_FLAVOR RedHat)
  elseif(${ISSUE_FLAVOR} MATCHES "Scientific.*")
    set(ISSUE_FLAVOR "ScientificLinux")
  elseif(${ISSUE_FLAVOR} MATCHES "Raspbian.*")
    set(ISSUE_FLAVOR Raspbian)
  elseif(${ISSUE_FLAVOR} MATCHES ".*openSUSE")
    set(ISSUE_FLAVOR openSUSE)
  elseif(${ISSUE_FLAVOR} MATCHES "SUSE LINUX")
    set(ISSUE_FLAVOR SuseLinux)
  elseif(${ISSUE_FLAVOR} MATCHES "arch")
    set(ISSUE         ArchLinux)
    set(ISSUE_FLAVOR  ArchLinux)
    set(ISSUE_RELEASE none)
  else()
    message("## could not recognize issue flavor: ${ISSUE_FLAVOR}")
  endif()

  string(REPLACE " " "-" ISSUE_FLAVOR "${ISSUE_FLAVOR}")

  if(NOT "${ISSUE_RELEASE}" MATCHES "(none|unknown)")
    set(ISSUE "${ISSUE_FLAVOR}-${ISSUE_RELEASE}")
  endif()
endif()
