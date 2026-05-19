# This file will be configured to contain variables for CPack. These variables
# should be set in the CMake list file of the project before CPack module is
# included. The list of available CPACK_xxx variables and their associated
# documentation may be obtained using
#  cpack --help-variable-list
#
# Some variables are common to all generators (e.g. CPACK_PACKAGE_NAME)
# and some are specific to a generator
# (e.g. CPACK_NSIS_EXTRA_INSTALL_COMMANDS). The generator specific variables
# usually begin with CPACK_<GENNAME>_xxxx.


set(CPACK_BUILD_SOURCE_DIRS "/home/ubuntu/M2/M2;/home/ubuntu/M2/M2/BUILD/build")
set(CPACK_CMAKE_GENERATOR "Ninja")
set(CPACK_COMPONENTS_ALL "ALL;common;devel")
set(CPACK_COMPONENT_UNSPECIFIED_HIDDEN "TRUE")
set(CPACK_COMPONENT_UNSPECIFIED_REQUIRED "TRUE")
set(CPACK_DEBIAN_PACKAGE_DESCRIPTION "Macaulay2 is a software system for algebraic geometry research, written by
Daniel R. Grayson and Michael E. Stillman. Based on Groebner bases, it
provides algorithms for computing homological invariants of rings and
modules.")
set(CPACK_DEBIAN_PACKAGE_NAME "Macaulay2")
set(CPACK_DEBIAN_PACKAGE_PRIORITY "optional")
set(CPACK_DEBIAN_PACKAGE_RELEASE "1")
set(CPACK_DEBIAN_PACKAGE_SECTION "math")
set(CPACK_DEBIAN_PACKAGE_SHLIBDEPS "ON")
set(CPACK_DEBIAN_PACKAGE_URL "https://macaulay2.com")
set(CPACK_DEBIAN_PACKAGE_VERSION "1.26.05--")
set(CPACK_DEFAULT_PACKAGE_DESCRIPTION_FILE "/usr/share/cmake-4.2/Templates/CPack.GenericDescription.txt")
set(CPACK_DEFAULT_PACKAGE_DESCRIPTION_SUMMARY "Macaulay2 built using CMake")
set(CPACK_GENERATOR "TGZ")
set(CPACK_GENERATOR "TGZ")
set(CPACK_IGNORE_FILES "BUILD")
set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY "ON")
set(CPACK_INNOSETUP_ARCHITECTURE "x64")
set(CPACK_INSTALLED_DIRECTORIES "/home/ubuntu/M2/M2;/")
set(CPACK_INSTALL_CMAKE_PROJECTS "")
set(CPACK_INSTALL_PREFIX "/usr/local")
set(CPACK_MODULE_PATH "/home/ubuntu/M2/M2/cmake")
set(CPACK_NSIS_DISPLAY_NAME "Macaulay2 1.26.05--")
set(CPACK_NSIS_INSTALLER_ICON_CODE "")
set(CPACK_NSIS_INSTALLER_MUI_ICON_CODE "")
set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
set(CPACK_NSIS_PACKAGE_NAME "Macaulay2 1.26.05--")
set(CPACK_NSIS_UNINSTALL_NAME "Uninstall")
set(CPACK_OBJCOPY_EXECUTABLE "/usr/bin/objcopy")
set(CPACK_OBJDUMP_EXECUTABLE "/usr/bin/objdump")
set(CPACK_OUTPUT_CONFIG_FILE "/home/ubuntu/M2/M2/BUILD/build/CPackConfig.cmake")
set(CPACK_PACKAGE_CHECKSUM "SHA256")
set(CPACK_PACKAGE_CONTACT "Mahrud Sayrafi <mahrud@umn.edu>")
set(CPACK_PACKAGE_DEFAULT_LOCATION "/")
set(CPACK_PACKAGE_DESCRIPTION "Macaulay2 is a software system for algebraic geometry research, written by
Daniel R. Grayson and Michael E. Stillman. Based on Groebner bases, it
provides algorithms for computing homological invariants of rings and
modules.")
set(CPACK_PACKAGE_DESCRIPTION_FILE "/usr/share/cmake-4.2/Templates/CPack.GenericDescription.txt")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Software system for algebraic geometry research")
set(CPACK_PACKAGE_FILE_NAME "Macaulay2-1.26.05---1.src")
set(CPACK_PACKAGE_HOMEPAGE_URL "https://macaulay2.com")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "Macaulay2 1.26.05--")
set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "Macaulay2 1.26.05--")
set(CPACK_PACKAGE_NAME "Macaulay2")
set(CPACK_PACKAGE_RELEASE "1")
set(CPACK_PACKAGE_RELOCATABLE "true")
set(CPACK_PACKAGE_SUMMARY "Software system for algebraic geometry research")
set(CPACK_PACKAGE_URL "https://macaulay2.com")
set(CPACK_PACKAGE_VENDOR "Humanity")
set(CPACK_PACKAGE_VERSION "1.26.05--")
set(CPACK_PACKAGE_VERSION_MAJOR "1")
set(CPACK_PACKAGE_VERSION_MINOR "26")
set(CPACK_PACKAGE_VERSION_PATCH "05")
set(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local")
set(CPACK_READELF_EXECUTABLE "/usr/bin/readelf")
set(CPACK_RESOURCE_FILE_LICENSE "/usr/share/cmake-4.2/Templates/CPack.GenericLicense.txt")
set(CPACK_RESOURCE_FILE_README "/usr/share/cmake-4.2/Templates/CPack.GenericDescription.txt")
set(CPACK_RESOURCE_FILE_WELCOME "/usr/share/cmake-4.2/Templates/CPack.GenericWelcome.txt")
set(CPACK_RPM_CHANGELOG_FILE "/home/ubuntu/M2/M2/BUILD/build/CHANGELOG")
set(CPACK_RPM_EXCLUDE_FROM_AUTO_FILELIST_ADDITION "/usr/local;/usr/local/bin;/usr/local/lib;/usr/local/libexec;/usr/local/share;/usr/local/share/man;/usr/local/share/man/man1;/usr/local/share/info")
set(CPACK_RPM_PACKAGE_DESCRIPTION "Macaulay2 is a software system for algebraic geometry research, written by
Daniel R. Grayson and Michael E. Stillman. Based on Groebner bases, it
provides algorithms for computing homological invariants of rings and
modules.")
set(CPACK_RPM_PACKAGE_GROUP "Applications/Engineering")
set(CPACK_RPM_PACKAGE_LICENSE "GPLv2+")
set(CPACK_RPM_PACKAGE_NAME "Macaulay2")
set(CPACK_RPM_PACKAGE_RELEASE "1")
set(CPACK_RPM_PACKAGE_SOURCES "ON")
set(CPACK_RPM_PACKAGE_SUMMARY "Software system for algebraic geometry research")
set(CPACK_RPM_PACKAGE_URL "https://macaulay2.com")
set(CPACK_RPM_PACKAGE_VERSION "1.26.05--")
set(CPACK_RPM_POST_INSTALL_SCRIPT_FILE "/home/ubuntu/M2/M2/BUILD/build//CMakeFiles/rpm-postinstall.sh")
set(CPACK_RPM_POST_UNINSTALL_SCRIPT_FILE "/home/ubuntu/M2/M2/BUILD/build//CMakeFiles/rpm-postuninstall.sh")
set(CPACK_RPM_PRE_INSTALL_SCRIPT_FILE "/home/ubuntu/M2/M2/BUILD/build//CMakeFiles/rpm-preinstall.sh")
set(CPACK_RPM_PRE_UNINSTALL_SCRIPT_FILE "/home/ubuntu/M2/M2/BUILD/build//CMakeFiles/rpm-preuninstall.sh")
set(CPACK_SET_DESTDIR "OFF")
set(CPACK_SOURCE_GENERATOR "TGZ")
set(CPACK_SOURCE_IGNORE_FILES "BUILD")
set(CPACK_SOURCE_INSTALLED_DIRECTORIES "/home/ubuntu/M2/M2;/")
set(CPACK_SOURCE_OUTPUT_CONFIG_FILE "/home/ubuntu/M2/M2/BUILD/build/CPackSourceConfig.cmake")
set(CPACK_SOURCE_PACKAGE_FILE_NAME "Macaulay2-1.26.05---1.src")
set(CPACK_SOURCE_TOPLEVEL_TAG "Linux-Source")
set(CPACK_STRIP_FILES "")
set(CPACK_SYSTEM_NAME "Linux")
set(CPACK_THREADS "1")
set(CPACK_TOPLEVEL_TAG "Linux-Source")
set(CPACK_WIX_SIZEOF_VOID_P "8")

if(NOT CPACK_PROPERTIES_FILE)
  set(CPACK_PROPERTIES_FILE "/home/ubuntu/M2/M2/BUILD/build/CPackProperties.cmake")
endif()

if(EXISTS ${CPACK_PROPERTIES_FILE})
  include(${CPACK_PROPERTIES_FILE})
endif()
