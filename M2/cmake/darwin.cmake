set(CMAKE_SYSTEM_NAME 		Darwin)
set(CMAKE_SYSTEM_PROCESSOR	x86_64)

#set(CMAKE_SYSROOT /)
set(CMAKE_STAGING_PREFIX ${CMAKE_CURRENT_LIST_DIR}/usr-dist)

#set(tools /home/devel/gcc-4.7-linaro-rpi-gnueabihf)
#set(CMAKE_C_COMPILER ${tools}/bin/arm-linux-gnueabihf-gcc)
#set(CMAKE_CXX_COMPILER ${tools}/bin/arm-linux-gnueabihf-g++)
#TODO: use CMAKE_<LANG>_FLAGS_<CONFIG>_INIT for Accelerate framework?

# ONLY: CMAKE_FIND_ROOT_PATH will be searched
# NEVER: only the host system root will be searched
# BOTH: both will be searched
#set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
#set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
#set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
#set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
