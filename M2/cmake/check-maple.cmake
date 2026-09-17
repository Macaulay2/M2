# Maple is optional even when requested: ON means use it if available.
option(WITH_MAPLE "Enable optional Maple interface support when available" ON)
if(WITH_MAPLE)
  find_package(Maple OPTIONAL_COMPONENTS Convex)
  if(Maple_FOUND)
    message(STATUS "Maple interface: enabled; convex: ${Maple_Convex_FOUND}")
  else()
    message(STATUS "Maple interface: unavailable; convex: FALSE")
  endif()
else()
  # Reset results when reconfiguring a previously enabled build. Retain the
  # executable cache entry so ON can reuse the user's explicit path later.
  set(Maple_FOUND FALSE)
  set(MAPLE_FOUND FALSE)
  set(MAPLE_WORKS FALSE)
  set(Maple_Convex_FOUND FALSE)
  message(STATUS "Maple interface: disabled (WITH_MAPLE=OFF); convex: FALSE")
endif()
