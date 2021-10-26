# WIP

if(PROFILING)
  set(_title  A Profile of Macaulay2 ${CMAKE_BUILD_TYPE} build)

  add_custom_target(profile COMMAND
    export TITLE="${_title}" && read -ep "Profiling data log: " INPUT &&
    cat $$INPUT | sh ${CMAKE_SOURCE_DIR}/cmake/stackcollapse-m2.sh > $$INPUT.html &&
    echo "FlameGraph saved in $$INPUT.html" && xdg-open $$INPUT.html
    USES_TERMINAL)
endif()
