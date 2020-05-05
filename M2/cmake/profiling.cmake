# WIP

add_custom_target(profile
  COMMAND cat ${CMAKE_BINARY_DIR}/profile.raw | sh ${CMAKE_SOURCE_DIR}/cmake/stackcollapse-m2.sh
    --title "A Profile of \`ninja install-Macaulay2Doc\` (debug build)" > flamegraph.svg
  VERBATIM
  )
