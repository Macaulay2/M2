###############################################################################
# rationality targets and tests

set(M2_ARGS -q --stop)

set(RATIONALITY_TESTS plausibility1.m2 plausibility2.m2 plausibility3.m2 plausibility4.m2 proof-fixed.m2)

set(M2_PATH "join({\"${CMAKE_CURRENT_SOURCE_DIR}/\"},path)")

add_custom_target(M2-tests-rationality)

foreach(_testfile IN LISTS RATIONALITY_TESTS)
  add_custom_target(M2-tests-rationality-${_testfile}
    COMMAND ${M2} ${M2_ARGS} < ${_testfile}
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

  add_dependencies(M2-tests-rationality M2-tests-rationality-${_testfile})
endforeach()

add_test(NAME rationality/plausibility.m2
  COMMAND sh -c "cat plausibility1.m2 plausibility2.m2 plausibility3.m2 plausibility4.m2 | diff -u plausibility.m2 -"
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
add_test(NAME rationality/proof.m2
  COMMAND diff -u proof.m2 proof-fixed.m2
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )
