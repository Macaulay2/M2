#set(CMAKE_VERBOSE_MAKEFILE on)

set(M2LIST autoload.m2 chaincomplexes.m2 classes.m2 code.m2 colon.m2 combinatorics.m2 command.m2 content.m2 debugging.m2 docsearch.m2 document.m2 dotdot.m2 engine.m2 enginering.m2 exports.m2 expressions.m2 ext.m2 factor.m2 fano.m2 files.m2 flint.m2 fold.m2 galois.m2 gateway.m2 gb.m2 genmat.m2 gradedmodules.m2 html.m2 html0.m2 http.m2 hypertext.m2 indeterminates.m2 integers.m2 integrate.m2 jupyter.m2 last.m2 lists.m2 local.m2 localring.m2 mathml.m2 matrix.m2 matrix1.m2 matrix2.m2 matrix3.m2 max.m2 methods.m2 minPres.m2 modules.m2 modules2.m2 monideal.m2 monomcurve.m2 multilin.m2 mutablemat.m2 nets.m2 newring.m2 obsolete.m2 ofcm.m2 option.m2 orderedmonoidrings.m2 packages.m2 peek.m2 powers.m2 pretty.m2 printing.m2 profile.m2 quotient.m2 quotring.m2 radical.m2 rationals.m2 reals.m2 remember.m2 res.m2 ringmap.m2 rings.m2 robust.m2 schubert.m2 set.m2 setup.m2 structure.m2 system.m2 tables.m2 texhtml.m2 texmacs.m2 tor.m2 typicalvalues.m2 undoc.m2 validate.m2 variables.m2 varieties.m2 webapp.m2)

list(TRANSFORM M2LIST PREPEND ${CORE_DIR}/ OUTPUT_VARIABLE CORELIST)

add_custom_target(m2 DEPENDS ${CORELIST} ${CORE_DIR}/tvalues.m2 ${CORE_DIR}/loadsequence ${CORE_DIR}/installedpackages)

# TODO: version.m2?

add_custom_command(OUTPUT ${CORE_DIR}/statuscodes
  COMMENT
    "Generating ../Share/Macaulay2/Core/statuscodes"
  COMMAND
    ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}
  COMMAND
    ${CMAKE_COMMAND} -E copy_if_different statuscodes ${CORE_DIR}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/Macaulay2/e-includes
  )

add_custom_command(OUTPUT ${CORELIST} ${CORE_DIR}/loadsequence ${CORE_DIR}/installedpackages
  COMMENT
    "Generating ../Share/Macaulay2/Core/*.m2"
  COMMAND
    ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}
  COMMAND
    ${CMAKE_COMMAND} -E copy_if_different ${M2LIST} loadsequence installedpackages ${CORE_DIR}
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
  )

add_custom_command(OUTPUT ${CORE_DIR}/tvalues.m2
  COMMAND
    M2-binary -q --silent --stop -e errorDepth=0 --no-preload --no-tvalues ${CMAKE_CURRENT_SOURCE_DIR}/tvalues-make.m2 -e "make \"${CMAKE_SOURCE_DIR}/Macaulay2/d/\"; exit 0"
  COMMAND
    ${CMAKE_COMMAND} -E make_directory ${CORE_DIR}
  COMMAND
    ${CMAKE_COMMAND} -E copy_if_different tvalues.m2 ${CORE_DIR}
  VERBATIM
  DEPENDS ${CORELIST} ${CORE_DIR}/statuscodes
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )

#install(FILES ${M2LIST} loadsequence installedpackages
#  DESTINATION ${CMAKE_BINARY_DIR}/usr-dist/common/share/Macaulay2/Core)
