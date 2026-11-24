###############################################################################
## This directory contains the rules for generating grammar files for editors
## and syntax highlighting engines.
## - M2-editors generates grammar files

set(M2 ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
set(M2_ARGS --script)

###############################################################################
## Generating the grammar files is mainly done in make-M2-symbols.m2.
## To add to the M2-editors target, add grammar filenames here and in .gitignore.
set(GRAMMAR_FILES
  prism/macaulay2.js
  pygments/macaulay2.py
#  rouge/macaulay2.rb
  vim/syntax/m2.vim
  vim/dict/m2.vim.dict)

set(GRAMMAR_TEMPLATES ${GRAMMAR_FILES})
list(TRANSFORM GRAMMAR_TEMPLATES APPEND ".in")
add_custom_target(M2-editors DEPENDS ${GRAMMAR_FILES})
add_custom_command(OUTPUT ${GRAMMAR_FILES}
  COMMENT "Generating M2 grammar files for editors"
  COMMAND ${M2} ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/make-M2-symbols.m2
  MAIN_DEPENDENCY make-M2-symbols.m2
  DEPENDS M2-core ${GRAMMAR_TEMPLATES})

################################################################################
## Generating prism.js/highlight.js for syntax highlighting in the html
## documentation

set(PRISM_DIR ${CMAKE_CURRENT_BINARY_DIR}/prism)
file(MAKE_DIRECTORY ${PRISM_DIR})
file(COPY prism/ DESTINATION ${PRISM_DIR}
  FILES_MATCHING PATTERN "*.json" PATTERN "*.js" PATTERN "*.css")

if(NOT NPM)
  return()
endif()

add_custom_target(M2-prism ALL DEPENDS prism/prism.js)
add_custom_command(OUTPUT prism/prism.js
  COMMENT "Generating prism.js syntax file"
  COMMAND ${NPM} install && ${NPM} run build
  WORKING_DIRECTORY ${PRISM_DIR}
  DEPENDS prism/macaulay2.js)
