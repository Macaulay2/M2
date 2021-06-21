###############################################################################
## This directory contains the rules for generating grammar files for editors
## and syntax highlighting engines as well as the M2-mode package for Emacs.
## - M2-editors generates grammar files
## - M2-emacs   generates the M2-mode package for Emacs

# TODO: generate vim grammar
# TODO: generate textmate grammar

set(M2 ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
set(M2_ARGS --script)

###############################################################################
## Generating the grammar files is mainly done in make-M2-symbols.m2.
## To add to the M2-editors target, add grammar filenames here and in .gitignore.
set(GRAMMAR_FILES
  atom/macaulay2.cson
  prism/macaulay2.js
  pygments/macaulay2.py
#  rouge/macaulay2.rb
  vim/m2.vim.syntax
  vim/m2.vim.dict
  emacs/M2-symbols.el)

set(GRAMMAR_TEMPLATES ${GRAMMAR_FILES})
list(TRANSFORM GRAMMAR_TEMPLATES APPEND ".in")
add_custom_target(M2-editors DEPENDS ${GRAMMAR_FILES})
add_custom_command(OUTPUT ${GRAMMAR_FILES}
  COMMENT "Generating M2 grammar files for editors"
  COMMAND ${M2} ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/make-M2-symbols.m2
  MAIN_DEPENDENCY make-M2-symbols.m2
  DEPENDS M2-core ${GRAMMAR_TEMPLATES})

###############################################################################
## Generating the M2-mode package for Emacs

if(NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/emacs/make-M2-emacs-help.m2)
  message(WARNING "Macaulay2/editors/emacs submodule is missing; skipping M2-emacs target")
  return()
endif()

# TODO: only the emacs target is installed in usr-dist by default
set(M2_INSTALL_EMACSDIR
  ${M2_DIST_PREFIX}/${M2_INSTALL_DATAROOTDIR}/emacs/site-lisp/macaulay2)
file(COPY emacs/ DESTINATION ${M2_INSTALL_EMACSDIR}
  FILES_MATCHING PATTERN "M2*" PATTERN "*.in" EXCLUDE)

set(EMACS_FILES M2-emacs-help.txt M2-emacs.m2 M2-symbols.el.gz)
list(TRANSFORM EMACS_FILES PREPEND ${M2_INSTALL_EMACSDIR}/)
add_custom_target(M2-emacs ALL DEPENDS ${EMACS_FILES})
add_custom_command(OUTPUT ${EMACS_FILES}
  COMMENT "Generating Emacs package"
  COMMAND ${M2} ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/emacs/make-M2-emacs-help.m2
  COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/emacs/M2-symbols.el ${M2_INSTALL_EMACSDIR}
  COMMAND gzip -nf9 M2-symbols.el
  # This command updates the generated content in the M2-emacs submodule
  COMMAND ${CMAKE_COMMAND} -E copy M2-emacs-help.txt M2-emacs.m2 M2-symbols.el.gz ${CMAKE_CURRENT_SOURCE_DIR}/emacs
  MAIN_DEPENDENCY emacs/make-M2-emacs-help.m2
  WORKING_DIRECTORY ${M2_INSTALL_EMACSDIR}
  DEPENDS emacs/M2-symbols.el make-M2-symbols.m2)
