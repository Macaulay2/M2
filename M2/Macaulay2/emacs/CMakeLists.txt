###############################################################################
## This directory contains the rules for generating the M2-mode Emacs package.
## The target is M2-emacs

# TODO: generate textmate grammar

set(M2_ARGS -q --stop --silent)
set(EMACS_DIR ${M2_DIST_PREFIX}/${M2_INSTALL_DATAROOTDIR}/emacs/site-lisp)
set(EMACS_FILES M2-README M2-emacs-hlp.txt M2-emacs.m2 M2-init.el M2-mode.el M2-symbols.el M2.el)

# TODO: move this from ALL to install
add_custom_target(M2-emacs DEPENDS M2-symbols.el M2-emacs-hlp.txt M2-emacs.m2)

# TODO: refactor the three scripts
add_custom_command(OUTPUT M2-symbols.el M2-emacs-hlp.txt M2-emacs.m2
  COMMENT
    "Generating Emacs package"
  COMMAND
    M2-binary ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/makesyms.m2 -e 'exit 0'
  COMMAND
    M2-binary ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/makehlp.m2 -e 'exit 0'
  COMMAND
    M2-binary ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/makem2.m2 -e 'exit 0'
  WORKING_DIRECTORY ${EMACS_DIR}
  DEPENDS M2-core
  )

configure_file(M2-init.el.in ${EMACS_DIR}/M2-init.el @ONLY)
file(COPY M2-mode.el M2.el M2-README DESTINATION ${EMACS_DIR})
