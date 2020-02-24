set(CMAKE_VERBOSE_MAKEFILE OFF)

set(M2_ARGS -q --stop --silent)
set(EMACS_FILES M2-README M2-emacs-hlp.txt M2-emacs.m2 M2-init.el M2-mode.el M2-symbols.el M2.el)

configure_file (
  ${CMAKE_CURRENT_SOURCE_DIR}/M2-init.el.in
  ${CMAKE_CURRENT_BINARY_DIR}/M2-init.el
  @ONLY
  )

add_custom_target(emacs ALL DEPENDS M2-symbols.el M2-emacs-hlp.txt M2-emacs.m2)

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
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS m2
  )

# TODO
#install -c -d "${CMAKE_BUILD_DIR}/usr-dist/common/share/emacs/site-lisp"
#install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/M2-mode.el ${CMAKE_BUILD_DIR}/usr-dist/common/share/emacs/site-lisp)
#install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/M2.el
#install(FILES ${CMAKE_CURRENT_SOURCE_DIR}/M2-README
#install(FILES M2-symbols.el M2-emacs-hlp.txt M2-emacs.m2 M2-init.el)
