###############################################################################

find_package(LATEX COMPONENTS BIBTEX PDFLATEX)
set(BOOKNAME Macaulay2-${PACKAGE_VERSION}-documentation)

set(M2 ${M2_DIST_PREFIX}/${M2_INSTALL_BINDIR}/M2)
set(M2_ARGS --script)

## for M2book.pdf put these lines in texmf.cnf
# main_memory = 1263000
# hash_extra = 15000
# pool_size = 300000
# max_strings = 30000

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.tex
  COMMENT   "Generating ${BOOKNAME}.tex"
  COMMAND   ${M2} ${M2_ARGS} ${CMAKE_CURRENT_SOURCE_DIR}/book.m2
  COMMAND   mv M2book.tex ${BOOKNAME}.tex
  DEPENDS   book.m2 booktex.m2 M2book.tex.in
  USES_TERMINAL
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.aux
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.tex
  COMMAND   ${LATEX_COMPILER} -interaction=batchmode ${BOOKNAME}
  COMMENT   "Running LaTeX ..."
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.bbl
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.aux
            ${CMAKE_CURRENT_SOURCE_DIR}/papers.bib
  COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/papers.bib papers.bib
  COMMAND   ${BIBTEX_COMPILER} -terse ${BOOKNAME}
  COMMENT   "Running BibTeX ..."
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.dvi
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.bbl
  COMMAND   ${LATEX_COMPILER} -interaction=batchmode ${BOOKNAME}
  COMMENT   "Rerunning LaTeX ..."
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.pdf
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.bbl
            ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.dvi
  COMMAND   ${PDFLATEX_COMPILER} -interaction=batchmode ${BOOKNAME}
  COMMENT   "Running pdfTeX ..."
  )

# Eventually trigger the whole process
add_custom_target(M2-book DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.pdf)
