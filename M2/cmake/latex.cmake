find_package(LATEX COMPONENTS BIBTEX PDFLATEX)

MACRO (_ADD_LATEX_TARGET _package _bookdir _bookname)
  add_custom_command(
    COMMENT   "Generating ${_bookname}.tex and running LaTeX ..."
    OUTPUT    ${_bookdir}/${_bookname}.tex
              ${_bookdir}/${_bookname}.aux
    COMMAND   ${M2} ${M2_ARGS} -e ${M2_DOC_STRING} -e "exit 0"
    COMMAND   ${LATEX_COMPILER} -interaction=batchmode ${_bookname}
    VERBATIM
    DEPENDS   ${CMAKE_CURRENT_SOURCE_DIR}/../m2/book.m2
    # TODO: allow per package bibliography file, as indicated in per package templates
              ${CMAKE_CURRENT_SOURCE_DIR}/Style/papers.bib
              ${CMAKE_CURRENT_SOURCE_DIR}/Style/M2book.tex.in
    WORKING_DIRECTORY ${_bookdir}
    ${USES_TERMINAL})

  add_custom_command(
    COMMENT   "Running BibTeX ..."
    OUTPUT    ${_bookdir}/${_bookname}.bbl
    DEPENDS   ${_bookdir}/${_bookname}.aux
              ${CMAKE_CURRENT_SOURCE_DIR}/Style/papers.bib
    COMMAND   ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/Style/papers.bib papers.bib
    COMMAND   ${BIBTEX_COMPILER} -terse ${_bookname}
    WORKING_DIRECTORY ${_bookdir})

  add_custom_command(
    COMMENT   "Rerunning LaTeX ..."
    OUTPUT    ${_bookdir}/${_bookname}.dvi
    DEPENDS   ${_bookdir}/${_bookname}.bbl
    COMMAND   ${LATEX_COMPILER} -interaction=batchmode ${_bookname}
    WORKING_DIRECTORY ${_bookdir})

  add_custom_command(
    COMMENT   "Running pdfTeX ..."
    OUTPUT    ${_bookdir}/${_bookname}.pdf
    DEPENDS   ${_bookdir}/${_bookname}.bbl
              ${_bookdir}/${_bookname}.dvi
    COMMAND   ${PDFLATEX_COMPILER} -interaction=batchmode ${_bookname}
    WORKING_DIRECTORY ${_bookdir})
ENDMACRO (_ADD_LATEX_TARGET)
