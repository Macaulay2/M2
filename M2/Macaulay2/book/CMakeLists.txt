###############################################################################
# TODO: Currently failing due to many errors in booktex.m2
# also roman.m2 doesn't exit. Remove?

find_package(LATEX)
set(BOOKNAME Macaulay2-${PACKAGE_VERSION}-book)

## for M2book.pdf put these lines in texmf.cnf
# main_memory = 1263000
# hash_extra = 15000
# pool_size = 300000
# max_strings = 30000

configure_file(book.m2 book.m2 COPYONLY)
configure_file(booktex.m2 booktex.m2 COPYONLY)
configure_file(papers.bib papers.bib COPYONLY)

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.tex
  COMMAND   M2-binary -q book.m2 --silent --stop -e 'exit 0' # ${M2_ARGS}
  COMMAND   mv M2book.tmp ${BOOKNAME}.tex
  COMMENT   "Generating ${BOOKNAME}.tex"
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.aux
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.tex
  COMMAND   ${LATEX_COMPILER} -interaction=batchmode ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}
  COMMENT   "Latex (first pass)"
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.bbl
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.aux
  COMMAND   ${BIBTEX_COMPILER} -terse ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}
  COMMENT   "Bibtex"
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.dvi
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.bbl
  COMMAND   ${LATEX_COMPILER} -interaction=batchmode ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}
  COMMENT   "Latex (second pass)"
  )

add_custom_command(
  OUTPUT    ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.log
  DEPENDS   ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.bbl
            ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.dvi
  COMMAND   ${LATEX_COMPILER} -interaction=batchmode ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}
  COMMENT   "Latex (third pass)"
  )

# Eventually trigger the whole process
add_custom_target(M2-book DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${BOOKNAME}.log)
