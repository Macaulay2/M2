#set(CMAKE_VERBOSE_MAKEFILE on)

# TODO: Is this folder necessary? Was it an attempt to automate writing d/*? 

add_custom_target(e-includes DEPENDS geores.hpp geogbvec.hpp statuscodes.h)

add_custom_command(OUTPUT geores.hpp geogbvec.hpp statuscodes.h
  COMMAND
    sed -e "s/geobucket/gbvectorHeap/g" -e "s/FREEMODULETYPE/GBRing/g" -e "s/VECTYPE/gbvector */g" ${CMAKE_CURRENT_SOURCE_DIR}/geoT.hpp > geogbvec.hpp
  COMMAND
    sed -e "s/geobucket/respolyHeap/g" -e "s/FREEMODULETYPE/const res2_poly/g" -e "s/VECTYPE/res2term */g" ${CMAKE_CURRENT_SOURCE_DIR}/geoT.hpp > geores.hpp
  COMMAND
    sed -e "s/\\(.*\\):\\(.*\\):\\(.*\\)/  \\2 = \\1,/" ${CMAKE_CURRENT_SOURCE_DIR}/statuscodes > statuscodes.h
  COMMAND
    echo "  DUMMY_STATUS_CODE" >> statuscodes.h
  VERBATIM
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
  )
