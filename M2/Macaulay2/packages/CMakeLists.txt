# TODO: add all packages; these are the bare minimum

add_custom_target(packages ALL
  COMMAND
    ln -fs ${CMAKE_CURRENT_SOURCE_DIR}/{Elimination,Text,SimpleDoc,SimpleDoc,LLLBases,IntegralClosure,PrimaryDecomposition,ReesAlgebra,Classic,Parsing,TangentCone,ConwayPolynomials,InverseSystems,Truncations,Polyhedra,FourTiTwo,FourierMotzkin,Normaliz,Topcom}.m2 ${CMAKE_BINARY_DIR}/Macaulay2/share/Macaulay2
  COMMAND
    ln -fs ${CMAKE_CURRENT_SOURCE_DIR}/{Style,SimpleDoc,PrimaryDecomposition,Polyhedra} ${CMAKE_BINARY_DIR}/Macaulay2/share/Macaulay2
  DEPENDS m2
  )

# TODO
# cd ${PACKAGE}-temporary && M2-binary  -q --stop --silent -e errorDepth=3 -e debugLevel=0  -e "installPackage(\"${PACKAGE}\", RemakeAllDocumentation => false, IgnoreExampleErrors => false, RerunExamples => false, CheckDocumentation => true, UserMode => false, Verbose => false, InstallPrefix => \"${CMAKE_BINARY_DIR}/Macaulay2/share/\", SeparateExec => true, DebuggingMode => true); exit 0"

# TODO: unmark-packages?
