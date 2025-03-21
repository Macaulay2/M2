document {
     Key => {(symbol ^, ChainComplex, ZZ)},
     Headline => "access member, cohomological degree",
     Usage => "C^n",
     Inputs => {"C", "n"},
     Outputs => {
     	  Module => {"The ", TT "(-n)", "-th component ", TT "C_(-n)", " of ", TT "C"}
	  },
     "Subscripts refer to homological degree, and superscripts refer to cohomological degree.
     It is only a matter of notation: ", TT "C_(-n)", " is always the same as ", TT "C^n", ".",
     EXAMPLE lines ///
     	  R = QQ[x,y,z];
	  C = res coker vars R
	  C = dual C
	  C^2
	  C^2 == C_(-2)
     	  ///,
     SeeAlso => {ChainComplex, (symbol^, ChainComplex, Array)}
     }
document {
     Key => {(symbol ^, ChainComplexMap, ZZ),
	  (symbol ^, GradedModuleMap, ZZ)},
     Headline => "iterated composition",
     Usage => "f^n",
     Inputs => {"f" => {"or a ", ofClass GradedModuleMap}, "n"},
     Outputs => {
     	  ChainComplexMap => {"the composite ", TT "f o f o ... o f", " (", TT "n", " times)"}
	  },
     "If ", TT "f", " is a ", TO GradedModuleMap, ", then so is the result.",
     PARA{},
     "One use of this function is to determine if a chain complex is well-defined.  
     The chain complex will be well-defined if the square of the differential is zero.",
     EXAMPLE lines ///
     	  R = QQ[x,y,z];
	  C = res coker vars R
	  C.dd^2 == 0
     	  ///,
     SeeAlso => {ChainComplex}
     }

document {
     Key => {
       (symbol ^,ChainComplex,Array)},
     Headline => "projection onto summand",
     Usage => "M^[i,j,...,k]",
     Inputs => {"M",
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => {ofClass ChainComplexMap}
	  },
     PARA{},
     "The chain complex ", TT "M", " should be a direct sum, and the result is the map
     obtained by projection onto the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.",
     PARA{},
     EXAMPLE lines ///
	  M = ZZ^2 ++ ZZ^3
      	  M^[0]
      	  M^[1]
      	  M^[1,0]
	  ///,
     PARA{},
     "If the components have been given names (see ", TO directSum, "), use those instead.",
     EXAMPLE lines ///
       	  R = QQ[x,y,z];
	  C = res coker vars R
	  D = (a=>C) ++ (b=>C)
	  D^[a]
	  ///,
     SeeAlso => {directSum, (symbol ^,Matrix,Array), (symbol _,Module,Array),(symbol ^,Module,List)}
     }
 
