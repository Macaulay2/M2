document { 
     Key => {
	  (symbol _, ChainComplex, ZZ),     -- component
	  (symbol _, GradedModule, ZZ)},     -- component
     Headline => "component",
     Usage => "C_i",
     Inputs => {
	  "C" => {"or ", ofClass GradedModule},
	  "i"
	  },
     Outputs => {
	  Module => { "the ", TT "i", "-th component of ", TT "C"}
	       },
     EXAMPLE lines ///
     	  R = QQ[x,y,z]/(x^3,y^3,z^3,x*y*z);
	  C = res(coker vars R, LengthLimit=>8)
     	  rank C_7
	  C.dd_3
	  ///,
     SUBSECTION "Programming hint",
	  "The chain complex ", TT "C", " is implemented as a hash table, 
	  but since the computation of a projective resolution 
	  can be stopped prematurely, Macaulay2 doesn't bother
	  populating the hash table with the relevant free modules 
	  until explicitly requested by the user, for example, in response to the
	  command ", TT "C_i", " described above.  The hash table ", 
	  TT "C", " can be examined directly with code like ", TT "C#i", ", but in order to populate 
	  the hash table completely, use ", TO (complete, ChainComplex), ".",
     SeeAlso => {resolution,
	  (symbol ^, ChainComplex, ZZ),
	  (symbol _, ChainComplexMap, ZZ)}
     }

document {
     Key => {(symbol _, ChainComplexMap, ZZ),
	  (symbol _, GradedModuleMap, ZZ)},
     Headline => "component map",
     Usage => "p_i",
     Inputs => {
	  "p" => ("a map ", TT "D", " <- ", TT "C", " of chain complexes, of degree ", TT "d", ", say"),
	  "i"
	  },
     Outputs => { Matrix => {"the component ", TT "D_(i+d) <- C_i"} },
     EXAMPLE lines ///
	  R = ZZ/101[a..c];
	  I = image vars R
	  J = image symmetricPower (2,vars R)
	  g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))
	  g_1
	  g_2
	  ///,
     "The map ", TT "p", " may also be ", ofClass GradedModuleMap, ".",
     SeeAlso => { (symbol _, ChainComplex, ZZ), extend, resolution, image, vars, symmetricPower }
     }

document {
     Key => ((symbol _,symbol =),ChainComplex,ZZ),
     Headline => "install component of chain complex",
     Usage => "C_i = M",
     Inputs => { "C" , "i", "M" },
     Outputs => {{"install ", TT "M", " as the ", TT "i", "-th module of the chain complex ", TT "C"}},
     EXAMPLE lines ///
	  R = ZZ[x..z]
	  C = chainComplex R
	  C_2 = R^11
	  C_4 = R^13
	  C
     ///,
     SeeAlso => {((symbol _,symbol =),ChainComplexMap,ZZ)}}

document {
     Key => ((symbol _,symbol =),ChainComplexMap,ZZ),
     Headline => "install component of chain complex map",
     Usage => "f_i = g",
     Inputs => { "f" , "i", "g" },
     Outputs => {{"install ", TT "g", " as the ", TT "i", "-th module of the chain complex map ", TT "f"}},
     EXAMPLE lines ///
	  R = ZZ[x..z]
	  C = chainComplex R
	  C.dd
	  C.dd_1 = vars R
	  C.dd_3 = transpose vars R
	  C.dd
	  C
	  HH C
	  prune HH C
     ///,
     SeeAlso => {((symbol _,symbol =),ChainComplex,ZZ)}}
 
document {
     Key => (symbol _,ChainComplex,Array),
     Headline => "inclusion from summand",
     Usage => "C_[i,j,...,k]",
     Inputs => {"C" => {ofClass ChainComplex},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => {ofClass ChainComplexMap}
	  },
     PARA{},
     "The chain complex ", TT "C", " should be a direct sum, and the result is the map
     corresponding to inclusion from the sum of the components numbered or named
     ", TT "i, j, ..., k", ".",
     PARA{},
     EXAMPLE lines ///
	  R = QQ[a,b];
	  C = res coker vars R
	  D = C ++ C
	  D_[0]
	  D_[1,0]
	  ///,
     PARA{},
     "If the components have been given names (see ", TO directSum, "), use those instead.",
     EXAMPLE lines ///
	  D = (a=>C) ++ (b=>C)
	  D_[a]
	  ///,
     SeeAlso => {directSum, (symbol ^, ChainComplex, Array), (symbol ^, Module, Array)}
     }

document { 
     Key => {
	  (symbol _, ChainComplexMap, Array),
	  (symbol _, GradedModuleMap, Array)
	  },
     Headline => "component of map corresponding to summand of source",
     Usage => "F_[i,j,...,k]",
     Inputs => {"F" => {"or ", ofClass{GradedModuleMap}},
	  Nothing => {TT "[i,j,...,k]", ", an array of indices"}},
     Outputs => {
     	  Nothing => ofClass{ChainComplexMap, GradedModuleMap}
	  },
     "The source of the chain complex map ", TT "F", " should be a 
     direct sum, and the result is the component of this map 
     corresponding to the sum of the components numbered or named
     ", TT "i, j, ..., k", ".  Free modules are regarded as direct sums of modules.
     In otherwords, this routine returns the map given by certain blocks of columns.",
     EXAMPLE lines ///
          R = ZZ[a..d];
          F = (vars R) ++ ((vars R) ++ matrix{{a-1,b-3}})
	  F_[1]
	  F_[1]^[1]
          ///,
     PARA{"If the components have been given names (see ", TO directSum, "), use those instead."},
     EXAMPLE lines ///
	  N = (a=>vars R) ++ (b=>vars R)
	  N_[a]
     	  N = directSum(x1 => matrix{{a,b-1}}, x2 => matrix{{a-3,b-17,c-35}}, x3 => vars R)
	  N_[x1,x3]
	  ///,
     SeeAlso => {(symbol^, ChainComplexMap, Array), (symbol_, ChainComplex, Array), directSum}
     }
