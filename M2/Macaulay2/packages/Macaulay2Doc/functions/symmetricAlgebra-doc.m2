--- status: Draft
--- author(s): MES
--- notes: 

document { 
     Key => {(symmetricAlgebra,Module),symmetricAlgebra,
	  [symmetricAlgebra, WeylAlgebra],[symmetricAlgebra, VariableBaseName],
	  [symmetricAlgebra, SkewCommutative],[symmetricAlgebra, MonomialSize],[symmetricAlgebra, Weights],
	  [symmetricAlgebra, Local],[symmetricAlgebra, Inverses],[symmetricAlgebra, Heft],
	  [symmetricAlgebra, Global],[symmetricAlgebra, Degrees],[symmetricAlgebra, DegreeRank],
	  [symmetricAlgebra, ConstantCoefficients], [symmetricAlgebra, MonomialOrder], [symmetricAlgebra, Variables]
	  },
     Headline => "the symmetric algebra of a module",
     Usage => "symmetricAlgebra M",
     Inputs => {
	  "M"
	  },
     Outputs => {
	  Ring => {"the symmetric algebra of ", TT "M"}
	  },
     EXAMPLE lines ///
     	  R = QQ[a..d];
	  M = image matrix{{a,b,c}}
	  symmetricAlgebra M
     	  symmetricAlgebra(R^{1,2,3})
	  ///,
     PARA{},
     "Most of the optional arguments for monoids (see ", TO (symbol SPACE, Ring,Array), " or ", TO "monoid", ")
     are available here as well, such as:",
     EXAMPLE lines ///
	  symmetricAlgebra(M, Variables=>{x,y,z})
	  symmetricAlgebra(M, VariableBaseName=>G, MonomialSize=>16)
	  symmetricAlgebra(M, Degrees=> {7:1})
          ///,
     Caveat => {"This function predates the ability to create polynomial rings over polynomial rings,
	  and perhaps that is what should be returned."},
     }
end
document { 
     Key => [symmetricAlgebra, Variables],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, Degrees],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, MonomialOrder],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, MonomialSize],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, Inverses],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, WeylAlgebra],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, VariableBaseName],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, SkewCommutative],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, DegreeRank],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [symmetricAlgebra, Weights],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }

