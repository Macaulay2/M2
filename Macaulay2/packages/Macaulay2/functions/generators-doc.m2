--- status: TODO
--- author(s): 
--- notes: 

document {
     Key => generators,
     Headline => "provide matrix or list of generators",
     Undocumented => {(generators, EngineRing),(generators, PolynomialRing),(generators, QuotientRing)},
     Usage => "generators x",
     Inputs => { "x" },
     Outputs => { {"provides the generators of ", TT "x", " in a convenient form, as a list of matrix, depending on the type"} },
     PARA,
     "Produces the generators of a Groebner basis, a polynomial ring,
     a monoid ring, a free module, a free group, a submodule given by
     means of generators (or for which generators have been computed),
     or a free monoid.",
     PARA,
     "Usually the result is a list of generators, but the generators of
     a module or Groebner basis are provided as the columns in a matrix.  
     The matrix is stored in a module M under M.generators, unless the matrix
     is the identity matrix.",
     PARA,
     "The symbol ", TT "gens", " is a synonym for ", TT "generators", ".",     
     SeeAlso => {"Monoid", "GroebnerBasis", "Module", "relations", "subquotient"}
     }

document { 
     Key => (generators,GroebnerBasis),
     Headline => "the generators of a Groebner basis",
     Usage => "m = generators g",
     Inputs => { "g" },
     Outputs => {"m" => {"whose columns are the generators of the Groebner basis ", TT "g"}},
     SeeAlso => {"Computing Groebner bases"}
     }
document {
     Key => (generators,Module),
     Usage => "generators M",
     Inputs => {
	  "M" => null
	  },
     Outputs => {
	  {"the matrix of generators of ", TT "M", "."}
	  },
     "Every module in Macaulay2 has, at least implicitly, a generator matrix and a 
     matrix of relations, both of which are matrices between free modules.  
     This function returns the generator matrix.",
     EXAMPLE {
	  "R = GF(8)",
      	  "f = R_0 ++ R_0^2 ++ R_0^3 ++ R_0^4",
      	  "generators image f",
      	  "generators cokernel f"
	  },
     Caveat => {
	  "This function returns a matrix with the given generators.  This 
	  set of generators may not be minimal, or sorted in any particular 
	  order. Use ", TO (trim,Module), " or ", TO (mingens,Module), " instead."
	  },
     SeeAlso => {(relations,Module)}
     }
document { 
     Key => (generators,GeneralOrderedMonoid),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
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
     Key => (generators, Ideal),
     Usage => {"generators I", EM " or ", "gens I"},
     Inputs => {"I"},
     Outputs => { Matrix => {"the one-row matrix whose entries are the generators of ", TT "I"} },
     "Each ideal in ", EM "Macaulay 2", " comes equipped with a one-row
     matrix with the generators of the ideal.
     It is this matrix which is returned.",
     EXAMPLE {
	  "R = ZZ/101[a,b,c];",
      	  "I = ideal(a^2,a*b-2,c^4,a*c-1,a*c-1)",
      	  "generators I"
	  },
     "If you want to remove unnecessary generators, use ", TO trim, ".",
     EXAMPLE {
	  "I = trim I",
	  "gens I"
	  }
     }
document { 
     Key => (generators,MonomialIdeal),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
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
     Key => (generators,Ring),
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Outputs => {
	  },
     Consequences => {
	  },     
     "description",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc1.m2:765:     Key => generators,
 -- doc1.m2:781:     Key => (generators,Module),
 -- doc6.m2:1063:document { Key => degrees, Headline => "degrees of generators" }
 -- doc8.m2:461:     Key => (generators, GroebnerBasis),
 -- doc_ideals.m2:199:     Key => (generators, Ideal),
 -- overviewB.m2:210:     Key => "extracting generators of an ideal",
 -- overviewC.m2:1255:     Key => "minimal presentations and generators",
