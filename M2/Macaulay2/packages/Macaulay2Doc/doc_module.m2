-- also includes: vector
-- TODO: (module, Ideal)

document {
     Key => module,
     Headline => "make or get a module"
     }

document { Key => (module, Ring),
     Usage => "module R",
     Inputs => {"R"},
     Outputs => {{"the free module of rank 1 over the ring R"}},
     EXAMPLE lines ///
     	  ZZ
	  module ZZ
     ///}

document { Key => (module, Vector),
     Headline => "the module of a vector",
     Usage => "module v",
     Inputs => {"v"},
     Outputs => {{"the module that contains the vector ", TT "v"}},
     "The class of ", TT "v", " is also equal to the module of ", TT "v", ".",
     EXAMPLE lines ///
     	  F = ZZ^4
	  v = F_2
	  module v
	  class v
     ///}

document { Key => (module, SheafOfRings),
     SeeAlso => { Variety, OO },
     Usage => "module F",
     Inputs => { "F" },
     Outputs => { { "the module corresponding to ", TT "F" }},
     EXAMPLE lines ///
     	  R = QQ[x..z]
     	  X = Proj R
	  OO_X^6
	  module oo
     ///
     }

document {
     Key => {relations,(relations, Module)},
     Headline => "the defining relations",
     TT "relations M", " -- produce the relations defining a module M.",
     PARA{},
     "The relations are represented as a matrix, and if not stored
     in the module under M.relations, the matrix is understood to be
     empty.",
     PARA{},
     SeeAlso => {"generators","subquotient"}}

document {
     Key => {vector,(vector,List),(vector,Matrix)},
     Headline => "make a vector",
     TT "vector {a,b,c,...}", " -- produces an element of a free module from a list.",
     PARA{},
     "The elements a,b,c,... must be elements of the same ring, or be
     convertible to elements of the same ring."}

document {
     Key => {super,(super, GradedModule),(super, CoherentSheaf),(super, Matrix),(super, Module),(super,Vector)},
     Headline => "get the ambient module",
     TT "super M", " -- yields the module that the module ", TT "M", " is a submodule of.",
     BR{},
     TT "super f", " -- if ", TT "f", " is a map whose target is a submodule 
     of ", TT "M", ", yields the composite of ", TT "f", " with the inclusion into ", TT "M", ".",
     PARA{},
     SeeAlso => { "cover", "ambient" }}
