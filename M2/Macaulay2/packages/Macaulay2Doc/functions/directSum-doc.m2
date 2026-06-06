--- status: moved December 2020
--- includes ++, directSum, isDirectSum, components, indices, IndexComponents

undocumented {
     (symbol ++,RingElement,ZZ),
     (symbol ++,ZZ,RingElement),
     (symbol ++,Matrix,ZZ),
     (symbol ++,ZZ,Matrix)
     }

document {
    Key => {
	isDirectSum,
       (isDirectSum, Module),
    },
     Headline => "whether something is a direct sum",
     "Works for modules, chain complexes, etc.
     The components of the sum can be recovered with ", TO "components", ".",
     EXAMPLE lines ///
     	  isDirectSum ZZ^6
	  F = ZZ^2 ++ ZZ^3
     	  isDirectSum F
	  components F
     ///
     }

document {
     Key => {
	  (symbol ++,Module,Module),
	  },
     Headline => "direct sum of modules",
     TT "M++N", " -- computes the direct sum of two modules (or coherent sheaves).",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "image vars R ++ kernel vars R",
	  },
     PARA {
     	  "The projection and inclusion maps for direct sums can be obtained with the following methods."
	  },
     UL {
	  TO (symbol ^,Module,Array),
	  TO (symbol _,Module,Array)
	  },
     PARA{
     	  "The components can be recovered later with ", TO "components", " or with ", TO "formation", ".",
	  },
     SeeAlso => directSum}

document {
     Key => {
	  (symbol ++,Matrix,Matrix),
	  (symbol ++,RingElement,Matrix),
	  (symbol ++,Matrix,RingElement),
	  (symbol ++,RingElement,RingElement)
	  },
     Headline => "direct sum of maps",
     TT "f++g", " -- computes the direct sum of two maps between modules.",
     PARA{},
     "If an argument is a ring element or integer, it is promoted
     to a one by one matrix.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "vars R ++ transpose vars R",
      	  "oo^[1]",
      	  "a++b++c",
	  },
     "Selecting rows or columns of blocks:",
     UL {
	  TO (symbol ^,Matrix,Array),
	  TO (symbol _,Matrix,Array)
	  },
     SeeAlso => {directSum, (symbol |, Matrix, Matrix), (symbol ||, Matrix, Matrix)}}

document {
    Key => {
	directSum,
       (directSum, Module),
       (directSum, Matrix),
       (directSum, List),
       (directSum, Option),
       (directSum, Sequence),
       (symbol++, Option, Option)
    },
     Headline => "direct sum of modules or maps",
     TT "directSum(M,N,...)", " -- forms the direct sum of matrices or modules.",
     PARA{
     	  "The components can be recovered later with ", TO "components", " or with ", TO "formation", ".",
	  },
     PARA{},
     "Projection and inclusion maps for direct sums:",
     UL {
	  TO (symbol ^,Module,Array),
	  TO (symbol _,Module,Array),
	  TO (symbol ^,Matrix,List),
	  TO (symbol _,Matrix,List)
	  },
     PARA{},
     "It sometimes happens that the user has indices for the components of
     a direct sum preferable to the usual consecutive small integers.  In 
     this case the preferred indices can be specified with code
     like ", TT "directSum(a=>M,b=>N,...)", ", as in the following example.",
     EXAMPLE {
	  ///F = directSum(a=>ZZ^1, b=>ZZ^2, c=>ZZ^3)///,
	  ///F_[b]///,
	  ///F^[c]///,
	  },
     "Similar syntax works with ", TO "++", ".",
     EXAMPLE {
	  ///F = (a => ZZ^1) ++ (b => ZZ^2)///,
	  ///F_[b]///,
	  },
     SeeAlso => {"++", "indices", formation},
     Subnodes => {
	 TO isDirectSum,
	 TO components,
     }}

document {
    Key => {
	components,
       (components, Module),
       (components, Matrix),
    },
     Headline => "list the components of a direct sum",
     TT "components x", " -- produces a list of the components of an element of a 
     free module.",
     BR{},
     TT "components M", " -- the list of components for a module ", TT "M", " which was
     formed as a direct sum, or ", TT "{M}", " if ", TT "M", " was not formed as a direct sum.
     Works also for matrices, chain complexes, etc.",
     SeeAlso => {"vector", "directSum", "++"},
     Subnodes => { TO indexComponents },
     }

-- TODO:
-- document { 
--      Key => (indices,HashTable),
--      Headline => "preferred indices of a direct sum",
--      Usage => "",
--      Inputs => {
-- 	  },
--      Outputs => {
-- 	  },
--      Consequences => {
-- 	  },     
--      "description",
--      EXAMPLE {
-- 	  },
--      Caveat => {},
--      SeeAlso => {"directSum", "components", "indexComponents"}
--      }

document {
     Key => indexComponents,
     Headline => "specify keys for components of a direct sum",
     TT "indexComponents", " -- a symbol used as a key in a direct sum
     under which to store a hash table in which to register preferred keys used
     to index the components of the direct sum.",
     PARA{},
     SeeAlso => {"directSum", "components", "indices"}}
