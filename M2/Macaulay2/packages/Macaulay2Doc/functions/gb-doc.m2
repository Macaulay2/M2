-- -*- coding: utf-8 -*-
--- status: TODO
--- author(s): 
--- notes: 

undocumented (NewFromMethod, GroebnerBasis, Matrix, GroebnerBasisOptions, OptionTable)

document { 
     Key => {gb,
	  (gb,Ideal),
	  (gb,Matrix),
	  (gb,Module),
	  [gb, BasisElementLimit],
	  [gb,ChangeMatrix],
	  [gb,CodimensionLimit],
	  [gb,DegreeLimit],
     	  [gb,GBDegrees],
	  [gb,HardDegreeLimit],
	  [gb,Hilbert],
	  [gb,PairLimit],
	  [gb,StopBeforeComputation],
	  [gb,StopWithMinimalGenerators],
     	  [gb,Strategy],
	  [gb,SubringLimit],
	  [gb,Syzygies],
	  [gb,SyzygyLimit],
	[gb, SyzygyRows],
	[gb, MaxReductionCount],
	(status, GroebnerBasis),
	  },
     Headline => "compute a Gröbner basis",
     Usage => "gb I",
     Inputs => {
	  "I" => "an ideal, module, or matrix",
	  Algorithm => Symbol => { "possible values: ",
	      TT "Homogeneous", ", ", TT "Inhomogeneous", ", ", TT "Homogeneous2", ", and ", TT "Sugarless", ". ",
	      "Experimental options include ", TT "LinearAlgebra", " and ", TT "Toric", "."},
     	  BasisElementLimit => ZZ => "stop when this number of (nonminimal) Gröbner basis elements has been found",
	  ChangeMatrix => Boolean => { 
	       "whether to compute the change of basis matrix from Gröbner basis elements to original generators.  Use ", TO "getChangeMatrix", " to recover it."},
	  CodimensionLimit => ZZ => "stop computation once codimension of submodule of lead terms reaches this value (not functional yet)",
	  DegreeLimit => List => "stop after the Gröbner basis in this degree has been computed",
	  GBDegrees => List => "a list of positive integer weights, one for each variable in the ring, to be used for
	   organizing the computation by degrees (the 'sugar' ecart vector)",
	  HardDegreeLimit => {
	       "throws away all S-pairs of degrees beyond the limit. The computation
	       will be re-initialized if higher degrees are required."},
     	  Hilbert => {"informs Macaulay2 that this is the ", TO poincare, 
	       " polynomial, and can be used to aid in the computation of the Gröbner basis (Hilbert driven)"},
      	  MaxReductionCount => ZZ => {
	       "the maximum number of reductions of an S-pair done before requeueing it, if the 
	       ", TT "Inhomogeneous", " algorithm is in use"
	       },
	  PairLimit => ZZ => "stop after this number of spairs has been considered",
	  StopBeforeComputation => Boolean => "whether to initialize the Gröbner basis engine but return before doing any computation (useful for 
	    using or viewing partially computed Gröbner bases)",
	  StopWithMinimalGenerators => Boolean => "whether to stop as soon as the minimal set (or a trimmed set, if not homogeneous or local) of generators is known.  Intended for internal use only",
	  Strategy => {
	       "either ", TO "LongPolynomial", ", ", TO "Sort", ", or a list of these.  ",
	       TO "LongPolynomial", ": use a geobucket data structure while reducing polynomials; ",
	       TT "Sort", ": sort the S-pairs by lead term (usually this is a bad idea). ",
	       "Another symbol usable here is ", TT "UseSyzygies", ". ",
	       "Usually S-pairs are processed degree by degree in the order that they were constructed."},
	  SubringLimit => ZZ => "stop after this number of elements of the Gröbner basis lie in the first subring",
	  Syzygies => Boolean => "whether to collect syzygies on the original generators during the computation.  Intended for internal use only",
	  SyzygyLimit => ZZ => "stop when this number of non-zero syzygies has been found",
	  SyzygyRows => ZZ => "for each syzygy and change of basis element, keep only this many rows of each syzygy"
	  },
     Outputs => {
	  GroebnerBasis => "a Gröbner basis computation object"
	  },
     "See ", TO "Gröbner bases", " for more 
     information and examples.",
     PARA{},
     "The returned value is not the Gröbner basis itself.  The
     matrix whose columns form a sorted, auto-reduced Gröbner
     basis are obtained by applying ", TO generators, " (synonym: ", TT "gens", ")
     to the result of ", TT "gb", ".",
     EXAMPLE {
	  "R = QQ[a..d]",
	  "I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3)",
	  "G = gens gb I"
	  },
     PARA {
	  "When ", TT "I", " is a subquotient module ", TT "M/N", " of a free module ", TT "F", ", then ", TT "N", " is generated by ", TT "relations I", "
	  and ", TT "M", " is generated by the concatenated matrix ", TT "generators I || relations I", " -- it is the Gröbner basis of that matrix which
	  is computed, so that reduction modulo the Gröbner basis can be used to determine membership in ", TT "M", ".  When relations are present, the
	  option ", TO "SyzygyRows", " is set to the number of columns of ", TT "generators I", ", so that if ", TT "ChangeMatrix => true", " is used, then 
	  division by the Gröbner basis can be to express
	  an element of ", TT "F", " as a linear combination of columns of ", TT "generators I", ", avoiding the computation of the coefficients of the columns
	  of ", TT "relations I", ", leaving all the information that is required to specify an element of ", TT "I", "."
	  },
     EXAMPLE lines ///
     R = QQ[x,y]
     M = subquotient(matrix {{x}}, matrix {{x+y}})
     gens gb M
     matrix {{x}} // gb(M,ChangeMatrix=>true)
     matrix {{y}} // gb(M,ChangeMatrix=>true)
     ///,
     SeeAlso => {
	  groebnerBasis,
	  (generators,GroebnerBasis),
	  poincare,
	  },
     Subnodes => {
	 TO GroebnerBasis,
	 TO GroebnerBasisOptions,
	 TO "Gröbner basis algorithms",
	 -- Mike wanted this: installGroebner,
	 TO gbSnapshot,
	 TO gbRemove,
	 TO "gbTrace",
	 TO LongPolynomial,
         }
     }

doc ///
Node
  Key
    "Gröbner basis algorithms"
    [gb, Algorithm]
    LinearAlgebra
    Homogeneous2
    Sugarless
    Toric
    UseSyzygies
  Usage
    gb(I, Algorithm => ...)
  Description
    Tree
      :Supported algorithms for @TO gb@
        @TT "Homogeneous"@
	@TT "Inhomogeneous"@
	@TT "Homogeneous2"@
	@TT "Sugarless"@
      :Experimental algorithms for @TO gb@
        @TT "LinearAlgebra"@
	@TT "Toric"@
  SeeAlso
    groebnerBasis
    markedGB
    "FGLM::FGLM"
///

document {
     Key => symbol gbTrace,
     Headline => "current engine computation tracing level",
     TT "gbTrace = n", " -- set the tracing level for ", TO "the engine of Macaulay2", " to
     level ", TT "n", ".  Meaningful values of ", TT "n", " for typical users are
     0, 1, 2, and 3.  Meaningful values for the developers are 4, 5, 8, 10, 11, and 100; the
     parity also has an effect when the value is at least 5.",
     PARA{},
     "The notations used in tracing are :",
     UL {
	  "g       - a generator reduced to something nonzero and has been added to the basis.",
	  "m       - an S-pair reduced to something nonzero and has been added to the basis.",
	  "z       - an S-pair reduced to zero, and a syzygy has been recorded.",
	  "u       - an S-pair reduced to zero, but the syzygy need not be recorded.",
	  "o       - an S-pair or generator reduced to zero, but no new syzygy occurred.",
	  "r       - an S-pair has been removed.",
	  "{2}     - beginning to reduce the S-pairs of degree 2.",
	  "(7)     - 7 more S-pairs need to be reduced.",
	  LI {"(8,9)   - 9 S-pairs, 8 predicted basis elements (", TO [gb,Hilbert], ")"},
	  ".       - a minor has been computed, or something has happened while computing a resolution.",
	  },
     SeeAlso => { "debugLevel", "engineDebugLevel" },
     }

document {
     Key => GroebnerBasis,
     Headline => "the class of all Gröbner bases",
     "A Gröbner basis in Macaulay2 consists of a Gröbner basis
     computation, and several associated matrices. Normally you don't
     need to refer to these objects directly, as many operations on
     matrices and modules create them, and refer to them.  For more
     information, see ", TO "Gröbner bases", ".",
    Subnodes => {
	TO returnCode,
	TO (generators, GroebnerBasis),
        TO (mingens, GroebnerBasis),
        TO (syz, GroebnerBasis),
        TO (target, GroebnerBasis),
	TO (getChangeMatrix, GroebnerBasis),
        },
     }

document {
     Key => GroebnerBasisOptions,
     "This class is used internally to record the options used with ", TO "gb", " when the resulting Gröbner basis is
     cached inside a matrix."
     }

document {
     Key => returnCode,
     TT "returnCode", " --  a key for a ", TO "GroebnerBasis", " under which is
     stored the return code from the engine for the computation."
     }

document {
     Key => {(markedGB, Matrix, Matrix), markedGB,
	  [markedGB,SyzygyMatrix],[markedGB,MinimalMatrix],[markedGB,ChangeMatrix]},
     Usage => "markedGB(lt,m)",
     Headline => "make a marked Gröbner basis",
     Inputs => {
	  "lt" => {"the matrix of monomials in (the columns of) ", TT "m", " to mark as lead terms, with respect to an
	       unspecified monomial ordering"},
	  "m" => {"the matrix whose columns are to form the generators of a Gröbner basis"},
	  SyzygyMatrix => Matrix => {"the matrix of syzygies"},
	  MinimalMatrix => Matrix => {"the matrix of minimal generators" },
	  ChangeMatrix => Matrix => {"the change-of-basis matrix" }
	  },
     Outputs => {
	  GroebnerBasis => {"the resulting Gröbner basis"}
	  }
     }

document {
     Key => LongPolynomial,
     Headline => "a Strategy option value",
     TT "LongPolynomial", " -- a strategy used with the keyword ", TO "Strategy", ".",
     PARA{},
     "Indicates that during computation of a Gröbner basis, the reduction
     routine will be replaced by one that will handle long polynomials more
     efficiently using \"geobuckets\", which accommodate the terms in buckets
     of geometrically increasing length.  This method was first used
     successfully by Thomas Yan, graduate student in CS at Cornell.",
     SeeAlso => {[gb,Strategy]}
     }

-- document {
--      Key => [gb,PairLimit], 
--      Headline => "stop when this number of pairs is handled",
--      TT "PairLimit", " -- keyword for an optional argument used with
--      ", TO "gb", " which specifies that the
--      computation should be stopped after a certain number of S-pairs
--      have been reduced.",
--      EXAMPLE {
-- 	  "R = QQ[x,y,z,w]",
--       	  "I = ideal(x*y-z,y^2-w-1,w^4-3)",
--       	  "gb(I, PairLimit => 1)",
--       	  "gb(I, PairLimit => 2)",
--       	  "gb(I, PairLimit => 3)"
-- 	  }
--      }

doc ///
   Key
     groebnerBasis
     (groebnerBasis,Ideal)
     (groebnerBasis,Module)
     (groebnerBasis,Matrix)
     [groebnerBasis,Strategy]
   Headline
     Gröbner basis, as a matrix
   Usage
     M = groebnerBasis I
     M = groebnerBasis(I, Strategy=>"MGB")
     M = groebnerBasis(I, Strategy=>"F4")
   Inputs
     I:Ideal
       or a module or a matrix (in which case the result is the Gröbner basis of the submodule
         generated by the columns)
     Strategy => String
       If not given, use the default algorithm.  If given, value must be "MGB"
       or "F4", and the result is experimental
     "MGBOptions" => List
       For internal use only.  Warning: the interface is likely to change.
   Outputs
     M:Matrix
       The matrix whose columns are the generators of the Gröbner basis of {\tt I}.
       In the non-local monomial order case, the result is auto-reduced, and sorted.
   Description
    Text
      With no {\tt Strategy} option, this just calls @TO "gb"@.
    Example
      R = QQ[a..d]
      M = groebnerBasis random(R^1,R^{4:-2});
      netList (ideal M)_*
    Text
      With a {\tt Strategy} option, the code is experimental, subject to
      interface changes, and might have bugs.  So use at your own
      risk!  However, it appears to work correctly and is often very
      fast, in cases where it applies.  If you encounter any bugs,
      please let us know!

      If either {\tt "MGB"} (MGB stands for mathicGB, the name of the package used),
      or {\tt "F4"} is given for the Strategy, then 
      experimental code (written by Bjarke Roune and M. Stillman) is used.
      The plan is for this to become the default version for Gröbner bases in later
      versions of Macaulay2.  But for now, it is experimental.
      
      These strategies only work for ideals in polynomial rings over a finite field ZZ/p.
      In other cases, either an error will be given, or the current default Gröbner
      basis algorithm will be used.
    Example
      R = ZZ/101[a..e]
      I = ideal sub(random(R^1, R^{4:-2}), e=>1);
      netList I_*
      gbI = ideal groebnerBasis(I, Strategy=>"MGB");
      netList gbI_*
    Text
      Also implemented is a Faugere-like algorithm that is sometimes much faster
      (but also sometimes takes a large amount of memory).
    Example
      gbTrace=1
      gbI = ideal groebnerBasis(I, Strategy=>"F4");
      netList gbI_*
   Caveat
     (1) The MGB and F4 options are experimental, work only over a finite field of char $< 2^{32}$, not over
     quotient rings, and not over exterior or Weyl algebras.  However, these versions can be much
     faster when they apply. (2) The experimental versions do not stash their results into the ideal
     or module. (3) The experimental version only works for ideals currently.
   SeeAlso
     gb
///

document {
    Key => {
	 getChangeMatrix,
	(getChangeMatrix, GroebnerBasis)
    },
    Headline => "get the change of basis matrix",
    TT "getChangeMatrix G", " -- for a Gröbner basis G, return the change of
    basis matrix from the Gröbner basis to another generating set,
    usually a minimal, or original, generating set.",
    PARA{},
    "The option ", TO "ChangeMatrix", " can be used with ", TO "gb",
    " to enable the computation of the change of basis matrix."
}
