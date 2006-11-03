--- status: TODO
--- author(s): 
--- notes: 

document { 
     Key => {gb,
	  (gb,Ideal),
	  (gb,Matrix),
	  (gb,Module)},
     Headline => "compute a Groebner basis",
     Usage => "gb I",
     Inputs => {
		"I" => "an ideal, module, or matrix"
	  },
     Outputs => {
	  GroebnerBasis => "a Groebner basis computation object"
	  },
     Consequences => {
	  },
     "See ", TO "Groebner bases", " for more 
     information and examples.",
     PARA{},
     "The returned value is not the Groebner basis itself.  The
     matrix whose columns form a sorted, auto-reduced Groebner
     basis are obtained by applying ", TO generators, " (synonym: ", TT "gens", ")
     to the result of ", TT "gb", ".",
     EXAMPLE {
	  "R = QQ[a..d]",
	  "I = ideal(a^3-b^2*c, b*c^2-c*d^2, c^3),",
	  "G = gens gb I"
	  },
     SeeAlso => {
	  "Groebner bases",
	  (generators,GroebnerBasis)
	  }
     }

document { 
     Key => [gb, SyzygyLimit],
     Headline => "stop when this number of syzygies is obtained",
     Usage => "gb(..., SyzygyLimit => true)",
     Inputs => {
	  },
     Consequences => {
	  },     
	"This option is meaningful only with ", TT "Syzygies => true", ".",
     Caveat => {
	"More useful with the ", TT "syz", " command than with ", TT "gb", "."
	},
     SeeAlso => {}
     }
document { 
     Key => [gb, SyzygyRows],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
	"This option is meaningful only with ", TT "Syzygies => true", ".",
     EXAMPLE {
	  },
     Caveat => {
	"More useful with the ", TT "syz", " command than with ", TT "gb", "."
	},
     SeeAlso => {}
     }
document { 
     Key => [gb, StopBeforeComputation],
     Headline => "do not actually compute a Groebner basis",
     Usage => "gb(..., StopBeforeComputation => true)",
     TT "StopBeforeComputation => true", " initialises the Groebner basis
     engine and returns a ", TT "GroebnerBasis", " object without actually
     computing the basis.",
     Caveat => {"This feature has not been implemented yet"},
     SeeAlso => {"Groebner bases"}
     }
document { 
     Key => {[gb, Strategy]},
     Headline => "set the Groebner basis strategy",
     Usage => "gb(...,Strategy => s)",
     Inputs => { "s" => Symbol => { "one of the symbols ", TT "LongPolynomial", ", ", TT "Sort", ", or ", TT "UseHilbertFunction" } },
     "See ", TO "Groebner bases", " for examples."
     }
document { 
     Key => {[gb, Algorithm],F4,Faugere,Homogeneous2,LinearAlgebra,Sugarless}, -- also: Homogeneous,Inhomogeneous
     Headline => "set the Groebner basis algorithm",
     Usage => "gb(...,Algorithm => s)",
     Inputs => { "s" => Symbol => { "one of the symbols ", TT "F4", ", ", TT "Faugere", ", ", TT "Homogeneous", ", ", TT "Homogeneous2", ", ",
	       TT "Inhomogeneous", ", ", TT "LinearAlgebra", ", or ", TT "Sugarless" }}
     }
document { 
     Key => [gb, Syzygies],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
	"This option can take values ", TT "true", " or ", TT "false", ".",
     EXAMPLE {
	  },
     Caveat => {
	"More useful with the ", TT "syz", " command than with ", TT "gb", "."
	},
     SeeAlso => {}
     }
document { 
     Key => [gb, HardDegreeLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
	  "Throws away all S-pairs of degrees beyond the limit. The computation
	  will be re-initialized if higher degrees are required.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, DegreeLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, ChangeMatrix],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, PairLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, BasisElementLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, CodimensionLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, GBDegrees],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, StopWithMinimalGenerators],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, Hilbert],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
document { 
     Key => [gb, SubringLimit],
     Headline => "",
     Usage => "",
     Inputs => {
	  },
     Consequences => {
	  },     
     "See ", TO "Groebner bases", " for examples.",
     EXAMPLE {
	  },
     Caveat => {},
     SeeAlso => {}
     }
 -- doc8.m2:479:     Key => symbol gbTrace,
 -- doc8.m2:503:     Key => gb,
 -- doc8.m2:523:     Key => [gb,StopBeforeComputation],
 -- doc8.m2:542:     Key => [gb,DegreeLimit], 
 -- doc8.m2:571:     Key => [gb,SyzygyLimit], 
 -- doc8.m2:602:     Key => [gb,PairLimit], 
 -- doc8.m2:637:     Key => [gb,CodimensionLimit], 
 -- doc8.m2:668:     Key => [gb,StopWithMinimalGenerators], 
 -- doc8.m2:693:     Key => [gb,Strategy], 
 -- doc8.m2:772:     Key => [gb,ChangeMatrix], 



-- document {
--      Key => [gb,ChangeMatrix], 
--      Headline => "whether to produce the change of basis matrix",
--      TT "ChangeMatrix => true", " -- an optional argument for ", TO "gb", " which
--      specifies whether to compute the change of basis matrix from the basis to
--      the original generators.",
--      PARA{},
--      "Intended for internal use only."
--      }
-- document {
--      Key => [gb,Strategy], 
--      Headline => "specify the strategy used to compute Groebner bases",
--      TT "gb(f,Strategy => v)", " -- an option for ", TO "gb", " which can
--      be used to specify the strategy to be used in the computation.",
--      PARA{},
--      "The strategy option value ", TT "v", " should be one of the following.",
--      UL {
-- 	  TO "Primary",
--      	  TO "Homogeneous",
-- 	  TO "Inhomogeneous",
-- 	  TO "LongPolynomial",
-- 	  TO "Sort"
-- 	  }
--      }
-- document {
--      Key => [gb,StopWithMinimalGenerators], 
--      Headline => "stop when minimal generators have been determined",
--      TT "StopWithMinimalGenerators", " -- keyword for an optional argument used 
--      with ", TO "gb", ", which, if the value provided is ", TT "true", "
--      indicates that the computation should stop as
--      soon as a complete list of minimal generators for the submodule
--      or ideal has been determined, even if the entire Groebner basis
--      has not yet been determined.",
--      PARA{},
--      "Currently this option is implemented by stopping the computation
--      as soon as the S-polynomials and generators of the same 
--      degree as the generator of highest degree have been processed.",
--      PARA{},
--      "This option is for internal use only.  Use ", TO "mingens", "
--      instead."
--      }
-- document {
--      Key => [gb,CodimensionLimit], 
--      Headline => "stop when this codimension is reached",
--      TT "CodimensionLimit => n", " -- keyword for an optional argument used with
--      ", TO "gb", " which specifies that the computation should stop when
--      the codimension of the zero set of the ideal (or submodule) generated
--      by the leading terms of the Groebner basis elements found so far reaches 
--      a certain limit.",
--      PARA{},
--      "This option has not been implemented yet.",
--      PARA{},
--      "Eventually the codimension of the ideal of leading terms is the
--      codimension of the original ideal."
--      }
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
-- 

-- document {
--      Key => [gb,DegreeLimit], 
--      TT "DegreeLimit => n", " -- keyword for an optional argument used with
--      ", TO "gb", " which specifies that the computation should halt after 
--      dealing S-polynomials up to degree ", TT "n", ".",
--      PARA{},
--      "This option is relevant only for homogeneous matrices.",
--      PARA{},
--      "For an example, see ", TO "Groebner bases", "."
--      }
-- 
-- document {
--      Key => [gb,StopBeforeComputation],
--      Headline => "whether to stop the computation immediately",
--      TT "StopBeforeComputation => true", " -- an optional argument used with ", TO "gb", ".",
--      PARA{},
--      "Tells whether not to start the computation, with the default value
--      being ", TT "false", ".  This can be useful when you want to obtain
--      the partially computed Groebner basis contained in an interrupted
--      computation."
--      }
-- 
-- 
-- document {
--      Key => gb,
--      Headline => "compute a Groebner basis",
--      TT "gb f", " -- compute the Groebner basis for the image of a ", TO "Matrix", " ", TT "f", ".",
--      PARA{},
--      "If the computation is interrupted, then the partially completed
--      Groebner basis is available as ", TT "f#{t,i}", ", where ", TT "t", " is true or
--      false depending on whether syzygies or the change of basis matrix are 
--      to be computed, and ", TT "i", " is the number of rows of the syzygy matrix to 
--      be retained.  The computation can be continued by repeating the 
--      ", TT "gb", " command with the same options."
--      }

TEST ///
-- Test of various stopping conditions for GB's
R = ZZ/32003[a..j]
I = ideal random(R^1, R^{-2,-2,-2,-2,-2,-2,-2});
gbTrace=3
--time gens gb I;
I = ideal flatten entries gens I;
G = gb(I, StopBeforeComputation=>true); -- now works

I = ideal flatten entries gens I;
mingens I; -- works now

I = ideal flatten entries gens I;
trim I -- It should stop after mingens are known to be computed.

I = ideal flatten entries gens I;
G = gb(I, DegreeLimit=>3); -- this one works
G = gb(I, DegreeLimit=>4); -- this one works
G = gb(I, DegreeLimit=>3); -- this one stops right away, as it should

I = ideal flatten entries gens I;
G = gb(I, BasisElementLimit=>3); -- does the first 3, as it should
G = gb(I, BasisElementLimit=>7); -- does 4 more.

I = ideal flatten entries gens I;
G = gb(I, PairLimit=>3); -- 

I = ideal flatten entries gens I;
hf = poincare ideal apply(7, i -> R_i^2)
G = gb(I, Hilbert=>hf); -- this works, it seems

Rlex = ZZ/32003[a..j,MonomialOrder=>Eliminate 1]
IL = substitute(I,Rlex);
G = gb(IL, SubringLimit=>1, Hilbert=>hf, DegreeLimit=>2); -- SubringLimit now seems OK
G = gb(IL, SubringLimit=>1, Hilbert=>hf, DegreeLimit=>4); 
assert(numgens source selectInSubring(1,gens G) == 1)
I = ideal flatten entries gens I;
G = gb(I, DegreeLimit=>1); 
--G = gb(I, CodimensionLimit=>3); -- this isn't implemented yet, and is ignored...

///
