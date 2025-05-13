-- -*- coding: utf-8 -*-
--		Copyright 1993-2002 by Daniel R. Grayson

document {
     Key => Resolution,
     Headline => "the class of all resolution computations",
     PARA{},
     "These resolutions are internal engine objects not meant to be examined
     by the user.",
     PARA{},
     "The symbol ", TT "Resolution", " is also used in a ", TO "ChainComplex", " to
     store the resolution it comes from.",
     Subnodes => {
	 TO resolution,
         },
     }

document {  -- This node is used as an example in the node: Key
     Key => resolution,
     Headline => "projective resolution",
     Subnodes => {
	TO (resolution, Ideal),
	TO (resolution, Matrix),
	TO (resolution, Module),
	TO [resolution, DegreeLimit],
        TO [resolution, SyzygyLimit],
        TO [resolution, PairLimit],
        TO [resolution, StopBeforeComputation],
        TO [resolution, LengthLimit],
        TO [resolution, HardDegreeLimit],
        TO [resolution, Strategy],
        TO [resolution, SortStrategy],
	TO [resolution, FastNonminimal]
        },
     }
document {
     Key => [resolution,DegreeLimit],
     Headline => "compute only up to this degree",
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "resolution", " which specifies that the computation should halt
     after dealing with degree n.",
     PARA{},
     "This option is relevant only for homogeneous modules.",
     PARA{},
     Caveat => "One might get some matrix entries of slightly higher degree than requested.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2}}",
      	  "res(M,DegreeLimit => 1)",
      	  "res(M,DegreeLimit => 2)"
	  },
     }

document { -- This node is used as an example in the node: Key
     Key => [resolution,SyzygyLimit],
     Headline => "stop when this number of syzygies is reached",
     TT "SyzygyLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     stop after a certain number of syzygies have computed.",
     PARA{},
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2,w^4}}",
      	  "res(M,SyzygyLimit => 1)",
      	  "res(M,SyzygyLimit => 2)",
      	  "res(M,SyzygyLimit => infinity)"
	  }
     }

document {
     Key => [resolution,PairLimit],
     Headline => "stop when this number of pairs has been handled",
     TT "PairLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     be stopped after a certain number of S-pairs have been reduced.",
     EXAMPLE {
	  "R = QQ[x,y,z,w]",
      	  "M = cokernel matrix {{x*y-z,y^2-w-1,w^4-3}}",
      	  "res(M, PairLimit => 1)",
      	  "res(M, PairLimit => 10)",
      	  "res(M, PairLimit => 20)"
	  }
     }

document {
     Key => [resolution,StopBeforeComputation],
     Headline => "whether to stop the computation immediately",
     TT "StopBeforeComputation", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA{},
     "Tells whether to start the computation, with the default value
     being ", TT "true", ".  This can be useful when you want to obtain
     the partially computed resolution contained in an interrupted computation."
     }

document {
     Key => [resolution,LengthLimit],
     Headline => "stop when the resolution reaches this length",
     TT "LengthLimit", " -- keyword for an optional argument used with
     ", TO "resolution", " which indicates how long a resolution to make.",
     PARA{},
     "For polynomial rings over a field or over the integers, the length
     is taken to be the dimension of the ring, so the complete resolution will
     be obtained.  For quotient rings of such rings, the same number is used,
     so the complete resolution may not be obtained.",
     PARA{},
     "In the current version, asking for a second and longer resolution of the
     same module involves recomputing the resolution from scratch.  Eventually
     the previous work will be used and the recomputation will go quicker.",
     PARA{},
     "The resolution returned may actually be one step longer than requested.
     The extra differential is not guaranteed to be minimal."
     }

document {
     Key => [resolution,HardDegreeLimit],
     TT "HardDegreeLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA{},
     "The default value is ", TT "{}", ".",
     PARA{},
     "Information above the specified degree is discarded."
     }

document {
     Key => [resolution,Strategy],
     TT "Strategy => n", " -- an option for ", TO "resolution", " which specifies
     which algorithm to use.  Strategies are specified by number and the
     algorithms available are",
     UL {
	  SPAN (TT "Strategy => 0", " -- Compute syzygies on the Gröbner bases of each syzygy
	       module.  The algorithm uses important speedups due to R. La Scala.
	       This algorithm appears to be on the average the fastest."),
	  SPAN (TT "Strategy => 1", " -- An older version of algorithm 0, which doesn't allow as
	       much experimentation, but can sometimes be marginally faster."),
	  SPAN (TT "Strategy => 2", " -- Compute syzygies on the minimal generators of each
	       matrix in the resolution.  Over quotient rings, it's preferred."),
	  SPAN (TT "Strategy => 3", " -- Same as algorithm 2, but compute those Hilbert functions
	       which allow removal of S-pairs (a la Robbiano, et al.). Sometimes this
	       improvement can be very dramatic.")
	  },
     "All algorithms use induced monomial orders (Schreyer orders), since
     this makes an enormous improvement to the efficiency of the algorithm."
     }

document {
     Key => [resolution,SortStrategy],
     TT "SortStrategy => n", " -- an option for ", TO "resolution", " which
     specifies the strategy to be used for sorting S-pairs.",
     PARA{},
     "Not implemented yet."
     }

document {   -- This node is used as an example for the documentation node: Key, Usage
     Key => (resolution, Module),
     Headline => "compute a free resolution of a module",
     Usage => "resolution M\nres M",
     Inputs => { "M" },
     Outputs => { {"a free resolution of ", TT "M"} },
     PARA {
     	  "The given generators and relations are used to determine a ", TO "presentation", " of ", TT "M", " to serve as the first matrix of the free
     	  resolution; if the presentation is not minimal, and a minimal resolution is desired, use ", 
     	  TT "resolution minimalPresentation M", " instead."},
     PARA {"Warning: the resolution can have free modules with unexpected ranks
	  when the module ", TT "M", " is not homogeneous.  Here is an example
	  where even the lengths of the resolutions differ.  We compute
	  a resolution of the kernel of a ring map in two ways.
	  The ring ", TT "R", " is constructed naively, but the ring
	  ", TT "S", " is constructed with variables of the right degrees
	  so the ring map ", TT "g", " will turn out to be homogeneous."},
     EXAMPLE {
	  "k = ZZ/101; T = k[v..z];",
	  "m = matrix {{x,y,z,x^2*v,x*y*v,y^2*v,z*v,x*w,y^3*w,z*w}}",
	  "n = rank source m",
	  "R = k[u_1 .. u_n]",
	  "S = k[u_1 .. u_n,Degrees => degrees source m]",
	  "f = map(T,R,m)",
	  "g = map(T,S,m)",
	  "res ker f",
	  "res ker g",
	  "isHomogeneous f",
	  "isHomogeneous g"
	  },
     EXAMPLE {
	  "R = ZZ/32003[a..d]/(a^2+b^2+c^2+d^2);",
	  "M = coker vars R",
	  "C = resolution(M, LengthLimit=>6)"
	  },
     PARA {
	  "A manually constructed resolution can be installed as the resolution of
	  a module, bypassing the call to the engine when a resolution is requested,
	  as follows."
	  },
     EXAMPLE {
     	  "A = QQ[x,y]",
	  "C = chainComplex(
	       map(A^1,A^{3:-2},{{x^2,x*y,y^2}}),
	       map(A^{3:-2},A^{2:-3},{{y,0},{ -x,y},{0,-x}}),
	       map(A^{2:-3},0,0))",
	  "M = HH_0 C",
	  "res M = C;",
	  "res M"
	  },
     PARA {
     	  "For an overview of resolutions, in order of increasing detail, see:"
	  },
     UL {
	  SPAN (TO "Hilbert functions and free resolutions"),
	  TO "free resolutions of modules",
	  SPAN (TO "computing resolutions", " -- most detailed")
	  },
     "Some useful related functions:",
     UL {
	  TO (betti,GradedModule),
	  TO (status,Resolution),
	  }
     }

document {
     Key => (resolution, Matrix),
     Headline => "compute the comparison map between resolutions of the source and target of a module map represented by a matrix",
     Usage => "resolution f",
     Inputs => { "f" => {"a module homomorphism ", TT "N <--- M"} },
     Outputs => { {"a chain map from a projective resolution of the source of ", TT "f", " to a resolution of the target of ", TT "f" } },
     EXAMPLE {
	  "R = ZZ[x,y,z]",
	  "N = R^1/(x,y,z)",
	  "M = R^1/(x^2,y^2,x*y*z,z^2)",
	  "f = map(N,M,1)",
	  "res f"
	  },
     SeeAlso => { "free resolutions of modules" }
     }

document { -- This node is used as an example in the documentation nodes: Inputs, Outputs
     Key => (resolution, Ideal),
     Headline => "compute a projective resolution of (the quotient ring corresponding to) an ideal",
     Usage => "resolution I",
     Inputs => {
	  "I" => { "an ideal in a ring ", TT "R", ", say" }
	  },
     Outputs => {
	  {"a resolution of ", TT "R/I", " by projective ", TT "R", "-modules"}
	  },
     EXAMPLE {
	  "R = ZZ[a..d]",
	  "I = ideal(a,b,c,d)",
	  "C = res I",
	  "C_2",
	  "C.dd_2"
	  },
     SeeAlso => { (symbol _, ChainComplex, ZZ), dd, res, ideal }
     }

document {
    Key => {
	(status, ChainComplex),
	(status, Resolution),
	[status, TotalPairs],
	[status, Monomials],
	[status, PairsRemaining]
    },
     Headline => "status of a resolution computation",
     TT "status C", " -- displays the status of the computation of a
     chain complex ", TT "C", " constructed by application of ", TO "resolution", " to
     a module, provided the resolution has been constructed in the engine;
     in particular, the module should be homogeneous and the ultimate coefficient ring of its
     ring should be a field.  The display has
     the same shape as the display produced by ", TO "betti", ", but
     the number(s) displayed in each degree differ.",
     PARA{},
     "Options:",
     UL {
	  {TT "TotalPairs", " -- display the total number of S-pairs, default value ",
	       toString (options status).TotalPairs },
	  {TT "PairsRemaining", " -- display the number of S-pairs remaining, default value ",
	       toString (options status).PairsRemaining},
	  {TT "Monomials", " -- display the number of monomials, default value ",
	       toString (options status).Monomials}
	  },
     EXAMPLE lines ///
          R = QQ[a..d]
	  D = res coker random(R^2,R^{4:-2})
	  status(D, TotalPairs => true, PairsRemaining => true, Monomials => true)
     ///
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
