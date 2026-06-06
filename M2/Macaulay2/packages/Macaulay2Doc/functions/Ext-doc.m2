document {
    Key => Ext,
    Headline => "compute an Ext module",
    SeeAlso => { Hom, tensor, Tor },
    Subnodes => {
	TO (Ext, Module, Module),
	TO (Ext, ZZ, Module, Module),
	TO (Ext, ZZ, Module, Matrix),
	TO (Ext, ZZ, Matrix, Module),
    }
}

document {
    Key => {
	(Ext, Module, Module),
	(Ext, Module, Ideal),
	(Ext, Module, Ring),
	(Ext, Ideal, Module),
	(Ext, Ideal, Ideal),
	(Ext, Ideal, Ring),
	(Ext, Ring, Module),
	(Ext, Ring, Ideal),
	(Ext, Ring, Ring),
    },
     Headline => "total Ext module",
     Usage => "Ext(M,N)",
     Inputs => { "M" => {ofClass{Ideal,Ring}}, "N" => {ofClass{Ideal,Ring}}},
     Outputs => {
	  TEX { "the $Ext$ module of $M$ and $N$,
	       as a multigraded module, with the modules $Ext^i(M,N)$ for all values of $i$ appearing simultaneously." }},
     PARA { "The modules ", TT "M", " and ", TT "N", " should be graded (homogeneous) modules over the same ring." },
     PARA { "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way." },
     PARA TEX {
	  "The computation of the total Ext module is possible for modules over the
	  ring $R$ of a complete intersection, according the algorithm
	  of Shamash-Eisenbud-Avramov-Buchweitz.  The result is provided as a finitely
	  presented module over a new ring with one additional variable of degree
	  ", TT "{-2,-d}", " for each equation of degree ", TT "d", " defining $R$.  The 
	  variables in this new ring have degree length 1 more than the degree length of 
	  the original ring, i.e., is multigraded, with the
	  degree ", TT "d", " part of $Ext^n(M,N)$ appearing as the degree
	  ", TT "prepend(-n,d)", " part of ", TT "Ext(M,N)", ".  We illustrate this in 
	  the following example."
	  },
     EXAMPLE lines ///
     R = QQ[x,y]/(x^3,y^2);
     N = cokernel matrix {{x^2, x*y}}
     H = Ext(N,N);
     ring H
     S = ring H;
     H
     isHomogeneous H
     rank source basis( { -2,-3 }, H)
     rank source basis( { -3 }, Ext^2(N,N) )
     rank source basis( { -4,-5 }, H)
     rank source basis( { -5 }, Ext^4(N,N) )
     hilbertSeries H
     hilbertSeries(H,Order=>11)
     ///,
     PARA{ "The result of the computation is cached for future reference." }
     }

document {
    Key => {
	(Ext, ZZ, Module, Module),
	(Ext, ZZ, Module, Ideal),
	(Ext, ZZ, Module, Ring),
	(Ext, ZZ, Ideal, Module),
	(Ext, ZZ, Ideal, Ideal),
	(Ext, ZZ, Ideal, Ring),
	(Ext, ZZ, Ring, Module),
	(Ext, ZZ, Ring, Ideal),
	(Ext, ZZ, Ring, Ring),
    },
     Usage => "Ext^i(M,N)",
     Headline => "Ext module",
     Inputs => { "i", "M", "N" },
     Outputs => {
	  { "the ", TT "i", "-th ", TT "Ext", " module of ", TT "M", " and ", TT "N" }
	  },
     "If ", TT "M", " or ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
     EXAMPLE lines ///
     	  R = ZZ/32003[a..d];
	  I = monomialCurveIdeal(R,{1,3,4})
	  M = R^1/I
	  Ext^1(M,R)
	  Ext^2(M,R)
	  Ext^3(M,R)
	  Ext^1(I,R)
          ///,
     "As an efficiency consideration, it is generally much more efficient to compute
     Ext^i(R^1/I,N) rather than Ext^(i-1)(I,N).  The latter first computes a presentation 
     of the ideal I, and then a free resolution of that.  For many examples, the
     difference in time and space required can be very large.",
     SeeAlso => {
	 "Complexes :: freeResolution",
	 "OldChainComplexes :: resolution",
	 Tor,
	 Hom,
	 monomialCurveIdeal,
	 (Ext,ZZ,Matrix,Module),
	 (Ext,ZZ,Module,Matrix),
     }
}

document {
    Key => {
	(Ext, ZZ, Matrix, Module),
	(Ext, ZZ, Matrix, Ideal),
	(Ext, ZZ, Matrix, Ring)
    },
     Usage => "Ext^i(f,N)",
     Headline => "map between Ext modules",
     Inputs => { "i", "f" => "M1 --> M2", "N" },
     Outputs => {
	  Matrix => {TEX ///the map $Ext^i(M2,N) \rightarrow{} Ext^i(M1,N)$///}
	  },
     "If ", TT "N", " is an ideal or ring, it is regarded as a module in the evident way.",
     EXAMPLE lines ///
     	  R = ZZ/32003[a..d];
	  I = monomialCurveIdeal(R,{1,3,4})
	  M1 = R^1/I
	  M2 = R^1/ideal(I_0,I_1)
	  f = inducedMap(M1,M2)
	  Ext^1(f,R)
	  g = Ext^2(f,R)
	  source g == Ext^2(M1,R)
	  target g == Ext^2(M2,R)
	  Ext^3(f,R)
          ///,
     SeeAlso => {
	 "Complexes :: freeResolution",
	 "OldChainComplexes :: resolution",
	 Tor,
	 Hom,
	 (Ext,ZZ,Module,Module)
     }
}

document {
    Key => {
	(Ext, ZZ, Module, Matrix),
	(Ext, ZZ, Ideal,  Matrix),
	(Ext, ZZ, Ring,   Matrix),
    },
     Usage => "Ext^i(M,f)",
     Headline => "map between Ext modules",
     Inputs => { "i", "M", "f" => "N1 --> N2"},
     Outputs => {
	  Matrix => {TEX ///the induced map $Ext^i(M,N1) \rightarrow{} Ext^i(M,N2)$///}
	  },
     "If ", TT "M", " is an ideal, it is regarded as a module in the evident way.",
     PARA{},
     -- the code for Hom(Module,Matrix) is wrong, so we disable this example temporarily
     -- EXAMPLE lines ///
     -- 	  R = ZZ/32003[a..d];
     -- 	  I = monomialCurveIdeal(R,{1,3,4})
     -- 	  M = R^1/I
     -- 	  f = map(R^1,module I,gens I)
     -- 	  Ext^1(M,f)
     -- 	  g = Ext^2(M,f)
     -- 	  source g == Ext^2(M,source f)
     -- 	  target g == Ext^2(M,target f)
     -- 	  Ext^3(f,R)
     --      ///,
     SeeAlso => {
	 "Complexes :: freeResolution",
	 "OldChainComplexes :: resolution",
	 Tor,
	 Hom,
	 (Ext,ZZ,Module,Module),
	 (Ext,ZZ,Matrix,Module)
     }
}
