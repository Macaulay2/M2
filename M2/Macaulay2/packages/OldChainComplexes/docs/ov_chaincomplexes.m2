document {
     Key => "free resolutions of modules",
     "The function ", TO "resolution", " (also called ", TT "res", "),
     can be used to produce a free resolution of a module.",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
	  "m = ideal vars R",
	  "M = m/m^3",
      	  "C = resolution M",
	  },
     "The default display for a chain complex shows the modules and
     the number of the stage at which they appear.  See the
     documentation of ", TO "resolution", " for details on
     the options that can be used to control the computation.",
     PARA{},
     "The same function, applied to a map ", TT "f", ", will produce a map
     from a free resolution of the source of ", TT "f", " to a free resolution of
     the target of ", TT "f", ".",
     EXAMPLE {
	  "h = resolution inducedMap(M, m^2/m^4)"
	 },
     Subnodes => {
	 TO "computing resolutions",
	 },
     }

document {
     Key => "computing resolutions",
     "Use the function ", TO "resolution", ", often abbreviated as ", TT "res", ",
     to compute a free resolution of a module.",
     EXAMPLE {
	  "R = QQ[x..z];",
	  "M = cokernel vars R",
	  "C = res M",
	  },
     "See ", TO "chain complexes", " for further details about how to handle
     and examine the result.",
     PARA{},
     "A reference to the result is stored within the module ", TT "M", ", so that
     requesting a computation of ", TT "res M", " a second time yields the formerly
     computed result immediately.",
     PARA{},
     "If the computation is interrupted or discontinued after the skeleton
     has been successfully computed, then the partially completed
     resolution is available as ", TT "M.cache.resolution", ", and can be
     examined with ", TO "status", ".  The computation can be continued
     with ", TT "res M", ".  Here is an example, with an alarm interrupting
     the computation several times before it's complete.  (On my machine,
     the computation takes a total of 14 seconds.)  (Example code, such as
     the code below, is run in such a way that interrupts stop the program,
     so to prevent that, we set ", TO "handleInterrupts", " to ", TO "true", ".)",
     PARA{},
     EXAMPLE {
	  "R = ZZ/2[a..d];",
	  "M = coker random(R^4, R^{5:-3,6:-4});",
	  "handleInterrupts = true",
///(<< "-- computation started: " << endl;
 while true do try (
     alarm 1;
     time res M;
     alarm 0;
     << "-- computation complete" << endl;
     status M.cache.resolution;
     << res M << endl << endl;
     break;
     ) else (
     << "-- computation interrupted" << endl;
     status M.cache.resolution;
     << "-- continuing the computation" << endl;
     ))///
	  },
     "If the user has a chain complex in hand that is known to be a
     projective resolution of ", TT "M", ", then it can be installed
     with ", TT "M.cache.resolution = C", ".",
     PARA{},
     "There are various optional arguments associated with ", TO2(resolution, "res"), "
     which allow detailed control over the progress of the computation."
     }

document {
     Key => "extracting information from chain complexes",
     "Let's make a chain complex.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z];",
      	  "C = res coker matrix {{x,y^2,z^3}};",
	  },
     "Some simple functions for discovering the shape of ", TT "C", ".",
     UL {
	  TO (length, ChainComplex),
	  TO (max, GradedModule),
	  TO (min, GradedModule),
	  },
     EXAMPLE {
	  "length C",
	  "max C",
	  "min C",
	  },
     "In order to see the matrices of the differential maps in a
     chain complex, examine ", TT "C.dd", ".",
     EXAMPLE {
      	  "C.dd",
	  },
     "If ", TT "C", " is a chain complex, then ", TT "C_i", " will produce
     the ", TT "i", "-th module in the complex, ", TT "C^i", " will produce
     the ", TT "-i", "-th module in it, and ", TT "C.dd_i", " will
     produce the differential whose source is ", TT "C_i", ".",
     EXAMPLE {
	  "C_1",
	  "C^-1",
	  "C.dd_2"
	  },
     "The function ", TO "betti", " can be used to display the ranks of the
     free modules in ", TT "C", ", together with the distribution of the basis
     elements by degree, at least for resolutions of homogeneous modules.",
     EXAMPLE {
	  "betti C"
	  },
     "The ranks are displayed in the top row, and below that
     in row ", TT "i", " column ", TT "j", " is displayed the number of
     basis elements of degree ", TT "i+j", " in ", TT "C_j", "."
     }

document {
     Key => "making chain complexes by hand",
     "A new chain complex can be made with ", TT "C = new ChainComplex", ".  This will
     automatically initialize ", TT "C.dd", ", in which the differentials are stored.
     The modules can be installed with statements like ", TT "C#i=M", " and the
     differentials can be installed with statements like ", TT "C.dd#i=d", ".
     The ring is installed with ", TT "C.ring = R", ".  It's up to the
     user to ensure that the composite of consecutive differential maps is zero.",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "d1 = matrix {{x,y}};",
	  },
     "We take care to use ", TO "map", " to ensure that the target of ", TT "d2", " is
     exactly the same as the source of ", TT "d1", ".",
     EXAMPLE {
	  "d2 = map(source d1, ,{{y*z},{-x*z}});",
	  "d1 * d2 == 0",
	  },
     "Now we make the chain complex, as explained above.",
     EXAMPLE {
	  "C = new ChainComplex; C.ring = R;",
	  "C#0 = target d1; C#1 = source d1; C#2 = source d2;",
	  "C.dd#1 = d1; C.dd#2 = d2;",
	  },
     "Our complex is ready to use.",
     EXAMPLE {
	  "C",
	  "HH_0 C",
	  "prune HH_1 C",
	  },
     "The chain complex we've just made is simple, in the sense that it's a homological
     chain complex with nonzero modules in degrees 0, 1, ..., n.  Such a chain
     complex can be made also with ", TO "chainComplex", ".  It goes to a bit
     of extra trouble to adjust the differentials to match the degrees of the
     basis elements.",
     EXAMPLE {
	  "D = chainComplex(matrix{{x,y}}, matrix {{y*z},{-x*z}})",
	  "degrees source D.dd_2",
	  }
     }

document {
     Key => "manipulating chain complexes",
     "There are several natural ways to handle chain complexes; for
     details, see ", TO "ChainComplex", ".  Let's illustrate by
     making two chain complexes.",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "M = coker vars R",
	  "N = coker matrix {{x}}",
	  "C = res M",
	  "D = res N",
	  },
     "We can form the direct sum as follows.",
     EXAMPLE {
	  "C ++ D"
	  },
     "We can shift the degree, using the traditional notation.",
     EXAMPLE {
	  "E = C[5]",
	  "E_-4 == C_1"
	  },
     "The same syntax can be used to make a chain complex from a
     single module.",
     EXAMPLE {
	  "R^4[1]"
	  },
     "We can form various tensor products with ", TO "**", ", and
     compute ", TO "Tor", " using them.",
     EXAMPLE {
	  "M ** D",
	  "C ** D",
	  "prune HH_1(C ** D)",
	  "prune HH_1(M ** D)",
	  "prune HH_1(C ** N)",
	  },
     "Of course, we can use ", TO "Tor", " to get the same result.",
     EXAMPLE {
	  "prune Tor_1(M,N)"
	  },
     }

document {
     Key => "maps between chain complexes",
     "One way to make maps between chain complexes is by lifting maps between
     modules to resolutions of those modules.  First we make some modules.",
     EXAMPLE {
	  "R = QQ[x,y];",
	  "M = coker vars R",
	  "N = coker matrix {{x}}",
	  },
     "Let's construct the natural map from ", TT "N", " to ", TT "M", ".",
     EXAMPLE {
	  "f = inducedMap(M,N)"
	  },
     "Let's lift the map to a map of free resolutions.",
     EXAMPLE {
	  "g = res f"
	  },
     "We can check that it's a map of chain complexes this way.",
     EXAMPLE {
	  "g * (source g).dd == (target g).dd * g"
	  },
     "We can form the mapping cone of ", TT "g", ".",
     EXAMPLE {
	  "F = cone g"
	  },
     "Since ", TT "f", " is surjective, we know that ", TT "F", "
     is quasi-isomorphic to ", TT "(kernel f)[-1]", ".  Let's
     check that.",
     EXAMPLE {
	  "prune HH_0 F",
	  "prune HH_1 F",
	  "prune kernel f",
	  },
     "There are more elementary ways to make maps between chain
     complexes.  The identity map is available from ", TO "id", ".",
     EXAMPLE {
	  "C = res M",
	  "id_C",
	  "x * id_C",
	  },
     "We can use ", TO "inducedMap", " or ", TT "**", " to construct natural maps
     between chain complexes.",
     EXAMPLE {
	  "inducedMap(C ** R^1/x,C)",
	  "g ** R^1/x"
	  },
     "There is a way to make a chain complex map by calling a function
     for each spot that needs a map.",
     EXAMPLE {
	  "q = map(C,C,i -> (i+1) * id_(C_i))"
	  },
     "Of course, the formula we used doesn't yield a map of chain
     complexes.",
     EXAMPLE {
	  "C.dd * q == q * C.dd"
	  }
     }
