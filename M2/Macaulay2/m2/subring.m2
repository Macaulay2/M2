-- Copyright 1996 Michael E. Stillman

-- SubringComputation = new SelfInitializingType of BasicList
PushforwardComputation = new SelfInitializingType of BasicList

-- valid values are (a) {J,cleanup code}   -- J is the aux matrix and the code to derive
-- 						the answer is the 2nd element.
--               or (b) {answer}           -- answer is a matrix

--subringOptions := mergeOptions(options gb, Strategy => , UseHilbertFunction => true

pushForward1 = method(
     Options => {
	  Strategy => NonLinear,            -- use the best choice
	  UseHilbertFunction => true,  -- if possible
	  MonomialOrder => EliminationOrder,
	  StopWithMinimalGenerators => false,            -- determine the minimal generators of the subring
	  BasisElementLimit => infinity,  -- number of generators of GB in the subring

	  StopBeforeComputation => false,
	  DegreeLimit => {},
	  PairLimit => infinity
	  }
     )

pushtest := (f,M,options) -> (
    comp := PushforwardComputation{M,NonLinear};
    if not f#?comp then (
	-- create the computation
	S := source f;
	n1 := numgens target f;
        order := if options.MonomialOrder === EliminationOrder then 
                     Eliminate n1
                 else if options.MonomialOrder === ProductOrder then 
		     ProductOrder{n1, numgens S}
		 else
		     Lex;
	JJ := gens graphIdeal(f,MonomialOrder => order, VariableBaseName => local X);
	m := presentation M;
	-- now map M to the new ring.
	xvars := map(ring JJ, ring M, submatrix(vars ring JJ, toList(0..n1-1)));
	m1 := xvars m;
	m1 = presentation ((cokernel m1) ** (cokernel JJ));
	mapback := map(S, ring JJ, map(S^1, S^n1, 0) | vars S);

	if options.UseHilbertFunction 
           and isHomogeneous m1 and isHomogeneous f and isHomogeneous m then (
	    hf := poincare cokernel m;
	    T := (ring hf)_0;
	    hf = hf * product(numgens source JJ, i -> (d := (degrees source JJ)#i#0; 1 - T^d));
	    (cokernel m1).poincare = hf;
	    );

	cleanupcode := g -> mapback selectInSubring(1,generators g);

	f#comp = {m1, cleanupcode};
	);

    if #( f#comp ) === 1 then
	f#comp#0
    else (
	gboptions := new OptionTable from {
			StopBeforeComputation => options.StopBeforeComputation,
			DegreeLimit => options.DegreeLimit,
			PairLimit => options.PairLimit};
	m1 = f#comp#0;
	g := gb(m1,gboptions);
	result := f#comp#1 g;
	--if isdone g then f#comp = {result};  -- MES: There is NO way to check this yet!!

	-- MES: check if the monomial order restricts to S.  If so, then do
        -- forceGB result;
	result
	)
    )

pushlinear := (f,M,options) -> (
    -- assumptions here:
    -- (a) f is homogeneous linear, and the linear forms are independent
    -- 
    -- First bring M over to a ring with an elimination order, which eliminates
    -- the variables 'not in' f.
    m := presentation M;    
    R := target f;
    S := source f;
    Rbase := ultimate(ambient, R);
    fmat := substitute(f.matrix,Rbase);
    n := numgens source f.matrix;
    n1 := numgens R - n;
    R1 := (ring Rbase)[Variables => numgens R, MonomialOrder => Eliminate n1];
    fg := newCoordinateSystem(R1, fmat);
    Fto := fg#0;  -- we don't need to go back, so we ignore fg#1
    m1 := Fto substitute(m,Rbase);
    m1 = presentation (cokernel m1 ** cokernel Fto presentation R);
    if isHomogeneous f and isHomogeneous m then (
        hf := poincare cokernel m;
        T := (ring hf)_0;
        (cokernel m1).poincare = hf;
        );
    g := selectInSubring(1, generators gb(m1,options));
    -- now map the answer back to S = source f.
    mapback := map(S, R1, map(S^1, S^n1, 0) | submatrix(vars S, {0..n-1}));
    mapback g
    )

document { quote NonLinear,
     TT "Strategy => NonLinear", " -- an option value for the ", TO "Strategy", "
     option to ", TO "pushForward1", "."
     }

pushForward1(RingMap, Module) := (f,M,options) -> (
    if options.Strategy === Linear then
        pushlinear(f,M,options)
    else if options.Strategy === NonLinear then
        pushtest(f,M,options)
    else error "unrecognized Strategy"
    )

document { pushForward1 => StopBeforeComputation,
     TT "StopBeforeComputation", " -- keyword for an optional argument used with
     ", TO "pushForward1", ".",
     PARA,
     "Tells whether to start the computation, with the default value
     being ", TT "true", "."
     }

document { pushForward1 => DegreeLimit,
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "pushForward1", " which specifies that the computation should halt after dealing 
     with degree ", TT "n", ".",
     PARA,
     "This option is relevant only for homogeneous matrices.",
     PARA,
     "The maximum degree to which to compute is computed in terms of the
     degrees of the ring map, ", TT "f", ".  For example, if ", TT "f", "
     consists of cubics, then to find a quadratic relation, this option
     should be set to at least 6, by specifying, for example, ", TT
     "DegreeLimit => 6", ".  The default is ", TT "infinity", ".",
     SEEALSO {"pushForward1", "DegreeLimit"}
     }

document { quote pushForward1,
     TT "pushForward1(f,M,options)", " -- Given a ring map f : R --> S, and an S-module
     M, yields a presentation matrix of the R-submodule of M generated by the given
     (S-module) generators of M.",
     PARA,
     "Warning: this function will be removed, and its function incorporated into
     that of ", TO "image", " and ", TO "prune", ".",
     PARA,
     "This is a very basic operation, and is used by several other functions.  See,
     for example, ", TO "pushForward", ".  Therefore we intend to eliminate it,
     and merge its function into ", TO "image", " after introducing
     generalized module homomorphisms which map an R-module to an S-module.",
     PARA,
     "As an example, the following fragment computes the ideal of the
     rational normal curve. This could also be done using ", TO "monomialCurve", ".",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "S = ZZ/101[s,t];",
      	  "f = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})",
      	  "pushForward1(f,S^1)",
	  },
     PARA,
     "Options:",
     MENU {
	  TO (pushForward1 => Strategy),
	  TO (pushForward1 => UseHilbertFunction),
	  TO (pushForward1 => MonomialOrder),
	  TO (pushForward1 => DegreeLimit),
	  TO (pushForward1 => PairLimit),
	  },
     PARA,
     "The following code performs the Groebner computation using a product order 
     rather than the default elimination order.",
     EXAMPLE "pushForward1(f,S^1,MonomialOrder=>ProductOrder)",
     PARA,
     "The computation is stashed inside the ring map, until the computation has
     finished completely.  This means that you may interrupt this command, and 
     later restart it. You may alo obtain partial results, as follows.",
     EXAMPLE {
	  "f = map(S,R,matrix{{s^4, s^3*t, s*t^3, t^4}})",
      	  "pushForward1(f,S^1,DegreeLimit=>4)",
	  },
     "After interrupting a computation (using control-C), you may view the
     equations so far obtained by using the ", TT "PairLimit", " option to prevent any
     further work from being done.",
     EXAMPLE "pushForward1(f,S^1,PairLimit=>0)",
     "The type ", TO "PushforwardComputation", " is used internally by our current implementation.",
     }

document { pushForward1 => StopWithMinimalGenerators,
     TT "StopWithMinimalGenerators => true", " -- an option for ", TO "pushForward1", "
     that specifies that the computation should stop as soon as a
     complete list of minimal generators for the submodule or ideal has been
     determined.",
     PARA,
     "The value provided is simply passed on to ", TO "gb", ": see 
     ", TO (gb => StopWithMinimalGenerators), " for details."
     }

document { pushForward1 => PairLimit,
     TT "PairLimit => n", " -- keyword for an optional argument used with
     ", TO "pushForward1", ", which specifies that the computation should
     be stopped after a certain number of S-pairs have been reduced."
     }

document { pushForward1 => MonomialOrder,
     TT "MonomialOrder => x", " -- a keyword for an optional argument to ", TO "pushForward1", "
     which tells which monomial order to use for the Groebner basis computation
     involved.",
     PARA,
     "Possible values:",
     MENU {
	  (TT "MonomialOrder => EliminationOrder", " -- use the natural elimination order (the default)"),
	  (TT "MonomialOrder => ProductOrder", " -- use the product order"),
	  (TT "MonomialOrder => LexOrder", " -- use lexical order"),
	  },
     SEEALSO "EliminationOrder"
     }

document { pushForward1 => UseHilbertFunction,
     TT "UseHilbertFunction => true", " -- a keyword for an optional argument to
     ", TO "pushForward1", " which specifies whether to use the Hilbert function,
     if one has previously been computed.",
     PARA,
     "The default is to use it if possible."
     }

document { pushForward1 => Strategy,
     TT "pushForward1(f,M,Strategy => v)", " -- an option for ", TO pushForward1, " 
     which can be used to specify the strategy to be used in the computation.",
     PARA,
     "The strategy option value ", TT "v", " should be one of the following.",
     MENU {
	  TO "NonLinear",
     	  TO "Linear"
	  },
     PARA,
     "The default is for the code to select the best strategy heuristically."
     }

document { quote PushforwardComputation,
     TT "PushforwardComputation", " -- a type of list used internally by
     ", TO "pushForward1", "."
     }

document { quote EliminationOrder,
     TT "EliminationOrder", " -- a value for the ", TO "MonomialOrder", "
     option to ", TO "pushForward1", " which specifies the natural elimination
     order be used."
     }

pushForward = method (Options => options pushForward1)

pushForward(RingMap, Module) := (f,M,options) -> (
     if isHomogeneous f and isHomogeneous M then (
	  -- given f:R-->S, and M an S-module, finite over R, find R-presentation for M
	  S := target f;
	  M = cokernel presentation M;
	  M1 := M ** (S^1/(image f.matrix));
	  if dim M1 > 0 then error "module given is not finite over base";
	  M2 := subquotient(matrix (basis M1 ** S), relations M);
	  cokernel pushForward1(f,M2,options)
	  )
     else error "not implemented yet for inhomogeneous modules or maps"
     )

document { quote pushForward,
     TT "pushForward", "(f,M) -- yields an R-presentation of the S-module M, where
     f:R --> S is a ring map, and M is considered as an R-module via f.",
     PARA,
     "If M is not finitely generated over R, then an error is raised.",
     PARA,
     "Currently, R and S must both be polynomial rings over the same base field."
     }

document { pushForward => StopBeforeComputation,
     TT "StopBeforeComputation => false", " -- an optional argument used with
     ", TO "pushForward", ".",
     PARA,
     "Tells whether to start the computation, with the default value
     being ", TT "true", "."
     }

document { pushForward => StopWithMinimalGenerators,
     TT "StopWithMinimalGenerators => true", " -- an option for ", TO "pushForward", "
     that specifies that the computation should stop as soon as a
     complete list of minimal generators for the submodule or ideal has been
     determined.",
     PARA,
     "The value provided is simply passed on to ", TO "gb", ": see 
     ", TO (gb => StopWithMinimalGenerators), " for details."
     }

document { pushForward => Strategy,
     TT "pushForward(f,M,Strategy => v)", " -- an option for ", TO pushForward, " 
     which can be used to specify the strategy to be used in the computation.",
     PARA,
     "The strategy option value ", TT "v", " should be one of the following.",
     MENU {
	  TO "NonLinear",
     	  TO "Linear"
	  }
     }

TEST "
    R = ZZ/101[a,b]
    S = ZZ/101[a,b,c]
    M = cokernel matrix{{c^3}}
    f = map(S,R)
    assert( R^3 == pushForward(f,M) )
"

document { quote UseHilbertFunction,
     TT "UseHilbertFunction", " -- an option for ", TO "pushForward1", "."
     }
