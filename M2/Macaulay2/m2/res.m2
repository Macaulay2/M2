--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

Resolution = new Type of MutableHashTable
name Resolution := g -> "<<resolution " | name g.handle | ">>"
document { quote Resolution,
    TT "Resolution", " -- the class of all resolution computations, as well
    as the key used in a ", TO "ChainComplex", " to store the resolution it
    comes from.",
    PARA,
    "These resolutions are internal engine objects not meant to be examined
    by the user.",
    PARA,
    "Functions dealing with resolutions:",
    MENU {
	 TO "status"
	 }
    }

resolution = method(
     Options => {
	  StopBeforeComputation => false,
	  LengthLimit => infinity,	  -- (infinity means numgens R)
	  DegreeLimit => null,		  -- slant degree limit
	  SyzygyLimit => infinity,	  -- number of min syzs found
	  PairLimit => infinity,	  -- number of pairs computed
	  HardDegreeLimit => {},          -- throw out information in degrees above this one
	  -- HardLengthLimit => infinity,    -- throw out information in lengths above this one
	  SortStrategy => 0,		  -- strategy choice for sorting s-pairs
          Algorithm => null		  -- algorithm to use, usually 1, but sometimes 2
	  }
     )

res = resolution

document { quote res,
    "See ", TO "resolution", ", of which ", TT "res", " is a synonym."
    }

document { quote resolution,
     TT "resolution", " -- a command for producing resolutions.",
     PARA,
     "See one of the following entries.",
     MENU {
	  TO (resolution, Module),
	  TO (resolution, Matrix),
	  TO (resolution, Ideal)
	  }
     }

document { resolution => DegreeLimit,
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "resolution", " which specifies that the computation should halt
     after dealing with degree n.",
     PARA,
     "This option is relevant only for homogeneous modules.",
     PARA,
     "One might get some matrix entries of slightly higher degree than requested.",
     EXAMPLE "R = ZZ/101[x,y,z,w]",
     EXAMPLE "M = cokernel matrix {{x*y-z^2,y^2-w^2}}",
     EXAMPLE "res(M,DegreeLimit => 1)",
     EXAMPLE "res(M,DegreeLimit => 2)"
     }

document { resolution => SyzygyLimit,
     TT "SyzygyLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     stop after a certain number of syzygies have computed.",
     PARA,
     EXAMPLE "R = ZZ/101[x,y,z,w]",
     EXAMPLE "M = cokernel matrix {{x*y-z^2,y^2-w^2,w^4}}",
     EXAMPLE "res(M,SyzygyLimit => 1)",
     EXAMPLE "res(M,SyzygyLimit => 2)",
     EXAMPLE "res(M,SyzygyLimit => infinity)"
     }

document { resolution => PairLimit,
     TT "PairLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     be stopped after a certain number of S-pairs have been reduced.",
     EXAMPLE "R = QQ[x,y,z,w]",
     EXAMPLE "M = cokernel matrix {{x*y-z,y^2-w-1,w^4-3}}",
     EXAMPLE "res(M, PairLimit => 1)",
     EXAMPLE "res(M, PairLimit => 10)",
     EXAMPLE "res(M, PairLimit => 20)"
     }

document { resolution => StopBeforeComputation,
     TT "StopBeforeComputation", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     "Tells whether to start the computation, with the default value
     being ", TT "true", ".  This can be useful when you want to obtain
     the partially computed resolution contained in an interrupted computation."
     }

document { quote LengthLimit,
     TT "LengthLimit", " -- a keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     MENU {
	  TO (resolution => LengthLimit),
	  }
     }

document { quote Algorithm,
     TT "Algorithm", " -- a keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     MENU {
	  TO (resolution => Algorithm),
	  }
     }

document { resolution => LengthLimit,
     TT "LengthLimit", " -- keyword for an optional argument used with
     ", TO "resolution", " which indicates how long a resolution to make.",
     PARA,
     "For polynomial rings over a field or over the integers, the length
     is taken to be the dimension of the ring, so the complete resolution will
     be obtained.  For quotient rings of such rings, the same number is used,
     so the complete resolution may not be obtained.",
     PARA,
     "In the current version, asking for a second and longer resolution of the
     same module involves recomputing the resolution from scratch.  Eventually
     the previous work will be used and the recomputation will go quicker.",
     PARA,
     "The resolution returned may actually be one step longer than requested.
     The extra differential is not guaranteed to be minimal."
     }

document { quote HardDegreeLimit,
     TT "HardDegreeLimit", " -- keyword for an optional argument which specifies
     that information above a specified degree is to be discarded.",
     PARA,
     "See:",
     MENU {
	  TO (resolution => HardDegreeLimit)
	  }
     }

document { resolution => HardDegreeLimit,
     TT "HardDegreeLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     "The default value is ", TT "{}", ".",
     PARA,
     "Information above the specified degree is discarded."
     }

document { resolution => Algorithm,
     TT "Algorithm => n", " -- an option for ", TO "resolution", " which specifies
     which algorithm to use.  Algorithms are specified by number and the
     algorithms available are",
     MENU {
	  (TT "Algorithm => 0", " -- Compute syzygies on the Groebner bases of each syzygy
	       module.  The algorithm uses important speedups due to R. Lascala.
	       This algorithm appears to be on the average the fastest."),
	  (TT "Algorithm => 1", " -- An older version of algorithm 0, which doesn't allow as 
	       much experimentation, but can sometimes be marginally faster."),
	  (TT "Algorithm => 2", " -- Compute syzygies on the minimal generators of each 
	       matrix in the resolution.  Over quotient rings, preferred."),
	  (TT "Algorithm => 3", " -- Same as algorithm 2, but compute those Hilbert functions 
	       which allow removal of s-pairs (a la Robbiano, et al.).  
	       Sometimes this improvement can be very dramatic.")
	  },
     "All algorithms use induced monomial orders (Schreyer orders), since 
     this makes an enormous improvement to the efficiency of the algorithm."
     }

document { quote SortStrategy,
     TT "SortStrategy", " -- an keyword for an optional argument which 
     specifies the strategy to be used for sorting S-pairs.",
     PARA,
     MENU {
	  TO (resolution => SortStrategy),
	  }
     }

document { resolution => SortStrategy,
     TT "SortStrategy => n", " -- an option for ", TO "resolution", " which 
     specifies the strategy to be used for sorting S-pairs.",
     PARA,
     "Not implemented yet."
     }

TEST "
R = ZZ/101[x,y]
M = cokernel matrix {{x^2+y^4, y^2 + x*y^3 + 11, 1 + x*y^2}}
C = res M
assert (HH_-1 C == 0)
assert (HH_0 C == M)
assert (HH_1 C == 0)
assert (HH_2 C == 0)
assert (HH_3 C == 0)
assert (HH_4 C == 0)
"

inf := t -> if t === infinity then -1 else t

spots := C -> select(keys C, i -> class i === ZZ)

defaultResolutionLength := (R) -> (
     numgens R + 1 + if ZZ === ultimate(coefficientRing, R) then 1 else 0
     )

resolutionLength := (R,options) -> (
     if options.LengthLimit == infinity then defaultResolutionLength R else options.LengthLimit
     )

resolutionByHomogenization := (M,options) -> (
     R    := ring M;
     f    := presentation M;
     p    := presentation R;
     A    := ring p;
     k    := coefficientRing A;
     n    := numgens A;
     A'   := k[Variables => n+1, MonomialOrder => GRevLex];
     toA' := map(A',A,(vars A')_{0 .. n-1});
     p'   := toA' p;
     R'   := A'/(ideal p');
     toR' := map(R',R,(vars R')_{0 .. n-1});
     f'   := toR' f;
     pH   := homogenize(generators gb p', A'_n);     	  forceGB pH;
     RH   := A' / ideal pH;
     toRH := map(RH, R', vars RH);
     fH   := homogenize(toRH generators gb f',RH_n); 	  forceGB fH;
     MH   := cokernel fH;
     C    := resolution(MH, options, LengthLimit => resolutionLength(R,options));
     toR  := map(R, RH, vars R | 1);
     toR C)

resolutionBySyzygies := (M,options) -> (
     R := ring M;
     maxlength := resolutionLength(R,options);
     if M.?resolution 
     then C := M.resolution
     else (
	  C = new ChainComplex;
	  C.ring = R;
	  f := presentation M;
	  C#0 = target f;
	  C#1 = source f;
	  C.dd#1 = f;
	  M.resolution = C;
	  C.length = 1;
	  );
     i := C.length;
     while i < maxlength and C.dd#i != 0 do (
	  g := syz C.dd#i;
	  shield (
	       i = i+1;
	       C.dd#i = g;
	       C#i = source g;
	       C.length = i;
	       );
	  );
     C)

resolutionInEngine := (M,options) -> (
     local C;
     R := ring M;
     degreelimit := (
	  if class options.DegreeLimit === ZZ then {options.DegreeLimit}
	  else if degreelimit === null then degreelimit = {}
	  else error "expected DegreeLimit to be an integer or null");
     maxlength := resolutionLength(R,options);
     if not M.?resolution 
     or M.resolution.Resolution.length < maxlength
     then M.resolution = (
	  if not isField coefficientRing R
	  then error "expected coefficient ring to be a field";
	  g := presentation M;
	  if options.Algorithm === 0 then
	      g = gens gb g;  -- this is needed since the (current)
			      -- default algorithm, 0, needs a GB 
			      -- to be previously computed.
	  harddegreelimit := (
	       if class options.HardDegreeLimit === ZZ then {options.HardDegreeLimit}
	       else if harddegreelimit === null then harddegreelimit = {}
	       else error "expected HardDegreeLimit to be an integer or null");
	  W := new Resolution;
	  W.ring = R;
	  W.length = maxlength;
	  W.DegreeLimit = degreelimit;
	  W.handle = newHandle(ggPush g, 
	       ggPush options.Algorithm,
	       ggPush maxlength,
	       ggPush harddegreelimit,
	       ggPush options.SortStrategy,
	       ggres);
	  C = new ChainComplex;
	  C.ring = R;
	  shield (C.Resolution = C.dd.Resolution = W);
	  C
	  );
     C = M.resolution;
     if C.?Resolution then (
	  W = C.Resolution;
	  if not W.?returnCode 
	  or W.returnCode =!= 0 
	  or W.length < maxlength
	  or W.DegreeLimit < degreelimit
	  then (
	       scan(keys C,i -> if class i === ZZ then remove(C,i));
	       scan(keys C.dd,i -> if class i === ZZ then remove(C.dd,i));
	       resOptions := {
		    maxlength,
		    inf options.SyzygyLimit,
		    inf options.PairLimit,
		    0, 0, 0};                   -- MES: these are three other options,
						-- to be filled in yet.
	       if not options.StopBeforeComputation then (
		    sendgg(ggPush W, 
			   ggPush degreelimit,
			   ggPush resOptions,
			   ggcalc);
		    W.returnCode = eePopInt();
		    W.length = maxlength;
		    )));
     C)

default := (o,defaults) -> merge(o,defaults,(x,y) -> if x === null then y else x)
Algorithm0 := new OptionTable from { Algorithm => 0 }
Algorithm1 := new OptionTable from { Algorithm => 1 }
Algorithm2 := new OptionTable from { Algorithm => 2 }

resolution Module := (M,o) -> (
     R := ring M;
     k := ultimate(coefficientRing, R);
     oR := options R;
     if oR.?SkewCommutative and oR.SkewCommutative and isHomogeneous M then (
	  resolutionInEngine(M,default(o,Algorithm2)))
     else if k === ZZ or not isCommutative R then resolutionBySyzygies(M,o)
     else if not isHomogeneous M then resolutionByHomogenization(M,o)
     else if isQuotientRing R then resolutionInEngine(M,default(o,Algorithm2))
     else resolutionInEngine(M,default(o,Algorithm1))
     )
document { (resolution, Module),
     TT "resolution M", " -- produces a projective resolution of the 
     module ", TT "M", ".",
     PARA,
     "If the computation is interrupted after the skeleton has been
     successfully computed, then the partially completed
     resolution is available as ", TT "M.resolution", ".  The computation 
     can be continued with ", TT "resolution M", ".",
     PARA,
     "If the user has a chain complex in hand which is known to be a
     projective resolution of ", TT "M", ", then it can be installed
     with ", TT "M.resolution = C", ".",
     PARA,
     "Optional arguments and flags:",
     MENU {
	  (TO (resolution => Algorithm), "             -- which algorithm to use"),
	  (TO (resolution => StopBeforeComputation), " -- whether to stop the computation immediately"),
	  (TO (resolution => DegreeLimit), "           -- compute only up to this degree"),
	  (TO (resolution => HardDegreeLimit), "       -- always compute only up to this degree"),
	  (TO (resolution => SyzygyLimit), "           -- stop when this number of syzygies are obtained"),
	  (TO (resolution => PairLimit), "             -- stop when this number of pairs are handled"),
	  (TO (resolution => LengthLimit), "           -- stop when the resolution reaches this length"),
	  (TO (resolution => SortStrategy), "          -- specify strategy for sorting S-pairs")
	  },
     PARA,
     "For an abbreviation, use ", TO "res", ".",
     SEEALSO {"ChainComplex", "resolution"}
     }

resolution Matrix := (f,o) -> (
     extend(resolution(target f, o), resolution(source f, o), matrix f)
     )
document { (resolution, Matrix),
     TT "resolution f", " -- when ", TT "f", " is a module homomorphism, produces a
     chain map from a resolution of the source of ", TT "f", " to a resolution of the
     target of ", TT "f", ".",
     EXAMPLE "R = ZZ/101[x,y];",
     EXAMPLE "m = ideal vars R",
     EXAMPLE "resolution map(m/m^3, m^2/m^4)"
     }


resolution Ideal := (I,options) -> resolution(
     if I.?quotient 
     then I.quotient
     else I.quotient = (ring I)^1/I,
     options)

document { (resolution, Ideal),
     TT "resolution I", " -- produces a projective resolution of the 
     module ", TT "R/I", " if ", TT "I", " is an ideal in the ring ", TT "R", ".",
     SEEALSO "resolution"
     }

TEST "
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (toList (t_1 .. t_9),3)			  -- 3 by 3
n = matrix pack (toList (u_1 .. u_9),3)			  -- 3 by 3
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 2, DegreeLimit => 1)
-- assert( rank C_2 == 2 )
C = res(M, LengthLimit => 2, DegreeLimit => 2)
-- assert( rank C_2 == 42 )
C = res(M, LengthLimit => 2, DegreeLimit => 3)
-- assert( rank C_2 == 83 )
C = res(M, LengthLimit => 2, DegreeLimit => 4)
--- assert( rank C_2 == 90 )
"

-- MES: these need to be documented.  They also only work currently
-- for correct strategy... (non-Schreyer resolution).  In fact, the
-- only way to obtain that is use Strategy => xxx, some xxx.

mingens(ZZ,Resolution) := (level,g,options) -> (
     sendgg(ggPush g, ggPush level, gggetmingens);
     getMatrix ring g			  -- we're losing information here! MES
     )

generators(ZZ,Resolution) := (level,g) -> (
     sendgg(ggPush g, ggPush level, gggetgb);
     getMatrix ring g)

-- this is undocumented debugging junk of Mike's

getchange(ZZ,Resolution) := (level,g) -> (
     sendgg(ggPush g, ggPush level, gggetchange);
     getMatrix ring g)

leadTerm(ZZ, ZZ, Resolution) := (n,level,g) -> (
     sendgg(ggPush g, ggPush n, ggPush level, gginitial);
     getMatrix ring g)

-----------------------------------------------------------------------------
pairs := g -> (
    sendgg(ggPush g, ggpairs);
    eePopIntarray())
remaining := g -> (
    sendgg(ggPush g, ggremaining);
    eePopIntarray())
nmonoms := g -> (
    sendgg(ggPush g, ggnmonoms);
    eePopIntarray())

ResolutionStatus := (r,options) -> (
     v := {};
     lab := {};
     if options#TotalPairs     === true then (
	  v = append(v,pairs r);
	  lab = append(lab,"total pairs");
	  );
     if options#PairsRemaining === true then (
	  v = append(v,remaining r);
	  lab = append(lab,"pairs remaining");
	  );
     if options#Monomials      === true then (
	  v = append(v,nmonoms r);
	  lab = append(lab,"monomials");
	  );
     numops := # v;
     if numops === 1 
     then lab = concatenate( "        : ", lab)
     else lab = concatenate( "        : (", between(",",lab), ")" );
     if numops === 0 then error "expected at least one option to be true";
     ss := v#0;
     minrow := ss_0;
     maxrow := ss_1;
     mincol := 0;
     maxcol := ss_2;
     leftside := apply(
	  splice {"totals:", apply(minrow .. maxrow, i -> string i | ":")},
	  s -> (9-# s,s));
     v = transpose v;
     v = drop(v,3);
     v = pack(v,maxcol-mincol+1);
     totals := apply(transpose v, sum);
     v = prepend(totals,v);
     v = transpose v;
     v = applyTable(v, toSequence);
     if numops === 1
     then v = applyTable(v,(i) -> if i === 0 then "." else name i)
     else v = applyTable(v,args -> concatenate("(", between(",",apply(args,name)), ")" ));
     just := (
	  if numops === 1
	  then (wid,s) -> (wid - # s, s)  -- right justify
	  else (wid,s) -> (			  -- center
	       n := # s;
	       w := (wid - n + 1)//2; 
	       (w, s, wid-w-n)));
     v = apply(v, col -> apply(col, s -> just(1 + max apply(col, i -> #i), s)));
     v = prepend(leftside,v);
     v = transpose v;
     v = apply(v, row -> (row,"\n"));
     << lab << endl;
     printString(stdout,v);
     )
statusDefaults := new OptionTable from {
     TotalPairs => true,
     PairsRemaining => false,
     Monomials => false
     }
status = method (Options => statusDefaults)
status ChainComplex := (C,options) -> ResolutionStatus(C.Resolution, options)
document { quote status,
     TT "status C", " -- displays the status of the computation of a
     chain complex C constructed by ", TO "resolution", ".  The display has
     the same shape as the display produced by ", TO "betti", ", but
     the number(s) displayed in each degree differ.",
     PARA,
     "Options:",
     MENU {
	  {TO TotalPairs, " -- display the total number of S-pairs, default value ",
	       statusDefaults.TotalPairs },
	  {TO PairsRemaining, " -- display the number of S-pairs remaining, default value ",
	       statusDefaults.PairsRemaining},
	  {TO Monomials, " -- display the number of monomials, default value ",
	       statusDefaults.Monomials}
	  }
     }
document { quote TotalPairs,
     TT "TotalPairs", " -- an option for ", TO "status", " which specifies
     whether to display the total number of S-pairs."
     }
document { quote PairsRemaining,
     TT "PairsRemaining", " -- an option for ", TO "status", " which specifies
     whether to display number of S-pairs remaining."
     }
document { quote Monomials,
     TT "Monomials", " -- an option for ", TO "status", " which specifies
     whether to display the number of monomials."
     }
status Resolution := (r,options) -> ResolutionStatus(r, options)
