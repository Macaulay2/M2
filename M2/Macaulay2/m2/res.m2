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

document { quote HardLengthLimit,
     TT "HardLengthLimit", " -- a keyword for an optional argument used
     with ", TO "resolution", " which indicates that those parts of the
     computation in higher degree than this should be discarded (i.e.
     an implied LengthLimit=>d will be appended to this resolution 
     computation, when HardLengthLimit=>d is given to the resolution
     routine.  This should only be used in extreme cases when
     the skeleton of the resolution is very large in higher degrees,
     and you do not need that part of the resolution in these higher
     (slanted) degrees."
     }

document { quote LengthLimit,
     TT "LengthLimit", " -- keyword for an optional argument used with
     ", TO "resolution", " which indicates how long a resolution to make.",
     PARA,
     "In the current version, asking for a second and longer resolution of the
     same module involves recomputing the resolution from scratch.  Eventually
     the previous work will be used and the recomputation will go quicker.",
     PARA,
     "The resolution returned may actually be one step longer than requested.
     The extra differential is not guaranteed to be minimal."
     }
resolution = method(
     Options => {
	  Compute => true,
	  LengthLimit => infinity,	  -- (infinity means numgens R)
	  DegreeLimit => {},		  -- slant degree limit
	  SyzygyLimit => infinity,	  -- number of min syzs found
	  PairLimit => infinity,	  -- number of pairs computed
	  HardDegreeLimit => {},          -- throw out information in degrees above this one
	  -- HardLengthLimit => infinity,    -- throw out information in lengths above this one
	  SortStrategy => 0,		  -- strategy choice for sorting s-pairs
          Algorithm => 1		  -- algorithm 0 doesn't work yet
	  }
     )


res = resolution
document { quote res,
    "See ", TO "resolution", "."
    }
document { quote resolution,
     TT "resolution M", " -- produces a projective resolution of the 
     module (or ideal) M.",
     PARA,
     "If the computation is interrupted after the skeleton has been
     successfully computed, then the partially completed
     resolution is available as ", TT "M.resolution", ".  The computation can
     be continued with ", TT "resolution M", ".",
     PARA,
     "If the user has a chain complex in hand which is known to be a
     projective resolution of ", TT "M", ", then it can be installed
     with ", TT "M.resolution = C", ".",
     PARA,
     "Optional arguments and flags:",
     MENU {
	  (TO "Algorithm", "        -- which algorithm to use"),
	  (TO "Compute", "          -- whether to start the computation"),
	  (TO "DegreeLimit", "      -- compute only up to this degree"),
	  (TO "SyzygyLimit", "      -- stop when this number of syzygies are obtained"),
	  (TO "PairLimit", "        -- stop when this number of pairs are handled"),
	  (TO "NextExponentSize", " -- the number of bits to use for each
	       exponent the next time expansion occurs"),
	  (TO "LengthLimit", "      -- stop when the resolution reaches this length"),
	  (TO "SortStrategy", "     -- specify strategy for sorting S-pairs")
	  },
     PARA,
     "For an abbreviation, use ", TO "res", ".",
     SEEALSO "ChainComplex"
     }

document { quote Algorithm,
     TT "Algorithm => n", " -- an option for ", TO "resolution", " which specifies
     which algorithm to use.  Algorithms are specified by number and the
     algorithms available are",
     MENU {
	  ("0", " -- The default algorithm, which is to compute syzygies on the Groebner bases 
	       of each syzygy module.  The algorithm uses important speedups due to
	       R. Lascala.  This algorithm appears to be on the average the fastest."),
	  ("1", " -- An older version of algorithm 0, which doesn't allow as much
	       experimentation, but can sometimes be marginally faster."),
	  ("2", " -- Compute syzygies on the minimal generators of each matrix in the
	       resolution."),
	  ("3", " -- Same as algorithm 2, but compute those Hilbert functions which allow
	       removal of s-pairs (a la Robbiano et al.  Sometimes this improvement can 
	       be very dramatic.")
	  },
     "All algorithms use induced monomial orders (Schreyer orders), since this makes an
     enormous improvement to the efficiency of the algorithm."
     }

document { quote SortStrategy,
     TT "SortStrategy => n", " -- an option for ", TO "resolution", " which specifies
     the strategy to be used for sorting S-pairs.",
     PARA,
     "Not implemented yet."
     }

inf := t -> if t === infinity then -1 else t

spots := C -> select(keys C, i -> class i === ZZ)

nres := (M,options) -> (
     -- inhomogeneous case here
     -- homogenize with respect to a new variable and then come back by setting
     -- it to 1
     R := ring M;
     f := presentation M;
     p := presentation R;
     A := ring p;
     k := coefficientRing A;
     n := numgens A;
     x := local x;
     B := k[x_0 .. x_n];
     forward := map(B,A,matrix{{x_1 .. x_n}});
     q := forward p;
     S := B / ideal q;
     forward = map(S,R,matrix{{x_1 .. x_n}} ** S);
     backward := map(R,S, 1 | vars R);
     f = homogenize(forward f,x_0);
     C := res (cokernel f, options);
     complete C;
     complete C.dd;
     D := new ChainComplex;
     D.ring = R;
     scan(spots C, i -> D#i = backward C#i);
     scan(spots C.dd, i -> D.dd#i = map(D_(i-1),D_i,backward C.dd#i));
     D)

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

resolution Module := (M,options) -> (
     if not isHomogeneous M then nres(M,options)
     else (
	  R := ring M;
	  degreelimit := options.DegreeLimit ;
	  if class degreelimit === ZZ then degreelimit = {degreelimit}
	  else if class degreelimit === List and all(degreelimit,i -> class i === ZZ)
	  then ()
	  else error "expected DegreeLimit to be an integer or list of integers";
	  harddegreelimit := options.HardDegreeLimit ;
	  if class harddegreelimit === ZZ then harddegreelimit = {harddegreelimit}
	  else if class harddegreelimit === List and all(harddegreelimit,i -> class i === ZZ)
	  then ()
	  else error "expected HardDegreeLimit to be an integer or list of integers";
	  maxlength := (
	       if options.LengthLimit === infinity 
	       then numgens R + 1
	       else options.LengthLimit
	       );
	  if not M.?resolution
     	  ------------------------
	  ---- to be removed later!
	  or M.resolution.Resolution.length < maxlength
     	  ------------------------
	  then M.resolution = (
	       if not isField coefficientRing R
	       then error "expected coefficient ring to be a field";
	       if (monoid R).Options.SkewCommutative then (
		    C := new ChainComplex;
		    C.ring = R;
		    f := presentation M;
		    i := 0;
		    C#i = target f;
		    while (
			 i = i+1;
			 C#i = source f;
			 C.dd#i = f;
			 i < maxlength and f != 0
			 ) do (
			 f = syz f;
			 );
		    C)
	       else (
		    g := presentation M;
		    if options.Algorithm === 0 then
		        g = gens gb g;  -- this is needed since the (current)
			                -- default algorithm, 0, needs a GB 
					-- to be previously computed.
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
		    C)
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
		    if options.Compute then (
			 sendgg(ggPush W, 
				ggPush degreelimit,
				ggPush resOptions,
				ggcalc);
			 W.returnCode = eePopInt();
			 W.length = maxlength;
			 )));
	  C)
     )

resolution Ideal := (I,options) -> (
     R := ring I;
     resolution(R^1/I, options))

TEST "
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (elements (t_1 .. t_9),3)			  -- 3 by 3
n = matrix pack (elements (u_1 .. u_9),3)			  -- 3 by 3
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
