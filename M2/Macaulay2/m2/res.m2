--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

inf := t -> if t === infinity then -1 else t

spots := C -> select(keys C, i -> class i === ZZ)

defaultResolutionLength := (R) -> (
     numgens R + 1 + if ZZ === ultimate(coefficientRing, R) then 1 else 0
     )

resolutionLength := (R,opts) -> (
     if opts.LengthLimit == infinity then defaultResolutionLength R else opts.LengthLimit
     )

resolutionByHomogenization := opts -> (M) -> (
     if gbTrace >= 1 then << "using resolution by homogenization" << endl;
     if opts.FastNonminimal then error "cannot specify FastNonminimal=>true with this input";
     R    := ring M;
     f    := presentation M;
     p    := presentation R;
     A    := ring p;
     k    := coefficientRing A;
     n    := numgens A;
     X    := local X;
     N    := monoid [X_0 .. X_n, MonomialOrder => GRevLex];
     A'   := k N;
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
     assert isHomogeneous MH;
     C    := resolution(MH, opts, LengthLimit => resolutionLength(R,opts));
     toR  := map(R, RH, vars R | 1);
     toR C)

resolutionBySyzygies := opts -> (M) -> (
     if gbTrace >= 1 then << "using resolution by syzygyies" << endl;     
     if opts.FastNonminimal then error "cannot specify FastNonminimal=>true with this input";
     R := ring M;
     maxlength := resolutionLength(R,opts);
     if M.cache.?resolution 
     then C := M.cache.resolution
     else (
	  C = new ChainComplex;
	  C.ring = R;
	  f := presentation M;
	  C#0 = target f;
	  C#1 = source f;
	  C.dd#1 = f;
	  M.cache.resolution = C;
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

resolutionInEngine := opts -> (M) -> (
     local C;
     R := ring M;
     degreelimit := (
	  if class opts.DegreeLimit === ZZ then {opts.DegreeLimit}
	  else if degreelimit === null then degreelimit = {}
	  else error "expected DegreeLimit to be an integer or null");
     maxlevel := resolutionLength(R,opts);
     if not M.cache.?resolution 
     or M.cache.resolution.Resolution.length < maxlevel
     or (M.cache.resolution.Resolution.Strategy === 4 and opts.Strategy =!= 4)
     or (M.cache.resolution.Resolution.Strategy =!= 4 and opts.Strategy === 4)
     then M.cache.resolution = (
          if flagInhomogeneity then (
	       if not isHomogeneous M then error "internal error: res: inhomogeneous matrix flagged";
	       if debugLevel > 0 then stderr << "--res: matrix is homogeneous, good" << endl;
	       );
	  g := presentation M;
	  if not instance(opts.Strategy, ZZ) then error "resolution in engine: expected Strategy option to be an integer";
	  if opts.Strategy === 0 or opts.Strategy === 4 then
	      g = generators gb g;  -- this is needed since the (current)
			      -- default algorithm, 0, needs a GB 
			      -- to be previously computed.
                              -- The non-minimal resolution algorithm 4 also needs this.
	  harddegreelimit := (
	       if class opts.HardDegreeLimit === ZZ then {opts.HardDegreeLimit}
	       else if harddegreelimit === null then harddegreelimit = {}
	       else error "expected HardDegreeLimit to be an integer or null");
	  W := new Resolution;
	  W.ring = R;
	  W.length = maxlevel;
	  W.DegreeLimit = degreelimit;
          W.Strategy = opts.Strategy;
	  log := FunctionApplication { rawResolution, (
		    raw g,					    -- the matrix
		    true,					    -- whether to resolve the cokernel of the matrix
		    maxlevel,				    -- how long a resolution to make, (hard : cannot be increased by stop conditions below)
		    false,					    -- useMaxSlantedDegree
		    0,					    -- maxSlantedDegree (is this the same as harddegreelimit?)
		    opts.Strategy,				    -- algorithm
		    opts.SortStrategy			    -- strategy (is this the same as opts.SortStrategy?)
		    )};
	  W#"RawComputation log" = Bag {log};
     	  W.RawComputation = value log;
	  W.returnCode = rawStatus1 W.RawComputation;
	  C = new ChainComplex;
	  C.ring = R;
	  shield (C.Resolution = C.dd.Resolution = W);
	  C
	  );
     C = M.cache.resolution;
     if C.?Resolution then (
	  W = C.Resolution;
	  if not W.?returnCode 
	  or RawStatusCodes#(W.returnCode) =!= "done"
	  or W.length < maxlevel
	  or W.DegreeLimit < degreelimit
	  then (
	       -- clear info in C because W may change as we continue the computation:
	       scan(keys C,i -> if class i === ZZ then remove(C,i));
	       scan(keys C.dd,i -> if class i === ZZ then remove(C.dd,i));
	       remove(C,symbol complete);
	       if not opts.StopBeforeComputation then (
		    log = FunctionApplication { rawGBSetStop,
			 (
			      W.RawComputation,
			      -- fill these in eventually:
			      opts.StopBeforeComputation,	    -- always_stop
			      degreeToHeft(R,degreelimit),          -- degree_limit
			      0,				    -- basis_element_limit (not relevant for resolutions)
			      inf opts.SyzygyLimit,		    -- syzygy_limit
			      inf opts.PairLimit,		    -- pair_limit
			      0,				    -- codim_limit (not relevant for resolutions)
			      0,				    -- subring_limit (not relevant for resolutions)
			      false,				    -- just_min_gens
			      -- {maxlevel}			    -- length_limit -- error if present is: "cannot change length of resolution using this algorithm"
			      {} 				    -- length_limit
			      )};
		    W#"rawGBSetStop log" = Bag {log};
		    value log;
		    rawStartComputation W.RawComputation;
		    W.returnCode = rawStatus1 W.RawComputation;
		    W.length = maxlevel;
		    W.DegreeLimit = degreelimit;
		    )));
     C)

default := (o,defaults) -> merge(o,defaults,(x,y) -> if x === null then y else x)
Strategy0 := new OptionTable from { Strategy => 0 }
Strategy1 := new OptionTable from { Strategy => 1 }
Strategy2 := new OptionTable from { Strategy => 2 }
Strategy3 := new OptionTable from { Strategy => 3 }
Strategy4 := new OptionTable from { Strategy => 4 }

resolution = method(
     Options => {
	  StopBeforeComputation => false,
	  LengthLimit => infinity,	  -- (infinity means numgens R)
	  DegreeLimit => null,		  -- slant degree limit
	  SyzygyLimit => infinity,	  -- number of min syzs found
	  PairLimit => infinity,	  -- number of pairs computed
	  HardDegreeLimit => {},          -- throw out information in degrees above this one
	  -- HardLengthLimit => infinity,    -- throw out information in lengths above this one
	  SortStrategy => 0,		  -- strategy choice for sorting S-pairs
          Strategy => null,		  -- algorithm to use, usually 1, but sometimes 2
          FastNonminimal => false
	  }
     )

engineReady := M -> (
     R := ring M;
     -- Needed to compute resolutions, (algorithms 0,1,2,3):
     --    Ring is (tower of) poly ring(s) over a field (or skew commutative, or quotient ring of such, or both)
     --    Ring is graded
     --    Ring is homogeneous in this grading
     --    Matrix is homogeneous in this grading
     -- Additional requirements for resolution algorithm 3 (which uses hilbert function):
     --    Ring is singly graded
     R.?Engine 
     and isHomogeneous M
     and (isCommutative R or isSkewCommutative R)
     and (
     	  k := ultimate(coefficientRing, R);
	  k =!= R
     	  and isField k
	  )
     )

protect ManualResolution				    -- not to be exported
storefuns#resolution = (M,C) -> M.cache.ManualResolution = C

resolution Module := ChainComplex => o -> (M) -> (
     if M.cache.?ManualResolution then return M.cache.ManualResolution;
     C := runHooks(Module,symbol resolution,(o,M));
     if C =!= null then return C;
     R := ring M;
     if isField R then return chainComplex map(minimalPresentation M,R^0,0);
     k := ultimate(coefficientRing, R);
     oR := options R;
     if engineReady M and (options R).Heft =!= null
     then (resolutionInEngine default(o,if o.FastNonminimal then Strategy4 else if isQuotientRing R or isSkewCommutative R then Strategy2 else Strategy1))(M)
     else if k === ZZ then (resolutionBySyzygies o)(M)
     else if not isHomogeneous M and isCommutative R and degreeLength R === 1 then (resolutionByHomogenization o)(M)
     else (resolutionBySyzygies o)(M)
     )

resolution Matrix := ChainComplexMap => options -> (f) -> extend(
     resolution(target f, options), 
     resolution(source f, options), 
     matrix f)

resolution Ideal := ChainComplex => options -> (I) -> resolution(
     if I.cache.?quotient 
     then I.cache.quotient
     else I.cache.quotient = (ring I)^1/I,
     options)

-----------------------------------------------------------------------------
getpairs := g -> rawGBBetti(raw g,1)
remaining := g -> rawGBBetti(raw g,2)
nmonoms := g -> rawGBBetti(raw g,3)

status Resolution := options -> (r) -> (
     r = raw r;
     b := new BettiTally;
     lab := ();
     f := (label,type) -> (
	  b = merge( applyValues(b, x->append(x,0)), applyValues(rawBetti(r,type), x->splice{#lab:0,x}), plus);
	  lab = append(lab,label);
	  );
     if options#TotalPairs     === true then f("total pairs",1);
     if options#PairsRemaining === true then f("pairs remaining",2);
     if options#Monomials      === true then f("monomials",3);
     numops := # lab;
     if numops === 0 then error "expected at least one option to be true";
     b = applyKeys( b, (i,d,h) -> (h - i, i)); -- skew the degrees in the usual way; this way the Koszul complex occupies a horizontal line instead of a diagonal line
     k := keys b;
     fi := first \ k;
     la := last  \ k;
     mincol := min la;
     mincol = min(0,mincol);
     maxcol := max la;
     minrow := min fi;
     maxrow := max fi;
     zer := toList (numops : 0);
     b = table(toList (minrow .. maxrow), toList (mincol .. maxcol), (i,j) -> if b#?(i,j) then b#(i,j) else zer);
     leftside := apply( splice {"total:", apply(minrow .. maxrow, i -> toString i | ":")}, s -> (6-# s,s));
     totals := apply(transpose b, sum);
     b = transpose prepend(totals,b);
     b = applyTable(b, unsequence @@ toSequence);
     zer = unsequence toSequence zer;
     b = applyTable(b, bt -> if bt === zer then "." else toString bt);
     b = apply(b, col -> ( 
	       wid := 1 + max apply(col, i -> #i); 
	       apply(col, 
		    if numops == 1
		    then s -> (wid-#s, s)		    -- right justify
		    else s -> ( n := # s; w := (wid - n + 1)//2; (w, s, wid-w-n)) -- center
		    )
	       ));
     b = transpose prepend(leftside,b);
     toString unsequence lab || "" || stack apply(b, concatenate))

status ChainComplex := options -> (C) -> (
     if not C.?Resolution then error "status: expected a resolution constructed in the engine";
     status(C.Resolution, options))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
