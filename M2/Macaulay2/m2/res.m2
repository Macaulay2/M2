--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

inf := t -> if t === infinity then -1 else t

spots := C -> select(keys C, i -> class i === ZZ)

defaultResolutionLength := (R) -> (
     numgens R + 1 + if ZZ === ultimate(coefficientRing, R) then 1 else 0
     )

resolutionLength := (R,options) -> (
     if options.LengthLimit == infinity then defaultResolutionLength R else options.LengthLimit
     )

resolutionByHomogenization := options -> (M) -> (
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
     -- assert(isHomogeneous MH);
     C    := resolution(MH, options, LengthLimit => resolutionLength(R,options));
     toR  := map(R, RH, vars R | 1);
     toR C)

resolutionBySyzygies := options -> (M) -> (
     R := ring M;
     maxlength := resolutionLength(R,options);
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

resolutionInEngine := options -> (M) -> (
     local C;
     R := ring M;
     degreelimit := (
	  if class options.DegreeLimit === ZZ then {options.DegreeLimit}
	  else if degreelimit === null then degreelimit = {}
	  else error "expected DegreeLimit to be an integer or null");
     maxlevel := resolutionLength(R,options);
     if not M.cache.?resolution 
     or M.cache.resolution.Resolution.length < maxlevel
     then M.cache.resolution = (
	  g := presentation M;
	  if options.Strategy === 0 then
	      g = generators gb g;  -- this is needed since the (current)
			      -- default algorithm, 0, needs a GB 
			      -- to be previously computed.
	  harddegreelimit := (
	       if class options.HardDegreeLimit === ZZ then {options.HardDegreeLimit}
	       else if harddegreelimit === null then harddegreelimit = {}
	       else error "expected HardDegreeLimit to be an integer or null");
	  W := new Resolution;
	  W.ring = R;
	  W.length = maxlevel;
	  W.DegreeLimit = degreelimit;
	  W.RawComputation = rawResolution(
	       raw g,					    -- the matrix
	       true,					    -- whether to resolve the cokernel of the matrix
	       maxlevel,				    -- how long a resolution to make
	       false,					    -- useMaxSlantedDegree
	       0,					    -- maxSlantedDegree (is this the same as harddegreelimit?)
	       options.Strategy,			    -- algorithm
	       options.SortStrategy			    -- strategy (is this the same as options.SortStrategy?)
	       );
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
	       if debugLevel > 0 then stderr << "--warning: about to recompute resolution, discarding data" << endl;
	       scan(keys C,i -> if class i === ZZ then remove(C,i));
	       scan(keys C.dd,i -> if class i === ZZ then remove(C.dd,i));
	       remove(C,complete);
	       resOptions := {
		    maxlevel,
		    inf options.SyzygyLimit,
		    inf options.PairLimit,
		    0, 0, 0};                   -- MES: these are three other options,
						-- to be filled in yet.
	       if not options.StopBeforeComputation then (
                    rawGBSetStop(W.RawComputation,
			 -- fill these in eventually:
			 false,				    -- always_stop
			 false,				    -- stop_after_degree
			 {},				    -- degree_limit
			 0,				    -- basis_element_limit
			 0,				    -- syzygy_limit
			 0,				    -- pair_limit
			 0,				    -- codim_limit
			 0,				    -- subring_limit
			 false,				    -- just_min_gens
			 {}				    -- length_limit
			 );
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
          Strategy => null		  -- algorithm to use, usually 1, but sometimes 2
	  }
     )

resolution Module := ChainComplex => o -> (M) -> (
     R := ring M;
     k := ultimate(coefficientRing, R);
     oR := options R;
     if oR.?SkewCommutative and oR.SkewCommutative then (
	  if isHomogeneous M then (
	       (resolutionInEngine default(o,Strategy2))(M))
	  else
	       (resolutionBySyzygies o)(M))
     else if not isCommutative R then (resolutionBySyzygies o)(M)
     else if R === ZZ then (resolutionBySyzygies o)(M)
     else if not isHomogeneous M then (resolutionByHomogenization o)(M)
     else if isQuotientRing R then (resolutionInEngine default(o,Strategy2))(M)
     else (resolutionInEngine default(o,Strategy1))(M)
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
     v := {};
     lab := {};
     if options#TotalPairs     === true then (
	  v = append(v,getpairs r);
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
	  splice {"totals:", apply(minrow .. maxrow, i -> toString i | ":")},
	  s -> (9-# s,s));
     v = transpose v;
     v = drop(v,3);
     v = pack(maxcol-mincol+1,v);
     totals := apply(transpose v, sum);
     v = prepend(totals,v);
     v = transpose v;
     v = applyTable(v, toSequence);
     if numops === 1
     then v = applyTable(v,(i) -> if i === 0 then "." else toString i)
     else v = applyTable(v,args -> concatenate("(", between(",",apply(args,toString)), ")" ));
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
     printString(stdio,v);
     << endl;
     )

status ChainComplex := options -> (C) -> status(C.Resolution, options)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2"
-- End:
