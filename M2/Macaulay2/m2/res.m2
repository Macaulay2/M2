--		Copyright 1995 by Daniel R. Grayson and Michael Stillman

Resolution = new Type of MutableHashTable
name Resolution := g -> "<<resolution " | name g.handle | ">>"

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
          Algorithm => null		  -- algorithm to use, usually 1, but sometimes 2
	  }
     )

res = resolution

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
     C    := resolution(MH, options, LengthLimit => resolutionLength(R,options));
     toR  := map(R, RH, vars R | 1);
     toR C)

resolutionBySyzygies := options -> (M) -> (
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

resolutionInEngine := options -> (M) -> (
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

resolution Module := o -> (M) -> (
     R := ring M;
     k := ultimate(coefficientRing, R);
     oR := options R;
     if oR.?SkewCommutative and oR.SkewCommutative then (
	  if isHomogeneous M then (
	       (resolutionInEngine default(o,Algorithm2))(M))
	  else
	       (resolutionBySyzygies o)(M))
     else if k === ZZ or not isCommutative R then (resolutionBySyzygies o)(M)
     else if not isHomogeneous M then (resolutionByHomogenization o)(M)
     else if isQuotientRing R then (resolutionInEngine default(o,Algorithm2))(M)
     else (resolutionInEngine default(o,Algorithm1))(M)
     )

resolution Matrix := options -> (f) -> extend(
     resolution(target f, options), 
     resolution(source f, options), 
     matrix f)

resolution Ideal := options -> (I) -> resolution(
     if I.?quotient 
     then I.quotient
     else I.quotient = (ring I)^1/I,
     options)

-- MES: these need to be documented.  They also only work currently
-- for correct strategy... (non-Schreyer resolution).  In fact, the
-- only way to obtain that is use Strategy => xxx, some xxx.

mingens(ZZ,Resolution) := options -> (level,g) -> (
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

statusDefaults := new OptionTable from {
     TotalPairs => true,
     PairsRemaining => false,
     Monomials => false
     }
status = method (Options => statusDefaults)

status Resolution := options -> (r) -> (
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
     printString(stdio,v);
     )

status ChainComplex := options -> (C) -> status(C.Resolution, options)

