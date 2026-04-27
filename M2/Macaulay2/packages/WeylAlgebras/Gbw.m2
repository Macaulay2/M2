-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

----------------------------------------------------------------------------------
-- Homogenization switch
-- determines whether homogenized Weyl algebra is used in certain algorithms
----------------------------------------------------------------------------------
HOMOGENIZATION := true

setHomSwitch = method ()
setHomSwitch Boolean := Boolean => s -> (t := HOMOGENIZATION; HOMOGENIZATION = s; t)
getHomSwitch = () -> HOMOGENIZATION

----------------------------------------------------------------------------------

-- this routine computes an initial basis of an ideal or matrix with respect
-- a weight vector w.
inw = method()
inw (RingElement, List) := (L, w) -> (
     R := ring L;
     if #w != numgens R then error "wrong length of weight vector";
     lf := listForm L;
     mw := max apply(lf,t->sum(#w,i->t#0#i*w#i));
     part(mw,w,L)
     )
inw(Ideal,  List) := (I, w) -> ideal inw(gens I, w)
inw(Matrix, List) := (m, w) -> (
     -- preprocessing
     W := ring m;
     -- error checking
     if W.monoid.Options.WeylAlgebra === {}
     then error "expected a Weyl algebra";
     if any(W.monoid.Options.WeylAlgebra, v -> class v =!= List)
     then error "expected non-homogenized Weyl algebra";
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     createDpairs W;
     
     -- case 1: weight vector w=(u,v) has u+v > 0.
     if all(w_(W.dpairInds#0) + w_(W.dpairInds#1), x -> x > 0) then (
	  if not W.?CommAlgebra then createCommAlgebra W;
	  tempW := (coefficientRing W)(monoid [(entries vars W)#0,
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       Weights => w, Global=>false]);
	  WtotempW := map(tempW, W, vars tempW);
	  tempWtoCA := map(W.CommAlgebra, tempW, vars W.CommAlgebra );
	  tempm := WtotempW m;
	  gbtempm := gens gb tempm;
	  t:=numgens target gbtempm;
	  s:=numgens source gbtempm;
	  N:=mutableMatrix(tempW,t,s);
	  wdeg:=0;
	  maxi:=0;
	  for j from 0 to s-1 do (
	      maxi=0;
	  for i from 0 to t-1 do (
	      fo := listForm gbtempm_(i,j);
     	      wdeg = max apply(fo,t->sum(#w,i->t#0#i*w#i));
	      if wdeg>maxi then maxi=wdeg;
	      );
	  for i from 0 to t-1 do (
	       N_(i,j)=part(maxi,w,gbtempm_(i,j));
	      );
	  );
	  inm := compress tempWtoCA (matrix N)
	  )


     -- case 2: use V-homogenization if u+v = 0 
     --	    and HOMOGENIZATION is turned off
     else if not getHomSwitch() and w_(W.dpairInds#0) == -w_(W.dpairInds#1) then (
    	  if numrows m > 1 then 
	  error "some functions are not implemented for noncyclic D-modules (matrix with one row expected)"; 
	  -- Make the homogenizing Weyl algebra
     	  if not W.?HomWeylAlgebra then
     	  createHomWeylAlgebra (ring m);
     	  HW := W.HomWeylAlgebra;
     	  -- Make the new weight vector
     	  wts := prepend(-1,w);
     	  -- Homogenize m
     	  tempm = W.WAtoHWA m;
	  mm := homogenize(tempm, HW_0, wts);
	  -- Do the computation
	  inm = compress W.HWAtoWA leadTerm(1, gens gb mm);
	  )

     -- case 3: use homogeneous Weyl algebras if some component of (u+v) is zero
     --	    and HOMOGENIZATION is turned on
     else (
    	  if numrows m > 1 then 
	  error "some functions are not implemented for noncyclic D-modules (matrix with one row expected)"; 
	  h := symbol h;
	  Wh := (coefficientRing W)(monoid [(entries vars W)#0, h,
	       WeylAlgebra => append(W.monoid.Options.WeylAlgebra, h),
	       MonomialOrder => {
		    Weights => toList(numgens W + 1: 1), 
		    Weights => append(w,0),
		    GRevLex}, Global=>false]);
	  WtoWh := map(Wh, W, (vars Wh)_{0..numgens W - 1});
	  WhtoW := map(W, Wh, (vars W)_{0..numgens W - 1} | matrix{{1_W}});
	  wt := toList(numgens Wh:1);
	  tempm = homogenize(WtoWh m, Wh_(numgens Wh - 1), wt);
	  gbtempm = gb tempm;
	  nonCommInds := positions(w_(W.dpairInds#0) + w_(W.dpairInds#1), x -> x == 0);
	  -- if some components of (u+v) equal 0, others are greater than 0,
	  -- then associated graded is half commutative, half non-commutative
	  if #nonCommInds != #W.dpairInds#0 then (
	       grW := (coefficientRing W)(monoid [(entries vars W)#0,
		    WeylAlgebra => apply(nonCommInds, i ->
			 W.dpairVars#0#i => W.dpairVars#1#i), Global=>false]);
	       WtogrW := map(grW, W, vars grW);
	       inm = compress WtogrW WhtoW leadTerm(2, gens gbtempm);
	       )
     	  else inm = compress WhtoW leadTerm(2, gens gbtempm);
	  );
     inm
     )

----------------------------------------------------------------------------------

-- this routine computes a grobner basis of an ideal or matrix with respect
-- a weight vector w.
gbw = method(Options => { Strategy => null })
gbw(Ideal,  List) := opts -> (I, w) -> ideal gbw(gens I, w, opts)
gbw(Matrix, List) := opts -> (m, w) -> (
     -- preprocessing
     W := ring m;
     -- error checking
     if W.monoid.Options.WeylAlgebra === {} 
     then error "expected a Weyl algebra";
     if any(W.monoid.Options.WeylAlgebra, v -> class v =!= List)
     then error "expected non-homogenized Weyl algebra";
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     createDpairs W;
     
     -- case 1: weight vector w=(u,v) has u+v > 0.
     if all(w_(W.dpairInds#0) + w_(W.dpairInds#1), x -> x > 0) then (
	  if not W.?CommAlgebra then createCommAlgebra W;
	  tempW := (coefficientRing W)(monoid [gens W,
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       Weights => w, Global=>false]);
	  WtotempW := map(tempW, W, vars tempW);
	  tempWtoW := map(W, tempW, vars W);
	  tempm := WtotempW m;
	  gbtempm := gb tempm;
	  gbm := tempWtoW gens gbtempm;
	  )

     -- case 2: use V-homogenization if u+v = 0 
     --	    and HOMOGENIZATION is turned off
     else if not (opts.Strategy === homogenize or getHomSwitch())
     and w_(W.dpairInds#0) == -w_(W.dpairInds#1) then (
	  -- Make the homogenizing Weyl algebra
     	  if not W.?HomWeylAlgebra then
     	  createHomWeylAlgebra (ring m);
     	  HW := W.HomWeylAlgebra;
     	  -- Make the new weight vector
     	  wts := prepend(-1,w);
     	  -- Homogenize m
     	  tempm = W.WAtoHWA m;
	  mm := homogenize(tempm, HW_0, wts);
	  -- Do the computation
	  gbm = compress W.HWAtoWA gens gb mm;
	  )

     -- case 3: use homogeneous Weyl algebras if some component of (u+v) is zero
     --	    and HOMOGENIZATION is turned on
     else (
	  h := symbol h;
	  Wh := (coefficientRing W)(monoid [(entries vars W)#0, h,
	       WeylAlgebra => append(W.monoid.Options.WeylAlgebra, h),
	       MonomialOrder => {
		    Weights => toList(numgens W + 1: 1), 
		    Weights => append(w,0),
		    GRevLex}, Global=>false]);
	  WtoWh := map(Wh, W, (vars Wh)_{0..numgens W - 1});
	  WhtoW := map(W, Wh, (vars W)_{0..numgens W - 1} | matrix{{1_W}});
	  wt := toList(numgens Wh:1);
	  tempm = homogenize(WtoWh m, Wh_(numgens Wh - 1), wt);
	  gbtempm = gb tempm;
	  gbm = compress WhtoW gens gbtempm;
	  );
     gbm
     )

--------------------------------------------------------------------------------

-- This routine computes the characteristic ideal of a D-module
characteristicIdeal = method()
characteristicIdeal Ideal := I -> (
    if not isWeylAlgebra(W := ring I) then error "expected an ideal in a Weyl algebra";
    createDpairs W;
    w := apply( toList(0..numgens W - 1),
	i -> if member(i, W.dpairInds#1) then 1 else 0 );
    ideal mingens inw (I, w)
    )

characteristicIdeal Module := M -> (
    if not isWeylAlgebra(W := ring M) then error "expected a module over a Weyl algebra";
    m := presentation M;
    createDpairs W;
    w := apply( toList(0..numgens W - 1),
	i -> if member(i, W.dpairInds#1) then 1 else 0 );
    ideal mingens ann cokernel inw (m, w)
    )

--------------------------------------------------------------------------------

-- This routine computes the singular locus of a D-ideal
-- SHOULD IT BE CHANGED SO THAT OUTPUT IS IN POLY SUBRING?
DsingularLocus = method()
DsingularLocus Ideal  := I -> DsingularLocus comodule I
DsingularLocus Module := M -> (
    if not isWeylAlgebra(W := ring M) then error "expected a module over a Weyl algebra";
    createDpairs W;
    if not W.?CommAlgebra then createCommAlgebra W;
    I1 := characteristicIdeal M;
    I2 := W.WAtoCA ideal W.dpairVars#1;
    -- do the saturation
    SatI := saturate(I1, I2);
    -- set up an auxiliary ring to perform intersection
    tempCA := (coefficientRing W)(monoid [W.dpairVars#1, W.dpairVars#0,
            MonomialOrder => Eliminate (#W.dpairInds#1)]);
    newInds := inversePermutation join(W.dpairInds#1, W.dpairInds#0);
    CAtotempCA := map(tempCA, W.CommAlgebra,
	matrix {apply(newInds, i -> tempCA_i)});
    tempCAtoCA := map(W.CommAlgebra, tempCA, matrix{ join (
		apply(W.dpairVars#1, i -> W.WAtoCA i),
		apply(W.dpairVars#0, i -> W.WAtoCA i) ) } );
    -- do the intersection
    gbSatI := gb CAtotempCA SatI;
    I3 := ideal compress tempCAtoCA selectInSubring(1, gens gbSatI);
    if I3 == ideal 1_(W.CommAlgebra) then W.CAtoWA I3
    else W.CAtoWA radical I3
    )
