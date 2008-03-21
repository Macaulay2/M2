-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai


-- this routine computes an initial basis of an ideal or matrix with respect
-- a weight vector w.
inw = method()
inw (RingElement, List) := (L, w) -> (
     (inw(matrix{{L}}, w))_(0,0)
     )

inw (Ideal, List) := (I, w) -> (
     ideal inw(gens I, w)
     )

inw (Matrix, List) := (m, w) -> (
     -- preprocessing
     W := ring m;
     -- error checking
     if W.monoid.Options.WeylAlgebra === {}
     then error "expected a Weyl algebra";
     if any(W.monoid.Options.WeylAlgebra, v -> class v =!= Option)
     then error "expected non-homogenized Weyl algebra";
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     createDpairs W;
     
     -- case 1: weight vector w=(u,v) has u+v > 0.
     if all(toList(0..(#W.dpairInds#0)-1), i ->
	  w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) > 0)
     then (
	  if not W.?CommAlgebra then createCommAlgebra W;
	  tempW := (coefficientRing W)(monoid [(entries vars W)#0,
	       WeylAlgebra => W.monoid.Options.WeylAlgebra,
	       Weights => w, Global=>false]);
	  WtotempW := map(tempW, W, vars tempW);
	  tempWtoCA := map(W.CommAlgebra, tempW, vars W.CommAlgebra );
	  tempm := WtotempW m;
	  gbtempm := gb tempm;
	  inm := compress tempWtoCA leadTerm(1, gens gbtempm);
	  )

     -- case 2: use V-homogenization if u+v = 0 
     --	    and HOMOGENIZATION is turned off
     else if not getHomSwitch() and all(toList(0..(#W.dpairInds#0)-1), 
	       i -> w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) == 0) then (
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
	  nonCommInds := positions(toList(0..#W.dpairInds#0-1), i ->
	       w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) == 0);
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


-- this routine computes a grobner basis of an ideal or matrix with respect
-- a weight vector w.
gbw = method()
gbw (Ideal, List) := (I, w) -> (
     ideal gbw(gens I, w)
     )

gbw (Matrix, List) := (m, w) -> (
     -- preprocessing
     W := ring m;
     -- error checking
     if W.monoid.Options.WeylAlgebra === {} 
     then error "expected a Weyl algebra";
     if any(W.monoid.Options.WeylAlgebra, v -> class v =!= Option)
     then error "expected non-homogenized Weyl algebra";
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     createDpairs W;
     
     -- case 1: weight vector w=(u,v) has u+v > 0.
     if all(toList(0..(#W.dpairInds#0)-1), i ->
	  w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) > 0)
     then (
	  if not W.?CommAlgebra then createCommAlgebra W;
	  tempW := (coefficientRing W)(monoid [(entries vars W)#0,
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
     else if not getHomSwitch() and all(toList(0..(#W.dpairInds#0)-1), 
	       i -> w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) == 0) then (
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

-- These routines are the old names of inw. They will eventually be eliminated.
inW1 = method()
inW1 (Ideal, List) := (I, w) -> (
     setHomSwitch(false);
     inw(I, w)
     )
gbW1 = method()
gbW1 (Ideal, List) := (I, w) -> (
     setHomSwitch(false);
     gbw(I, w)
     )
inW2 = method()
inW2 (Ideal, List) := (I, w) -> (
     setHomSwitch(true);
     inw(I, w)
     )
gbW2 = method()
gbW2 (Ideal, List) := (I, w) -> (
     setHomSwitch(true);
     gbw(I, w)
     )
