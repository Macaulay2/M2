-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai


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

inw (Ideal, List) := (I, w) -> (
     ideal inw(gens I, w)
     )

protect WAtoHWA								    -- used as a key
protect HWAtoWA
protect HomWeylAlgebra

inw (Matrix, List) := (m, w) -> (
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
     else if not getHomSwitch() and all(toList(0..(#W.dpairInds#0)-1), 
	       i -> w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) == 0) then (
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
     if any(W.monoid.Options.WeylAlgebra, v -> class v =!= List)
     then error "expected non-homogenized Weyl algebra";
     if #w =!= numgens W
     then error ("expected weight vector of length " | numgens W);
     createDpairs W;
     
     -- case 1: weight vector w=(u,v) has u+v > 0.
     if all(toList(0..(#W.dpairInds#0)-1), i ->
	  w#(W.dpairInds#0#i) + w#(W.dpairInds#1#i) > 0)
     then (
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

TEST///
x = symbol x;
dx = symbol dx;
y = symbol y;
dy = symbol dy;
-- Tests finding Grobner basis in case when u+v>0
W = QQ[x,y,dx,dy, WeylAlgebra => {x=>dx,y=>dy}]
I = ideal(x^2+y^3, x*y);
J = ideal(x+dy,x*dx+y*dy);
K = ideal(y*dx, x*dx+y);
assert (entries gens gbw(I,{2,1,0,0}) == entries matrix {{x*y,x^2+y^3,y^4}});
assert (entries gens gbw(I,{1,2,0,0}) == entries matrix {{x*y,x^3,x^2+y^3}});
assert (entries gens gbw(J,{4,3,2,1}) == entries matrix{{1}});
assert (entries gens gbw(K,{2,1,0,0}) == entries matrix {{y*dx, x*dx+y, y^2}});
assert (entries gens gbw(K,{1,2,0,0}) == entries matrix {{x*dx^2+dx, x*dx+y}});
assert (sub(inw(I,{2,1,0,0}),W) == ideal(x*y,x^2,y^4));
assert (sub(inw(J,{3,2,3,1}),W) == W);
assert (sub(inw(K,{1,2,0,0}),W) == ideal(x*dx^2,y));
-- Testing when u+v = 0
assert (entries gens gbw(K,{2,3,-2,-3}) == entries matrix {{y*dx, x*dx+y,x*dx^2+dx}});
assert (entries gens gbw(K,{3,2,-3,-2})==entries matrix {{y*dx, x*dx+y,x*dx^2+dx}});
assert (entries gens gbw(K,{0,0,0,0}) == entries matrix {{y*dx, x*dx+y, y^2}});
assert (sub(inw(K,{2,3,-2,-3}),W) == ideal(y,x*dx^2+dx));
///


TEST///
x = symbol x;
dx = symbol dx;
y = symbol y;
dy = symbol dy;
z = symbol z;
dz = symbol dz;
-- Initial ideals and gb's in the same Grobner cone
W = QQ[x,y,z,dx,dy,dz, WeylAlgebra => {x=>dx,y=>dy,z=>dz}]
A = matrix{{1,1,1},{0,2,7}};
b = {1,5};
I = gkz(A,b);

-- weight vector of the form (-u,u)
w1 = {-1,-10,-30,1,10,30};
w2 = {-1,-10,-31,1,10,31};
I1 = inw(I, w1);
G1 = gbw(I, w1);
assert(I1 == inw(I, w2));
assert(G1 == gbw(I, w2));
setHomSwitch false;
I1' = inw(I, w1);
G1' = gbw(I, w1);
assert(I1' == I1);
assert(G1' == G1);
assert(I1' == inw(I, w2));
assert(G1' == gbw(I, w2));
setHomSwitch true;

-- weight vector (u,v) with u+v > 0
w1 = {0,1,2,3,4,100};
w2 = {0,1,2,3,4,101};
assert(inw(I,w1) == inw(I, w2));
assert(gbw(I,w1) == gbw(I, w2));

-- weight vector (u,v) with some comp's of u+v > 0, others equal to 0.
w1 = {1,-3,107,-1,4,-5};
w2 = {1,-3,108,-1,4,-5};
I1 = inw(I, w1);
assert(I1 == substitute(inw(I, w2), ring I1));
assert(gbw(I, w1) == gbw(I, w2));
///
