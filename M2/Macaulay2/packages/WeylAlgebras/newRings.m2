-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

-- All internal functions

-- used as a key
protect WAtoHWA
protect HWAtoWA
protect HomWeylAlgebra
protect IntRing

-- this function associates to a Weyl algebra W
-- the Weyl algebra W[h], where h is used for homogenizing purposes
createHomWeylAlgebra = method()
createHomWeylAlgebra(PolynomialRing) := (W) -> (
     h := symbol h;
     W.HomWeylAlgebra = (coefficientRing W)(monoid [h, (entries vars W)#0,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate 1]);
     HW := W.HomWeylAlgebra;
     W.WAtoHWA = map(HW, W, (vars HW)_{1..numgens W});
     W.HWAtoWA = map(W, HW, matrix{{1_W}} | (vars W));
     );

-- this function associates to a polynomial ring R
-- the ring R[s], where s is used for intersecting purposes
protect RtoIR
protect IRtoR
createIntRing = method()
createIntRing(PolynomialRing) := (R) -> (
     s := symbol s;
     R.IntRing = (coefficientRing R)(monoid [(entries vars R)#0, s,
	  WeylAlgebra => R.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate numgens R]);
     IR := R.IntRing;
     R.RtoIR = map(IR, R, (vars IR)_{0..numgens R - 1});
     R.IRtoR = map(R, IR, (vars R) | matrix{{0_R}});  
     );

-- this function associates to a Weyl algebra W
-- the associated commutative ring W.CommRing in the same number of variables
-- and constructs two maps: WAtoCR and CRtoWA 
protect CRtoWA
protect CommRing
protect WAtoCR
createAssCommRing = method()
createAssCommRing(PolynomialRing) := (W) -> (
     if (W.monoid.Options.WeylAlgebra === {}) then
     error "Expected a Weyl algebra" ;
     
     createDpairs W;
     dpI := W.dpairInds;
     
     x := symbol x;
     ksi := symbol ksi;
     c := symbol c;
     
     W.CommRing = (coefficientRing W)(monoid [dpI#0 / (i -> x_i), dpI#1 / (i -> ksi_i), dpI#2 / (i -> c_i)]);
     CR := W.CommRing;
     << "CR = " << CR << endl;
     W.WAtoCR = map(CR, W, (vars CR)_(apply(numgens W, i->(
	       		 local p;
	       		 if (p = position(dpI#0, j -> j==i)) =!= null 
			 then p
			 else if (p = position(dpI#1, j -> j==i)) =!= null 
			 then p + #dpI#0
			 else position(dpI#2, j -> j==i) + #dpI#0 + #dpI#1   
	       		 ))));
     W.CRtoWA = map(W, CR, (vars W)_( dpI#0 | dpI#1 | dpI#2));
     );

-- This function for Weyl Algebra W creates W.ThetaRing 
-- and two functions W.isGeneric and W.WtoT.
--
-- If W is K[t_0, ..., t_m, x_0, ...,  x_n, dx_0, ..., dx_n] then 
-- ThetaRing = K[t_0, ..., t_m, theta_0, ..., theta_n], where theta_i = x_i * dx_i.
--
-- isGeneric tells whether its argument is of form x^a * p(theta, t) * dx^b.
--
-- WtoT(f) returns the generator in ThetaRing that corresponds to f in W.
-- ( isGeneric(f) == true should hold. )
protect isGeneric
protect WtoT
protect ThetaRing
createThetaRing = method()
createThetaRing PolynomialRing := W -> (
     createDpairs W;
     dpV := W.dpairVars;
     dpI := W.dpairInds;
     
     theta := symbol theta;
     W.ThetaRing = (coefficientRing W)[ theta_0..theta_(#(dpV#0)-1) ];
     
     W.isGeneric = g -> (
	  t := 0_(W.ThetaRing);
	  e := listForm g;
	  a := apply(dpI#0, i->e#0#0#i); -- exponent for x
	  b := apply(dpI#1, i->e#0#0#i); -- exponent for Dx
	  all(e,  m->(
		    a' := dpI#0 / (i->m#0#i);
		    b' := dpI#1 / (i->m#0#i);
		    (a - b) == (a' - b')
		    ))  	  
	  );
     
     W.WtoT = g -> (
	  t := 0_(W.ThetaRing);
	  e := listForm g;
	  a := apply(dpI#0, i->e#0#0#i); -- exponent for x
	  b := apply(dpI#1, i->e#0#0#i); -- exponent for Dx
	  if any(e,  m->(
		    a' := dpI#0 / (i->m#0#i);
		    b' := dpI#1 / (i->m#0#i);
		    (a - b) != (a' - b')
		    )) then error "expected argument of form x^a * p(theta, t) * dx^b";  
	  c := a - b;
	  a = c / (u -> if u>0 then u else 0);
	  b = c / (u -> if u<0 then -u else 0);
	  g = product(#(dpI#1), i->W_(dpI#1#i)^(a#i)) 
	  * product(#(dpI#0), i->W_(dpI#0#i)^(b#i))
	  * g;
	  while g != 0 do(
	       c = leadCoefficient g;
      	       d := first exponents leadMonomial g;
	       t = t + c * 	
	       product(#(dpI#0), i -> 
		    theta_i^(min(d#(dpI#0#i),d#(dpI#1#i))));	   
	       g = g - c *     
	       product(#(dpI#0), i -> 
		    (W_(dpI#0#i) *  W_(dpI#1#i))^(min(d#(dpI#0#i),d#(dpI#1#i)))); 
	       ); 
	  t
	  );
    W.ThetaRing, W.WtoT)

