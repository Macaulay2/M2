---------------------------------------------------------------------------------
-- Returns Ann f^s in K[s].
-- Input:   f: Polynomial in D_n(K).
-- Output:  an ideal in D_n[s].	
AnnFs = method()
AnnFs(RingElement) := f -> (
     W := ring f;
     if (W.?dpairVars == false) then
     createDpairs W;
     dpI := W.dpairInds;
     dpV := W.dpairVars;
     
     -- sanity check
     if (#(W.dpairInds#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if any(listForm f, m -> any(dpI#1, i -> m#0#i != 0)) then
     error "expected no differentials in f";

     AnnIFs(ideal dpV#1, f)
     );

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes a Bernstein polynomial & operator for a polynomial f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

AnnIFs = method()
AnnIFs(Ideal, RingElement) := (I, f) -> (
     W := ring I;
     n := numgens W;
     
     t := symbol t;
     dt := symbol dt;
     WAopts := W.monoid.Options.WeylAlgebra | {t => dt};
     WT := (coefficientRing W)[ t, dt, (entries vars W)#0, 
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ];
     u := symbol u;
     v := symbol v;
     WTUV := (coefficientRing W)[ u, v, t, dt, (entries vars W)#0,
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ];
     WtoWTUV := map(WTUV, W, (vars WTUV)_{4..n+3});
     -- twist generators of I into generators of KI
     f' := substitute(f,WTUV);
     twistList := apply( toList(3..n+3), 
	  i -> WTUV_i + (WTUV_i*f' - f'*WTUV_i)*dt);
     twistMap := map(WTUV, WTUV, matrix{{u,v,t-f'}|twistList});
     tempKI := twistMap(ideal t + WtoWTUV I);
     wts := {1,-1,1,-1} | toList(n:0);
     KI = ideal homogenize(gens tempKI, u, wts);
     
     g := (entries gens KI)#0 | { u * v - 1 };
     preGens := flatten entries substitute(
	  selectInSubring(1, gens gb ideal g), WT);
     use WT;
     s := symbol s;
     WS := (coefficientRing W)[(entries vars W)#0, s,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra];
     WTtoWS := g -> (
	  e := exponents leadMonomial g;
	  if e#0 > e#1 then g = dt^(e#0-e#1) * g
	  else g = t^(e#1-e#0) * g;
	  g' := 0_WS;
	  while (d := exponents leadMonomial g; d#0 * d#1 != 0) do(
	       c := leadCoefficient g;
	       g' = g' + c * (-s-1)^(d#1) * WS_(drop(d, 2) | {0}); -- >%-0	
	       g = g - c * (t*dt)^(d#1) * WT_({0,0} | drop(d, 2));
	       ); 
	  g' + substitute(g, WS)
	  );
     use W;
     ideal (preGens / WTtoWS) 
     )






