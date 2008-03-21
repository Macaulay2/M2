-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

---------------------------------------------------------------------------------
-- Returns Ann f^s in K[s].
-- Input:   f: Polynomial in A_n(K).
-- Output:  an ideal in A_n[s].	
AnnFs = method()
AnnFs(RingElement) := Ideal => f -> (
     W := ring f;
     createDpairs W;
     dpI := W.dpairInds;
     dpV := W.dpairVars;
     
     -- sanity check
     if #(W.dpairInds#2) != 0 then
     error "expected no central variables in Weyl algebra";
     if any(listForm f, m -> any(dpI#1, i -> m#0#i != 0)) then
     error "expected no differentials in f";

     AnnIFs(ideal dpV#1, f)
     );
------------------------------------------------------------------------------
AnnIFs = method()
AnnIFs(Ideal, RingElement) := Ideal => (I, f) -> (
     pInfo(1, "AnnIFs: Warning! The method expects an f-saturated module"); 
     W := ring I;
     createDpairs W;
     n := numgens W;
          
     t := symbol t;
     dt := symbol dt;
     WAopts := W.monoid.Options.WeylAlgebra | {t => dt};
     WT := (coefficientRing W)(monoid [ t, dt, (entries vars W)#0, 
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ]);
     u := symbol u;
     v := symbol v;
     WTUV := (coefficientRing W)(monoid [ u, v, t, dt, (entries vars W)#0,
	  WeylAlgebra => WAopts,
	  MonomialOrder => Eliminate 2 ]);
     (u,v,t,dt) = (WTUV_u,WTUV_v,WTUV_t,WTUV_dt);
     WTUVtoWT := map(WT,WTUV,{0,0} | flatten entries vars WT);     
     createDpairs WTUV;
     WtoWTUV := map(WTUV, W, (vars WTUV)_{4..n+3});
     -- twist generators of I into generators of KI
     f' := substitute(f,WTUV);
     twistList := apply( toList(3..n+3), 
	  i -> WTUV_i + (WTUV_i*f' - f'*WTUV_i)*dt);
     twistMap := map(WTUV, WTUV, matrix{{u,v,t-f'}|twistList});
     tempKI := twistMap(ideal t + WtoWTUV I);
     wts := {1,-1,1,-1} | toList(n:0);
     KI := ideal homogenize(gens tempKI, u, wts);
     
     g := (entries gens KI)#0 | { u * v - 1 } | 
     if isHomogeneous f and I == ideal W.dpairVars#1
     then {sum(#(WTUV.dpairVars#0), i->(
		    WTUV.dpairVars#0#i * WTUV.dpairVars#1#i
		    )) + (first degree f - 1)*t*dt + (first degree f)}
     else {};
     preGens := apply(flatten entries selectInSubring(1, gens gb ideal g), 
	  a->WTUVtoWT a) ;
     s := symbol s;
     WS := (coefficientRing W)(monoid [(entries vars W)#0, s,
	  WeylAlgebra => W.monoid.Options.WeylAlgebra]);
     s = WS_s;
     t = WT_symbol t;
     dt = WT_symbol dt;
     WTtoWS := g -> (
	  e := first exponents leadMonomial g;
	  if e#0 > e#1 then g = dt^(e#0-e#1) * g
	  else g = t^(e#1-e#0) * g;
	  g' := 0_WS;
	  while (d := first exponents leadMonomial g; d#0 * d#1 != 0) do(
	       c := leadCoefficient g;
	       g' = g' + c * (-s-1)^(d#1) * WS_(drop(d, 2) | {0}); -- >%-0	
	       g = g - c * (t*dt)^(d#1) * WT_({0,0} | drop(d, 2));
	       ); 
	  g' + substitute(g, WS)
	  );
     ideal (preGens / WTtoWS) 
     )






