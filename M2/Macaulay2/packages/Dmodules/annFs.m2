-- Copyright 1999-2008 by Anton Leykin and Harrison Tsai

---------------------------------------------------------------------------------
AnnFs = method()
AnnFs RingElement := Ideal => f -> (
-- Input:   f, a polynomial in n variables 
--             (has to be an element of A_n, the Weyl algebra).
-- Output:  Ann f^s, an ideal in A_n[s].	
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
AnnFs List := Ideal => F -> (
-- Input:   F = {f_1,...,f_r}, a list of polynomials in n variables
--                             (f_i has to be an element of A_n, the Weyl algebra).
-- Output:  Ann f_1^{s_1}...f_r^{s_r}, an ideal in A_n<t_1,..., t_r,dt_1,...,dt_r>.	
     W := ring first F;
     createDpairs W;
     dpI := W.dpairInds;
     dpV := W.dpairVars;
     
     -- sanity check
     if #(W.dpairInds#2) != 0 then
     error "expected no central variables in Weyl algebra";
     if any(F, f->any(listForm f, m -> any(dpI#1, i -> m#0#i != 0))) then
     error "expected no differentials in f";
     --
     n := #dpV#0; -- number of x_i
     r := #F;     -- number of t_i     
     t := symbol t;
     dt := symbol dt;
     WAopts := W.monoid.Options.WeylAlgebra | apply(r, i->t_i=>dt_i);
     WT := (coefficientRing W) ( monoid [ 
	       dpV#0, apply(r, i->t_i), dpV#1, apply(r, i->dt_i),  
	       WeylAlgebra => WAopts,
	       Degrees=>toList(n:0)|toList(r:-1)|toList(n:0)|toList(r:1)
	       ] );
     ideal apply(r, 
	  i->WT_(n+i) - sub(F#i,WT) -- t_i - f_i
	  ) + ideal apply(n,j->(
	       DXj := dpV#1#j;
	       -- dx_j + sum(r, i->(d/dx_j)f_i * dt_i)
	       sub(DXj,WT) + sum(r, i->sub(DXj*F#i-F#i*DXj,WT)*WT_(i+r+2*n) ) 
	       )) 	  
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

--------------------------------------------------------------------





