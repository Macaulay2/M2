--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes a Bernstein operator for a polynomial f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
globalBFunction(Ideal, RingElement) := options -> (I, f) -> (
     W := ring I;
     AnnI := AnnIFs (I,f);
     Ws := ring AnnI;
     ns := numgens Ws;
     
     Ks := (coefficientRing W)[Ws_(ns-1)];
     elimWs := (coefficientRing Ws)[(entries vars Ws)#0,
	  WeylAlgebra => Ws.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate (ns-1)];
     use W;
     ff := substitute(f,elimWs);
     elimAnnI := substitute(AnnI, elimWs);
     H := gens elimAnnI | matrix{{ff}};
     bpoly := (mingens ideal selectInSubring(1, gens gb H))_(0,0);
     bpoly = substitute(bpoly, Ks);
     bpoly
     )
