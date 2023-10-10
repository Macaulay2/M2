-- Copyright 1999-2009 by Anton Leykin and Harrison Tsai

-----------------------------------------------------------------------
-- globalBFunction (f) -> bf
-- f = polynomial (assumed to be an element of 
-- 	     a Weyl algebra with no parameters)
-- bf = global b-function (polynomial in K[s], where K is 
-- 	     the coefficient field)
--
-- (method: definition 5.3.10 in Saito-Strumfels-Takayama)
-----------------------------------------------------------------------

globalBFunction = method(Options => {Strategy => GeneralBernsteinSato})

-- makes polynomial f monic (internal) 
makeMonic := f -> ( if coefficientRing ring f === QQ 
     then (1 / (leadCoefficient f)) * f 
     else (1 // (leadCoefficient f)) * f
     );

-- also used in multiplierIdeals.m2
star = method()
star (Ideal,List) := (I,vw) -> (
     W := ring I;
     n := numgens W;
     u := symbol u;
     Wu := (coefficientRing W)(monoid [gens W, u_1, u_2, 
	       WeylAlgebra => W.monoid.Options.WeylAlgebra]);
     (u1,u2) := toSequence (gens Wu)_{n,n+1};
     Ih := ideal homogenize(sub(gens I,Wu), u1, vw | {1,0}) + ideal (1-u1*u2);
     sub(eliminateWA(Ih,{u1,u2}),W)
     ) 

--------------------------------------------------------
-- version that uses bFunction
--------------------------------------------------------
-- internal
globalBFunctionIdeal := method(Options => {Strategy => TryGeneric})
globalBFunctionIdeal RingElement := RingElement => o -> f -> (
     W := ring f;
     createDpairs W;
     dpI := W.dpairInds;
     
     -- sanity check
     if (#(W.dpairInds#2) != 0) then
     error "expected no central variables in Weyl algebra";
     if any(listForm f, m -> any(dpI#1, i -> m#0#i != 0)) then
     error "expected no differentials in the polynomial";
     
     t := symbol t;
     dt := symbol dt;     
     WT := (coefficientRing W)(monoid [ t, dt, (entries vars W)#0, 
	       WeylAlgebra => W.monoid.Options.WeylAlgebra | {t => dt}]);
     t = WT_t;
     dt = WT_dt;
     w := {1} | toList (((numgens W) // 2):0);
     f' := substitute(f,WT);
     If := ideal ({t - f'} 
     	  | (dpI#1 / (i->(
	       	    	 DX := WT_(i+2);
	       	    	 (DX * f' - f' * DX) * dt + DX
	       	    	 )))
	  );
     pInfo(666, toString If);
     bfunc := bFunction(If, w, Strategy => o.Strategy);
     s := (ring bfunc)_0;
     makeMonic substitute(bfunc, { s => -s - 1 })
     );--end of globalBFunction

------------------------------------------------------------------
-- REDUCED global b-function: b(s)/(s+1)
------------------------------------------------------------------
globalRB := method()
globalRBAnnFs = method()

globalRB (RingElement,Boolean) := RingElement => (f,isRed) -> (
     W := ring f;
     AnnI := AnnFs f;
     globalRBAnnFs(f, AnnI, isRed);
)

globalRBAnnFs (RingElement,Ideal,Boolean) := RingElement => (f,AnnI,isRed) -> (
     W := ring f;
     Ws := ring AnnI;
     ns := numgens Ws;
     createDpairs W;
     df := apply(W.dpairVars#1, dx->dx*f-f*dx);
     
     elimWs := (coefficientRing Ws)(monoid [(entries vars Ws)#0,
	  WeylAlgebra => Ws.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate (ns-1)]);
     ff := substitute(matrix{{f}|
	       if isRed then df else {}
	       },elimWs);
     elimAnnI := substitute(AnnI, elimWs);
     H := gens elimAnnI | ff;
     
     gbH := gb H;

     bpolys := selectInSubring(1, gens gbH);
     if (bpolys == 0) then error "module not specializable";
     if (rank source bpolys > 1) then error "ideal principal but not
     realized as such.  Need better implementation";
     bpoly := bpolys_(0,0);

     Ks := (coefficientRing W)(monoid [Ws_(ns-1)]);
     bpoly = substitute(bpoly, Ks);
     if isRed then bpoly = bpoly*(Ks_0+1);
     bpoly
     )

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- This routine computes a Bernstein operator for a polynomial f
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
globalB = method()

globalB(Ideal, RingElement) := HashTable => (I, f) -> (
     W := ring I;
     AnnI := AnnIFs (I,f);
     Ws := ring AnnI;
     ns := numgens Ws;
     
     elimWs := (coefficientRing Ws)(monoid [(entries vars Ws)#0,
	  WeylAlgebra => Ws.monoid.Options.WeylAlgebra,
	  MonomialOrder => Eliminate (ns-1)]);
     ff := substitute(f,elimWs);
     elimAnnI := substitute(AnnI, elimWs);
     H := gens elimAnnI | matrix{{ff}};
     
     gbH := gb(H, ChangeMatrix => true);

     bpolys := selectInSubring(1, gens gbH);
     if (bpolys == 0) then error "module not specializable";
     if (rank source bpolys > 1) then error "ideal principal but not
     realized as such.  Need better implementation";
     bpoly := bpolys_(0,0);

     ind := position((entries gens gbH)#0, i -> (i == bpoly));
     C := getChangeMatrix gbH;
     Bop := C_(numgens source H - 1, ind);
     
     Ks := (coefficientRing W)(monoid [Ws_(ns-1)]);
     bpoly = substitute(bpoly, Ks);
     hashTable {Bpolynomial => bpoly, Boperator => Bop}
     )

globalBoperator = method()
globalBoperator(RingElement) := RingElement => (f) -> (
     W := ring f;
     createDpairs W;
     I := ideal W.dpairVars#1;
     (globalB(I,f))#Boperator
     )

--------------------------------------------------------------
-- MAIN function
-------------------------------------------------------------
globalBFunction RingElement := RingElement => o -> f -> (
     if #(options (ring f).monoid)#WeylAlgebra == 0 -- not WA 
     then (
	  D := makeWeylAlgebra(ring f,SetVariables=>false);
	  f = sub(f,D);
	  );
     result := (
	  if o.Strategy == IntRing 
	  or o.Strategy == TryGeneric 
	  or o.Strategy == NonGeneric 
     	  then globalBFunctionIdeal(f, o)
     	  else if o.Strategy == ReducedB
	  then globalRB (f,true)
	  else if o.Strategy == ViaAnnFs
	  then globalRB (f,false)
	  else if o.Strategy == GeneralBernsteinSato
	  then generalB {f}
	  else error "wrong Strategy option"
	  );
     result
     )

--------------------------------------------------------------
-- global generalized Bernstein-Sato polynomial
generalB = method(Options => {
	  Strategy => InitialIdeal,
	  Exponent => null
	  })
generalB List := RingElement => o-> F -> generalB(F, 1_(ring first F), o)     
generalB (List, RingElement) := RingElement => o->(F,g) -> (
-- Input:   F = {f_1,...,f_r}, a list of polynomials in n variables                                                                                                                                       
--          g, a polynomial
-- Output:  b_{f,g}^{(m)}, a generalized B-S polynomial, an element of QQ[s]
     m := o.Exponent;
     if m =!= null and (class m =!= ZZ or m<1) then error "expected a positive integer for Exponent";
     if #F == 0 then error "the list is empty";
     if #(options (ring first F).monoid)#WeylAlgebra == 0 -- not WA 
     then (
	  D := makeWeylAlgebra(ring first F,SetVariables=>false);
	  F = apply(F, f->sub(f,D));
	  g = sub(g,D);
	  );
     r := #F; 
     AnnI := MalgrangeIdeal F;
     DY := ring AnnI;
     K := coefficientRing DY;
     n := numgens DY // 2 - r; -- DY = k[x_1,...,x_n,t_1,...,t_r,dx_1,...,dx_n,dt_1,...,dt_r]
          w := toList(n:0) | toList(r:1);
     I1 := 
     if m===null and o.Strategy===InitialIdeal then (
	  inw(intersect(AnnI, ideal sub(g, DY)), -w|w)
	  ) 
     else if m===null and o.Strategy===StarIdeal then (
	  star(AnnI,-w|w) + sub(g*ideal F,DY)
	  )
     else if m=!=null then (
	  star(AnnI,-w|w) + (sub(ideal F, DY))^m
	  )
     else error "uknwnown Strategy";
     
     -- use linear algebra
     s := symbol s; 
     P := -sum(r,i->DY_(2*n+r+i)*DY_(n+i)); -- s = -(dt_1*t_1 + ... + dt_r*t_r)
     gg := sub(g,DY);
     powers := matrix(DY,{{}});
     d := 0;
     while true do (
	  powers = powers | matrix {{P^d*gg % I1}};
	  Cpowers := coefficients powers;
	  KerC := ker lift(Cpowers#1, K); -- kernel of the coefficients matrix
	  if KerC != 0 then (
	       Sring := K(monoid[s]);
	       return sum(d+1, i->(gens KerC)_(i,0)*Sring_0^i);
	       ) ;
	  d = d + 1;
	  );
     
     /// -- old code below
     SDY := K (monoid [s, gens DY, WeylAlgebra => DY.monoid.Options.WeylAlgebra, MonomialOrder=>{Weights=>toList(n+1:0)|toList(n+2*r:1)}]);
     v := gens SDY; 
     SX := take(v,{0,n}); notSX := drop(v,{0,n}); 
     T := take(v,{n+1,n+r}); dT := take(v,{2*n+r+1,2*(n+r)});
     SXring := K(monoid [SX]);
     SDYtoSX := map(SXring,SDY, gens SXring | toList(n+2*r:0) );
     I2' := sub(I1,SDY) + ideal (SDY_0 + sum(r,i->dT#i*T#i)); -- I2 + (s-\sigma)
     --if o.GuessedRoots =!= null and #o.GuessedRoots == 1 
     --then I2' = I2' + ideal(SDY_0 - first o.GuessedRoots);
     G2' := flatten entries gens gb I2'; 
     I2 := SDYtoSX ideal select(G2', f->all(listForm f, m->sum drop(first m,n+1)==0));
     --I2 := SDYtoSX eliminate(notSX,I2'); -- works incorrectly for Weyl algebras
     g' := SDYtoSX sub(g,SDY);
     I3 := I2 : g';
     I4 := eliminate(drop(gens SXring,1), I3);
     Sring = K(monoid [SXring_0]);
     sub(I4_0, Sring)
     ///
     )

--------------------------------------------------------------
-- global generalized Bernstein-Sato ideal 
-- internal
generalBideal = method(Options => { Strategy => ViaLinearAlgebra })
generalBideal (List, RingElement) := RingElement => o->(F,g) -> (
-- Input:   F = {f_1,...,f_r}, a list of polynomials in n variables                                                                                                                                       
--                             (f_i has to be an element of A_n, the Weyl algebra).                                                                                                                       
--          g, a polynomial
-- Output:  the generalized B-S ideal, an ideal of QQ[s_1..s_r]
     r := #F; 
     AnnI := MalgrangeIdeal F;
     DY := ring AnnI;
     K := coefficientRing DY;
     n := numgens DY // 2 - r; -- DY = k[x_1,...,x_n,t_1,...,t_r,dx_1,...,dx_n,dt_1,...,dt_r]
     I0 := --if g==1 then 
     AnnI; --else intersect(AnnI, ideal sub(g, DY));
     w := toList(n:0) | toList(r:1);
     I1 := inw(I0, -w|w);
     --I1 := I0; -- + ideal sub(product F,DY);
     s := symbol s; 
     SDY := K (monoid [s_1..s_r, gens DY, 
	       WeylAlgebra => DY.monoid.Options.WeylAlgebra, MonomialOrder=>{Weights=>toList(n+r:0)|toList(n+2*r:1)}]);
     v := gens SDY; 
     SX := take(v,{0,n+r-1}); notSX := drop(v,{0,n+r-1}); 
     T := take(v,{n+r,n+2*r-1}); dT := take(v,{2*n+2*r,2*n+3*r-1});
     SXring := K(monoid [SX]);
     SDYtoSX := map(SXring,SDY, gens SXring | toList(n+2*r:0) );
     I2' := sub(I1,SDY) + ideal apply(r,i->SDY_i + dT#i*T#i); -- I2 + (s_1-dt_1*t_1,...,s_n-dt_n*t_n)
     G2' := flatten entries gens gb I2'; 
     I2 := SDYtoSX ideal select(G2', f->all(listForm f, m->sum drop(first m,n+r)==0));
     g' := SDYtoSX sub(g,SDY);
     I3 := I2 : g';
     I4 := eliminate(drop(gens SXring,r), I3);
     Sring := K(monoid [take(gens SXring,r)]);
     sub(I4, Sring)
     )


