-- Copyright 1999-2008 by Anton Leykin and Harrison Tsai

-- AnnFs uses the algorithm of Oaku-Takayama which involves adding 4 extra variables to the Weyl algebra and performing two eliminations. TODO: Implement the Briancon-Maisonobe algorithm. This would require support for working with Ore algebras (for instance [dt, s] = s) in the Macaulay2 core.

---------------------------------------------------------------------------------
AnnFs = method()
AnnFs RingElement := Ideal => f -> (
-- Input:   f, a polynomial in n variables 
--             (has to be an element of A_n, the Weyl algebra).
-- Output:  Ann f^s, an ideal in A_n[s].	
     if #(options (ring f).monoid)#WeylAlgebra == 0 -- not WA 
     then (
	  D := makeWeylAlgebra(ring f,SetVariables=>false);
	  f = sub(f,D);
	  );
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

-- This function used to be called AnnFs but was incomplete, it only computes the Malgrange ideal.

MalgrangeIdeal = method()
MalgrangeIdeal List := Ideal => F -> (
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
--This needs documentation.
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
diffRatFun = method()
diffRatFun (List, RingElement) := RingElement => (m,f) -> (
    -- INPUT: 	m    a list of nonnegative integers
    -- 	    	f    a polynomial or rational function
    -- OUTPUT: Let D_i be the partial derivative with respect to the ith variable in 
    -- ring of f. Then the output is the result of applying to f the product of the
    -- differential operators D_i^(m_i).
     if isPolynomialRing (Q:=ring f) then diff(Q_m,f)
     else if isField Q and isPolynomialRing (R:=last Q.baseRings) then (
	  assert isPolynomialRing R;    
	  a := position(m,i->i>0); -- position of the first nonzero entry of m
	  if a === null then f
	  else (
	       g := numerator f;
	       h := denominator f;
	       diffRatFun(replace(a,m#a-1,m), (diff(R_a,g)*h-g*diff(R_a,h))/h^2) 
	       )	  
	  )
     else error "polynomial or rational function function expected"   
     )
diffRatFun (List, RingElement, RingElement, ZZ) := (m,g,f,a) -> (
    -- INPUT: 	m       a list of nonnegative integers
    -- 	    	f,g     polynomials
    --	        a    	an integer
    -- OUTPUT: Let D_i be the partial derivative with respect to the ith variable in 
    -- ring of f. Then the output is essentially the result of applying to g/f^a the 
    -- product of the differential operators D_i^(m_i), except that the output is given
    -- in the form (numer, denom, power of denom).
     R := ring f;
     p := position(m,i->i>0);
     if p === null then (g,f,a)
     else  diffRatFun(replace(p,m#p-1,m), diff(R_p,g)*f-a*g*diff(R_p,f), f, a+1) 
     )
memoize'diffRatFun = memoize diffRatFun

-- IN: g,f in K[s,x_1,...,x_n]
-- OUT: h, where hf^(s-k) = f^(k-ord(D_m)) * D_m(g*f^s)
-- internal
kDiffFs = method()
kDiffFs (ZZ, List, RingElement, RingElement) := RingElement => (k,m,g,f) -> (
     a := position(m,i->i>0);
     if a === null then return g*f^k;
     g' := kDiffFs(k-1,replace(a,m#a-1,m),g,f);
     R := ring g;
     s := R_0;
     if a==0 then s*g'*f -- multiply by "s"
     else diff(R_a,g')*f+(s-k+1)*g'*diff(R_a,f)
     )
memoize'kDiffFs := memoize kDiffFs

kOrderAnnFa = method()
kOrderAnnFa(ZZ,RingElement,ZZ) := Ideal => (k,f,a) -> (
     R := ring f;
     fa := if a>=0 then f^a else 1/f^(-a);
     n := numgens R;
     d'indices := reverse flatten({{toList(n:0)}} | apply(k, d->compositions (n,d+1)));
     --time M := matrix {apply(d'indices, c->sub(f^(k+1)*memoize'diffRatFun(c,fa),R))};
     --time 
     M := matrix {  
	  apply(d'indices, c->(
		    (g',f',a') := memoize'diffRatFun(c,1_R,f,-a);
		    f^(k-a-a')*g'
		    ))  
	  };
     pInfo(4, toString M);
     syzygies := entries transpose gens gb syz M;     
     pInfo(3, {"syz="}|syzygies);
     D := makeWeylAlgebra(R, SetVariables=>false);
     zeros := toList(n:0);
     ideal Dprune gens ideal ({0_D}|apply(syzygies, s->sum(#d'indices, i->sub(s#i,D)*D_(zeros|d'indices#i))))
     )

kCoeffVectorWRTs = method()
kCoeffVectorWRTs (ZZ,RingElement,Ring,RingMap) := (k,h,R,Rs'to'R) -> (
     s := (ring h)_0;
     ret := apply(k+1, i->Rs'to'R((h%s^(i+1))//s^i));
     --print(h,ret);
     ret 
     )

kOrderAnnFs = method()
kOrderAnnFs(ZZ,RingElement) := Ideal => (k,f) -> (
     R := ring f;
     s := local s;
     Rs := (coefficientRing R)[s,gens R,MonomialOrder=>Eliminate 1]; 
     Rs'to'R := map(R,Rs,matrix{{0_R}}|vars R);
     f = sub(f,Rs);
     n := numgens R;
     d'indices := reverse flatten({{toList(n+1:0)}} | apply(k, d->compositions (n+1,d+1)));
     M := transpose matrix apply(d'indices, c->kCoeffVectorWRTs(k,memoize'kDiffFs(k,c,1_Rs,f),R,Rs'to'R));
     pInfo(4, toString M);
     syzygies := entries transpose gens gb syz M;     
     pInfo(3, {"syz="}|syzygies);
     D := makeWeylAlgebra(R, SetVariables=>false);
     Ds := (coefficientRing D)[gens D, s, WeylAlgebra=>D.monoid.Options.WeylAlgebra, Weights=>toList(2*n:0)|{1}]; 
     zeros := toList(n:0);
     ideal Dprune gens ideal ({0_Ds}|apply(syzygies, s->sum(#d'indices, 
		    i->sub(s#i,Ds)*Ds_(zeros|drop(d'indices#i,1)|take(d'indices#i,1))
		    )))
     )

--------------------------------------
-- Other things
--------------------------------------

-- reiffen curves
reiffen = method()
reiffen(ZZ,ZZ) := (p,q) -> (
     if not (p>=4 and q>=p+1) then error "wrong values of arguments: see documentation";
     n := 2; 
     x := symbol x;
     R := QQ[x_1..x_n]; 
     x_1^p+x_2^q+x_1*x_2^(q-1) 
     )

kappaAnnF1PlanarCurve = method()
kappaAnnF1PlanarCurve RingElement := f -> (
     a := 0; b := 1; -- warning: an artificial choice
     -- CheckGenericity
     Rf := ring f;
     aaa := symbol aaa;
     R := (coefficientRing Rf)(monoid[gens Rf,aaa_1..aaa_(numgens Rf)]); 
     gensRf := (vars R)_(toList(0..numgens Rf-1));
     ff := (map(R,Rf,gensRf)) f;
     sz := syz matrix{{diff(R_0,ff),diff(R_1,ff)}};
     I := ideal((vars R)_(toList(numgens Rf..numgens R-1))*sz) + ideal ff;
     J := saturate(I,ideal gensRf);
     if sub(J, matrix{{0_R,0,a,b}}) != ideal 1_R
     then "algorithm's artificial choice is non-generic: see code";

     -- main algorithm
     mult := min(flatten entries monomials f / first@@degree);
     k := 1;
     local A;
     -- store data in a hashtable
     sf := toString f;
     pInfo(2, {"data#\"", sf, "\" = new MutableHashTable"});
     pInfo(2, {"data#\"", sf, "\"#\"m\" = new MutableHashTable"});
     pInfo(2, {"data#\"", sf, "\"#\"syz time\" = new MutableHashTable"});
     while true do (
     	  pInfo(1, {"-- order ", k, ": "});
     	  t'A := timing kOrderAnnFa(k,f,-1);
	  pInfo(1, {"-- syzygy computation time: ", first t'A});
	  pInfo(2, {"data#\"", sf, "\"#\"syz time\"#", k, " = ", first t'A});
	  A = last t'A;
     	  cI := charIdeal A;
--      	  t'dec := timing primaryDecomposition cI;
-- 	  pInfo(2, {"  primary decomposition time: ", first t'dec});
-- 	  dec := last t'dec;
-- 	  grD := ring cI;
--      	  conormalOfOrigin := select(dec, c->radical c == ideal take(gens grD, numgens grD//2));
-- 	  if #conormalOfOrigin != 1 then error "can't find the conormal of the origin"
-- 	  else (
--	       deg := degree first conormalOfOrigin;
	         R = ring cI; 
	         n := numgens R // 2;
		 cIcapRandomPlane := cI + ideal (drop(gens R,n)-{a,b});
		 --time print decompose cIcapRandomPlane;
		 --time 
		 dec2 := primaryDecomposition cIcapRandomPlane;
		 --time 
		 deg2 := degree first select(dec2,J->isSubset(ideal take(gens R,n),radical J));
		 deg := deg2;
	       pInfo(2, {"data#\"", sf, "\"#\"m\"#", k, " = ", deg});
	       pInfo(2, {"--  (deg,mult-1) = (", deg, ",", mult-1, ") : ", deg2});
	       if deg <= mult-1 then break;
--	       ); 
	  k = k+1;
	  );
     (k,A)
     )

---------------------------------------------------------------------------------
-- moved from Dsystems.m2

-- This routine takes a polynomial element f of the Weyl algebra
-- and returns its annihilator ideal.
polynomialAnnihilator = method()
polynomialAnnihilator RingElement := f -> (
    W := ring f;
    createDpairs W;
    dpV := W.dpairVars;
    -- error checking
    if W.monoid.Options.WeylAlgebra === {} then
    error "Expected element of a Weyl algebra";
    if substitute(f, (dpV#1 | dpV#2) / (i->(i=>0))) != f then
    error "Expected polynomial element of Weyl algebra";
    if W.monoid.Options.Degrees =!= toList(numgens W:{1}) then
    error "Expect all degrees in a Weyl algebra to be 1";
    tempL := (dpV#1 / (i -> 2*f*i - i*f));  -- (fD_i - df/dx_i)
    suffHigh := (degree f)#0 + 1;
    tempL = join(tempL, (dpV#1 / (i -> i^suffHigh)));  -- D_i^(large m)
    ideal tempL
    )

-- This routine takes a polynomial element f of the Weyl algebra
-- and returns the annihilator ideal of 1/f, or takes two polynomials
-- g and f and returns the annihilator ideal of g/f
rationalFunctionAnnihilator = method()
rationalFunctionAnnihilator RingElement := f -> rationalFunctionAnnihilator(1_(ring f), f)
rationalFunctionAnnihilator(RingElement, RingElement) := (g,f) -> (
    W := ring f;
    createDpairs W;
    dpV := W.dpairVars;
    -- error checking
    if W =!= ring g then
    error "Expected elements of the same ring";
    if W.monoid.Options.WeylAlgebra === {} then
    error "Expected element of a Weyl algebra";
    if substitute(f, (dpV#1 | dpV#2) / (i->(i=>0))) != f then
    error "Expected polynomial element of Weyl algebra";
    if substitute(g, (dpV#1 | dpV#2) / (i->(i=>0))) != g then
    error "Expected polynomial element of Weyl algebra";
    if W.monoid.Options.Degrees =!= toList(numgens W:{1}) then
    error "Expect all degrees in a Weyl algebra to be 1";

    -- get min root of b-function
    a := min getIntRoots globalBFunction f;

    IFs := AnnFs f;
    WFs := ring IFs;
    nFs := numgens WFs;
    Ia := substitute ( substitute(IFs, {WFs_(nFs-1) => a}), W);

    if a == -1 and g == 1_W then Ia
    else (
    	compensate := -1 - a;
    	F := map(W^1/Ia, W^1, matrix{{g*f^compensate}});
    	ideal mingens kernel F)
    )

----------------------------------------------------------------

TEST ///
-- TESTS TO WRITE (exported symbols);
--    AnnFs List
--    kDiffs
--    kOrderAnnFa
--    kOrderAnnFs
--    kappaAnnF1PlanarCurve

-- TESTS TO WRITE (unexported symbols);
--    diffRatFun (List, RingElement)
--    diffRatFun (List, RingElement, ZZ)
--    kCoeffVectorWRTs

----------
-- AnnFs
----------
----------------- TEST AnnFs -------------------
-- TEST: AnnFs RingElement
x = symbol x; z = symbol z; d = symbol d; Dz = symbol Dz;
R = QQ[x_1..x_4, z, d_1..d_4, Dz, WeylAlgebra => ( toList(1..4) / (i -> (x_i => d_i)) | {z => Dz} ) ]
f = x_1 + x_2 * z + x_3 * z^2 + x_4 * z^3

Ann = AnnFs f
R = ring Ann
s = R_(numgens R - 1)
assert ( Ann == ideal {
	  z * d_1 - d_2,
	  z * d_2 - d_3,
	  z * d_3 - d_4,
	  d_2^2 - d_1 * d_3,
	  d_3^2 - d_2 * d_4,
	  d_2 * d_3 - d_1 * d_4,
	  x_2 * d_1 + 2 * x_3 * d_2 + 3 * x_4 * d_3 - Dz,
	  x_2 * d_2 + 2 * x_3 * d_3 + 3 * x_4 * d_4 - z * Dz,
	  x_1 * d_1 - x_3 * d_3 - 2 * x_4 * d_4 + z * Dz - s,
	  3 * x_4 * z * d_4 - z^2 * Dz + x_2 * d_3 + 2 * x_3 * d_4
	  })
  
  
-- TEST: AnnFs List


----------------- TEST AnnIFs -------------------
x = symbol x; dx = symbol dx;
W = QQ[x,dx, WeylAlgebra=>{x=>dx}]
Ann = AnnIFs(ideal dx, x^2)
WS = ring Ann
assert( Ann == ideal (x*dx - 2*WS_2) )  


----------------- TEST diffRatFun -------------------
-- Test 1
x = symbol x, y = symbol y;
R = QQ[x,y];
f = 1/(x^4 + y);
ans = -24*x^3/(x^4+y)^4;
assert(diffRatFun({1,2}, f) == ans);
///
