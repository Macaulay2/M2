isInMultiplierIdeal = method(Options => {Strategy=>generalizedBFunction})
-- IN: g, an element of QQ[x]
--     I, an Ideal of QQ[x]
--     c, QQ
-- OUT: Boolean, is in multiplier ideal J(I^c)?     
isInMultiplierIdeal(RingElement, Ideal, QQ) := o -> (g,I,c) -> (
     roots := bFunctionRoots ( if o.Strategy===mGeneralizedBFunction then (
	       if c <= 0 then return true;
     	       l := lct I;
     	       m := ceiling(c+l); -- Need c < lct(I) + m
     	       if liftable(c+l,ZZ) then m = m + 1; 
     	       generalB(I_*, g, Exponent=>m)
	       -- When m=Exponent option is used in generalBFunction, than we need c<(lct(I)+m)
	       ) else if o.Strategy===generalizedBFunction then (
	       	    generalB(I_*, g)
	       ) else error "unknown Strategy"
	  ); 
     -c > max roots
     )

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

multiplierIdeal = method()
multiplierIdeal (Ideal, QQ) := (a,c) -> first multiplierIdeal(a,{c}) 
multiplierIdeal (Ideal, List) := (a,cs) -> (
     R := ring a;
     LCT := lct a;
     m := max{ceiling(max cs - LCT),1};
     if m == max cs - LCT then m = m + 1;

     -- star
     F := a_*;
     if #F == 0 then error "the list is empty";
     if #(options (ring first F).monoid)#WeylAlgebra == 0 -- not WA 
     then (
	  D := makeWeylAlgebra(ring first F,SetVariables=>false);
	  F = apply(F, f->sub(f,D));
	  );
     r := #F; 
     I0 := AnnFs F;
     DY := ring I0;
     K := coefficientRing DY;
     n := numgens DY // 2 - r; -- DY = k[x_1,...,x_n,t_1,...,t_r,dx_1,...,dx_n,dt_1,...,dt_r]
     w := toList(n:0) | toList(r:1);
     Istar := star(I0,-w|w);

     -- intersection part     	  
     SDY := K (monoid [s, gens DY, WeylAlgebra => DY.monoid.Options.WeylAlgebra]);
     v := gens SDY; 
     SX := take(v,{0,n}); notSX := drop(v,{0,n}); 
     T := take(v,{n+1,n+r}); dT := take(v,{2*n+r+1,2*(n+r)});
     SXring := K(monoid [SX,MonomialOrder=>Eliminate 1]);
     SDYtoSX := map(SXring,SDY, gens SXring | toList(n+2*r:0) );

     I2' := sub(Istar,SDY) + (sub(a, SDY))^m + ideal (SDY_0 + sum(r,i->dT#i*T#i)); -- Istar + a^m + (s-\sigma)
     I2 := SDYtoSX eliminateWA(I2', notSX);
     
     -- b-function part
     b'f1'm := generalB(a_*, 1_R, Exponent=>m);
     S := ring b'f1'm;
     
     apply(cs, c->(
	       b := b'f1'm;
     	       roots := select(bFunctionRoots b'f1'm, c'->-c'<=c); 
     	       for r in roots do (
     	       	    while b%(S_0-r)==0 do b = b//(S_0-r) 
     	       	    );
	       exceptionalLocusB(R,I2,b) -- ring I2 has to eliminate s 
	       ))
     )


jumpingCoefficients = method()
-- jumping numbers and multiplier ideals up to the analytic spead
jumpingCoefficients Ideal := I -> jumpingCoefficients(I,0, analyticSpread I)
-- jumping numbers and multiplier ideals in the interval [a,b]
jumpingCoefficients (Ideal, ZZ, ZZ) := (I,a,b) -> jumpingCoefficients(I,promote(a,QQ),promote(b,QQ))
jumpingCoefficients (Ideal, QQ, ZZ) := (I,a,b) -> jumpingCoefficients(I,promote(a,QQ),promote(b,QQ))
jumpingCoefficients (Ideal, ZZ, QQ) := (I,a,b) -> jumpingCoefficients(I,promote(a,QQ),promote(b,QQ))
jumpingCoefficients (Ideal, QQ, QQ) := (I,a,b) -> (
     -- candidates
     r := sort( bFunctionRoots generalB I_* / minus); 
     cs := (if min r>=a then {} else {last select(r, c->c<a)} ) | select(r, c->c<=b and c>=a);
     mI := multiplierIdeal(I,cs);
     jumps := toList select(0..#cs-1, i->i==0 or mI#i != mI#(i-1));
     ( jumps/(i->cs#i), jumps/(i->mI#i) )
     )

-- log canonical threshold computation via b-function
lct = method(Options => {Strategy=>GeneralBernsteinSato})
lct Ideal := RingElement => o -> I -> (
-- IN:  I,      ideal in QQ[x_1,...,x_n]
-- OUT: lct(I), an element of QQ
     W := makeWeylAlgebra ring I;
     F := (sub(I,W))_*;
     if o.Strategy === ViaBFunction then (
     	  w := toList (numgens ring I:0) | toList(#F:1); 
     	  b := bFunction(AnnFs F, w);
     	  S := ring b;
     	  r := numgens I;
     	  -- lct(I) = min root of b(s-r)
     	  b = sub(b, {S_0=>S_0-r})
	  )  
     else if o.Strategy === GeneralBernsteinSato then (
	  b = generalB(F,1_W);
	  S = ring b;
     	  b = sub(b, {S_0=>-S_0});
	  )
     else error "unknown Strategy";
     min bFunctionRoots b
     )   

hasRationalSing = method()
-- IN: F, a regular sequence in QQ[x] 
-- OUT: Boolean, has at most rational singularities?
hasRationalSing(List) := F ->(
     r := codim(ideal F);
     if r != #F then error "regular sequence expected";
     b := generalB F;
     S := (ring b)_0;
     b = sub(b, {S=>-S});
     bRoots := bFunctionRoots b;
     LCT := min bRoots;
     return ( 
	  LCT == r and LCT < min bFunctionRoots(b//(S-LCT)) 
	  ) 
     )
   
-- EXPERIMENTAL CODE BELOW --

isFsLocallyIntegrable = method()
isFsLocallyIntegrable (RingElement, QQ) := Boolean => (f,s) -> (
     R := ring f;
     -- makeWeylAlgebra R;
     n := numgens R;
     F := sub(f,RR_100(monoid [gens R]));
     num'steps := 100;
     num'samples := 1000;
     infinity'threshold := 1e20;
     epsilon := 1e-6;
     nbhd := 0.1^(n*first degree f); -- current size of the neighborhood
     shrink'factor := 10;
     integral := 0;
     for i from 1 to num'steps do (
     	  ave := 0; -- current average
	  next'nbhd := nbhd/shrink'factor;
     	  for j from 1 to num'samples do (
	       x := matrix{apply(n, i->random(-nbhd,nbhd))}; -- random point
	       if norm x > next'nbhd then ave = ave + abs(sub(F,x))^s;
	       );
     	  << "s = " << s << ", ave = " << (
	       d'integral=ave*(2*nbhd)^n/num'samples
	       ) << endl; 
	  if d'integral < integral * epsilon then return true;
	  nbhd = next'nbhd;
	  integral = integral + d'integral;
	  );
     false
     )
 
-- real log canonical threshold 
rlct = method()
rlct RingElement := RingElement => I -> (
-- IN:  f,  polynomial in QQ[x_1,...,x_n]
-- OUT: rlct(f), an element of QQ
     roots := reverse sort bFunctionRoots globalBFunction f;
     left := isFsLocallyIntegrable(f, roots#0 / 2);
     for i from 0 to #roots-2 do (
	  right := isFsLocallyIntegrable(f, (roots#i+roots#(i+1))/2);
	  print(left,right);
	  if left and not right then return -roots#i
	  else left = right;
	  );
     error "rlct not amongst the roots of the b-function, but it should be";
     )
