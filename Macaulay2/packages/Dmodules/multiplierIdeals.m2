isInMultiplierIdeal = method()
-- IN: g, an element of QQ[x]
--     I, an Ideal of QQ[x]
--     c, QQ
-- OUT: Boolean, is in multiplier ideal J(I^c)?     
isInMultiplierIdeal(RingElement, Ideal, QQ) := (g,I,c) -> (
     if c <= 0 then return true;
     roots := bFunctionRoots generalB(I_*, g);
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
multiplierIdeal (Ideal, QQ) := (a,c)->multiplierIdeal(a,{c}) 
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
