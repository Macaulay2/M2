eliminateWA = method()
eliminateWA (Ideal, List) := Ideal => (I,v) -> (
     R := ring I;
     if not all(v,g->isSubset(set {g},set gens R) ) then error "expected generators of the ring";
     w := apply(gens R, g->if isSubset(set{g}, set v) then 1 else 0); 
     W := (coefficientRing R)(monoid [gens R, WeylAlgebra => R.monoid.Options.WeylAlgebra,  MonomialOrder=>{Weights=>w}]);
     G := flatten entries gens gb sub(I,W);
     sub(ideal select(G, f->all(listForm f, m->sum(numgens W, i->m#0#i*w#i) ==0)),R)
     ) 

computeJf = method()
computeJf RingElement := Ideal => f -> (
     if #(options (ring f).monoid)#WeylAlgebra > 0 -- isWA
     then (
	  D := ring f;
	  )
     else ( 
     	  R = ring f;
	  D = makeWA(R,SetVariables=>false);
     	  f = sub(f,D);
	  );
     -- assume D = k<x_1..x_n,dx_1..dx_n>
     n := numgens D//2;
     K := coefficientRing D;
     	            
     w := toList(n:0) | {1};
     inIf := inw(AnnFs {f}, -w|w);
     
     Dt := ring inIf;
     x := take(gens Dt,{0,n-1});
     dx := take(gens Dt,{n+1,n+n});
     t := Dt_n; dt := Dt_(2*n+1);

     I1 := eliminateWA(inIf,dx);
     I2 := ideal apply(I1_*, g->(
	       d := first degree g;
	       if d>0 then t^d*g
	       else dt^(-d)*g
	       ));
     Rtdts := K (monoid [x,t,dt,symbol s, WeylAlgebra=>{t=>dt}]); 
     s := Rtdts_(n+2);
     Rs := K (monoid [s,x,Degrees=>{1}|toList(n:0),MonomialOrder=>Eliminate 1]);
     I3a := eliminateWA(
	  sub(I2,Rtdts) + ideal(
	       s + Rtdts_(n+1)*Rtdts_(n) -- s+dt*t
	       ), 
	  {Rtdts_(n),Rtdts_(n+1)} -- {t,dt}
	  );
     /// I3b := eliminate(
	  sub(I2,Rtdts) + ideal(
	       s + Rtdts_(n+1)*Rtdts_(n) -- s+dt*t
	       ), 
	  {Rtdts_(n),Rtdts_(n+1)} -- {t,dt}
	  );
     if I3b != I3a then error "eliminate is wrong!";
     ///;
     sub(I3a,Rs)
     )

exceptionalLocusB = method(Options => {Strategy => Syzygies})

-- find an algebraic set where b is not a multiple of the local b-function of f
exceptionalLocusB (RingElement,RingElement) := RingElement => o -> (f,b) -> (
     if #(options (ring f).monoid)#WeylAlgebra > 0 -- isWA
     then (
	  D := ring f;
	  R := null; 
	  )
     else ( 
     	  R = ring f;
	  D = makeWA(R,SetVariables=>false);
     	  f = sub(f,D);
	  );
     I3 := computeJf f;
     exceptionalLocusB(R,I3,b,o)
     )

-- this version performs the computation given (ring f, I3, b)
exceptionalLocusB (Ring,Ideal,RingElement) := RingElement => o -> (R,I3,b) -> (
     Rs := ring I3;
     K := coefficientRing Rs;
     s := Rs_0;
     n := numgens Rs - 1;
     x := take(gens Rs,{1,n});
     -- make everything live in Rs
     b = (map(Rs,ring b,{(ring b)_0=>s})) b;
     --
     if R === null then R = K (monoid [x]);
     --
     if o.Strategy === Syzygies then (
	  time gbI3 := gb I3; -- eliminate s
     	  exceptionalLocusB(R,gbI3,b,o)	       	  
	  )
     else if o.Strategy === ColonIdeal then -- eliminate s from I3:b    
     time sub(ideal selectInSubring(1, gens gb (I3 : b)),R)
     else error "unknown Strategy"
     ) 

exceptionalLocusB (Ring,GroebnerBasis,RingElement) := RingElement => o -> (R,gbI3,b) -> (
     Rs := ring gbI3;
     s := Rs_0;
     b = (map(Rs,ring b,{(ring b)_0=>s})) b;
     G3 := flatten entries gens gbI3; 
     reducedB := b%gbI3;
     d := first degree reducedB; 
     if d < 0 then ideal 1_R     
     else (
	  -- (1) 
	  -- return time sub(ideal selectInSubring(1, gens gb (ideal select(G3,g->degree(s,g)<=d) : reducedB)),R);
	  -- (2)
     	  syz1 := syz(matrix{{reducedB}|select(G3,g->degree(s,g)<=d)}, SyzygyRows=>1);		       	    
	  retI := time sub(ideal first entries selectInSubring(1, gens gb syz1),R);
	  if retI == 0 then error "zero"
	  else return retI;
     	  --(3)
	  toRVector := p->apply(d + 1, i->sum(select(listForm p, t->t#0#0 == i), t->t#1*R_(drop(t#0,1)))); 
	  G3up2d := flatten apply(G3,g -> apply(d-degree(s,g)+1, i->g*s^i)); -- take all GB elems and their multiples up to degree d 
	  M := apply(G3up2d, toRVector);
	  time quotient(image transpose matrix M, R*(vector toRVector reducedB)) 
	  --M := apply({reducedB} | G3up2d, toRVector);
	  --time ideal first entries syz transpose matrix M
	  )
     )

localBFunctionStrata = method()
localBFunctionStrata RingElement := HashTable => f -> ( 
     if #(options (ring f).monoid)#WeylAlgebra > 0 -- isWA
     then error "Weyl algebra not expected"
     else ( 
     	  R := ring f;
	  D := makeWA(R,SetVariables=>false);
     	  f = sub(f,D);
	  );

     I3 := computeJf f;
     gbI3 := gb I3;

     -- compute global b-function and its roots    
     gB := globalBFunction f;
     S := ring gB; s = S_0;
     roots := bFunctionRoots gB;
     
     strata := new MutableHashTable;
     for r in roots do (
     	  b := gB;
     	  while b%(s-r) == 0 do (
	       b = b//(s-r);
	       print factorBFunction b; 
	       myLocus := exceptionalLocusB(R,gbI3,b);
	       print mingens myLocus;
	       myLocus2 := exceptionalLocusB(R,I3,b,Strategy=>ColonIdeal);
	       print mingens myLocus2;
     	       assert(
     	       	    myLocus
     	       	    == 
     	       	    myLocus2
     	       	    );
	       strata#b = myLocus;
	       )
     	  );
     strata
     )

localBFunction = method(Options => {Strategy => Syzygies})
localBFunction (RingElement,Ideal) := HashTable => o -> (f,P) -> (
     if ring f =!= ring P then error "same ring for polynomial and ideal expected";
     R := ring f;
     D := makeWA(R,SetVariables=>false);
     f = sub(f,D);

     I3 := computeJf f;
     gbI3 := gb I3;

     -- compute global b-function and its roots    
     gB := globalBFunction f;
     S := ring gB; s = S_0;
     roots := bFunctionRoots gB;

     b = gB;
     for r in roots do (
     	  while b%(s-r) == 0 do (
	       b' := b//(s-r);
	       print factorBFunction b'; 
	       locus := exceptionalLocusB(R,gbI3,b',Strategy=>o.Strategy);
     	       if isSubset(locus,P) -- if point P is in the locus
	       then break
	       else b = b';     	       
	       )
     	  );
     b       
     )