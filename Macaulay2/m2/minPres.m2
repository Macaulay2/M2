-- This file written by Amelia Taylor <ataylor@math.rutgers.edu>


--------------------------------------------------------------
--This begins the code for minPres and minPresIdeal.  
--These 3 functions are called in that code.
mytrim := (I) -> (
     if isHomogeneous I then (trim I)
     else (ideal gens gb I)
     )

checkpoly:=(f)->(
     --input a polynomial.
     --output a list of the index of the first (by index in the ring) 
     --variable that occurs linearlly in f and does not occur in any other
     --term of f and a polynomial with that term eliminated.
     A := ring(f);
     p := first entries contract(vars A,f);
     i := position(p, g -> g != 0 and first degree g === 0);
     if i === null then
         {}
     else (
     	  v := A_i;
     	  c := f_v;
     	  {i,(-1)*(c^(-1)*(f-c*v))}
	  )
     )
minMap := (I) -> (
     --Input any ideal.
     --Return a map:  the ring of I to itself.  
     --If there are generators of I that have linear terms where 
     --the variable is not used in any other term in that generator
     --then that variable is mapped to the negative of that generator
     --minus the linear term.  
     A := ring I;              
     xmap := new MutableList from gens A;	
     M := first entries gens I;
     tried := new MutableList from splice {#M : NOTCHECKED};
     findnext := () -> (
     	  p := {};
     	  next := 0;
     	  done := false;
	  while next < #M and not done do (
	       if tried#next === NOTCHECKED then (
	       	    p = checkpoly(M#next);
	       	    tried#next = CHECKED;
	       	    if p =!= {} then
		         done = true
	       	    else next=next+1;
	       	    )
	       else
	            next=next+1
	  );
          p);
     p := findnext();
     while p =!= {} do (
	  xmap#(p#0) = p#1;
	  F1 := map(A,A,toList xmap);
	  F2 := map(A,A, F1 (F1.matrix));
     	  xmap = new MutableList from first entries F2.matrix;
	  M = apply(#M, i -> (
		    f := M#i;
		    g := F2 f;
		    if g != 0 and g != f then tried#i = NOTCHECKED;
		    g));
	  p = findnext();
	  );
     map(A,A,toList xmap))

minPresIdeal = method(Options=>{Variable => null})
minPresIdeal(Ideal) := o -> (I) -> (
     --Input:  Any ideal.
     --Output:  An ideal where the generators that have a linear term where 
     --that variable does not occur in other terms removed.  
     --Method:  We use minMap which calls checkpoly.
     R := ring I;
     F := minMap(I);
     LF := flatten entries F.matrix;
     l := positions(LF,f->(sum(sum exponents f))===1);
     varsR := apply(l,f->R_f);
     degreesS := apply(l,i->((monoid R).degrees)#i);
     if o.Variable === null then (
	  S := (coefficientRing R)[varsR,Degrees => degreesS];
     	  FmatS := substitute(F.matrix,S);
	  vv := map(S,S);)
     else (
	  R2 := (coefficientRing R)[varsR2];
	  y := o.Variable;
     	  var := splice{y_0..y_(#l-1)};
     	  S = (coefficientRing R)[var,Degrees => degreesS];
     	  vv = map(S,R2,vars S);
     	  J := substitute (ideal(compress gens F(I)),S);
     	  FmatS = vv(substitute(F.matrix,R2)););
     I.cache.minPresMap = map(S,R,FmatS);
     I.cache.minPresMapInv = map(R,S,varsR);
     mytrim vv(substitute (ideal compress gens F(I),S))
     )

minPres = method(Options=>{Variable => null})
minPres(Ring) := o -> (R) -> (
     --Input:  Any ring R.
     --Output:  A ring R'that is the input simplified.  So
     --R'=k[a1..an]/J' where a1..an are a subset of the 
     --vars of R and R' is isomorphic to R.  The maps from 
     --R to R' and from R' to R are encoded on R' as 
     --R'.minPresMap and R'.minPresMapInv.
     --Method:  First we get a map from the presentation of R to 
     --itself using minMap which in turn calls checkpoly.
     M := presentation R;
     F := minMap(ideal M);
     LF := flatten entries F.matrix;
     l := positions(LF,f->(sum(sum exponents f))===1);
     varsR := apply(l,f->R_f);
     degreesR := apply(l,i->((monoid R).degrees)#i);
     if o.Variable === null then(
     	  S := (coefficientRing R)[varsR,Degrees => degreesR];
     	  I := ideal(compress F(M));
     	  IS := mytrim(substitute(I,S));
     	  S2 := S/IS;
     	  FmatS := substitute(F.matrix,S2);
     	  R.minPresMap = map(S2,R,FmatS); 
     	  R.minPresMapInv = map(R,S2,varsR);
     	  S2)
     else (R2 := (coefficientRing R)[varsR];
	  y := o.Variable;
     	  var := splice{y_0..y_(#l-1)};
     	  S = (coefficientRing R)[var,Degrees => degreesR];
     	  vv := map(S,R2,vars S);
     	  I = substitute (ideal(compress F(M)),R2);
     	  IS = mytrim vv(I);
     	  S2 = S/IS;
     	  FmatR2 := vv(substitute(F.matrix,R2));
     	  FmatS = substitute(FmatR2,S2);
     	  R.minPresMap = map(S2,R,FmatS);
     	  R.minPresMapInv = map (R,S2,varsR);
     	  S2)
     )
     
minPresMap=method()
minPresMap(Ring):=(R)->(
     --Input:  A quotient ring.
     --Output:  A map from R to a ring isomorphic to R but has a minimal
     --presentation.
     I:=ideal(presentation R);
     minMap(I)
          )

minPresMapIdeal = method()
minPresMapIdeal(Ideal):=(I)->( 
     --Input:  An ideal.
     --Output:  A map from the ring of I, call it A, to a ring with 
     --fewer variables, say R such that A/I is isomorphic to R/minPres(I) 
     --via the output of this function.
      minMap(I)
           )

