-- This file written by Amelia Taylor <ataylor@stolaf.edu>

----- This file was last updated on January 16, 2005

--------------------------------------------------------------
-- This begins the code for minPres which takes both ideals and 
-- quotient rings as input.   

-- mytrim, checkpoly, finishmap are called in the main program below.  

mytrim := (I) -> (
     -- uses trim if I is homogeneous, sets up with a gb otherwise.
     if isHomogeneous I then (trim I)
     else (ideal generators gb I)
     )

checkpoly := (f)->(
     --input: a polynomial.
     --output: a list of the index of the first (by index in the ring) 
     --variable that occurs linearly in f and does not occur in any other
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
    
finishMap := (L,xmap) -> (
     -- 
     A := ring L_0;
     count := #L;
     while count > 0 do (
	  p := checkpoly(L_(count-1));
	  if p =!= {} then (
	       xmap#(p#0) = p#1;
	       F1 := map(A,A,toList xmap);
	       F2 := map(A,A, F1 (F1.matrix));
	       xmap = new MutableList from first entries F2.matrix;);
	  count = count-1
	  );
     map(A,A,toList xmap)
     )

minPres = method(Options=>{Variable => null})

minPres Ideal := o -> (I) -> (
     --Input:  Any ideal.
     --Output:  An ideal where the generators that have a 
     --         linear term where that variable does not occur 
     --         in other terms removed.  
     --Method:  We use minMapNew which calls finishMap, 
     --         dumpHighDegree, slice and getSingletons.
     R := ring I;
     if I == 0 then I else (
     	  F := finishMap(flatten entries generators I, new MutableList from first entries (vars R)); 
	  -- The core computation.  The rest is to set up maps for 
	  -- later use and to put the result in the correct ring. 
     	  LF := flatten entries F.matrix;
     	  l1 := apply(LF, f -> sum(exponents f));
     	  l := positions(l1,f -> if f===0 then false else (sum f) === 1);
	  -- l contains the indices for the remaining variables.  Needed to 
	  -- set the vars for the resulting ring as well as getting the correct 
	  -- degrees and the correct monomial order. 
	  varsR := apply(l,f->R_f);  
     	  degreesS := apply(l,i->((monoid R).degrees)#i);
	  oldMonOrder := (monoid R).Options.MonomialOrder;
	  if (oldMonOrder === Lex or oldMonOrder === GRevLex or oldMonOrder === RevLex)
	  then monOrder := oldMonOrder
	  -- The else puts together the Product Order when the original 
	  -- ideal is given in a produect order. 
	  else (oldpieces := toList oldMonOrder;  
	       newpieces := {};
	       count := 0;
	       done := #oldpieces;
	       m := l;
	       while done > 0 do (
		    sm := #(select(m, i -> i < oldpieces#0 + count));
		    newpieces = append(newpieces, sm);
		    if sm == 0 then m = m else m = drop(m, sm);
		    count = count + oldpieces#0;
		    oldpieces = drop(oldpieces, 1);
       	    	    done = #oldpieces;
		    );
	       newpieces = select(newpieces, i -> i != 0);
	       monOrder = ProductOrder newpieces;
	       );   	              	    
	  -- Next we set the new ring - call it S.  Uses new var names if desired and 
	  -- sets up a map from the old ring to the new in case new vars are used.
     	  if o.Variable === null then (
	       S := (coefficientRing R)[varsR, Degrees => degreesS, MonomialOrder => monOrder];
     	       FmatS := substitute(F.matrix, S);
	       vv := map(S,S);)
     	  else (
	       R2 := (coefficientRing R)[varsR2];
	       y := o.Variable;
     	       var := splice{y_0..y_(#l-1)};
     	       S = (coefficientRing R)[var,Degrees => degreesS, MonomialOrder => monOrder];
     	       vv = map(S,R2,vars S);
     	       J := substitute (ideal(compress generators F(I)),S);
     	       FmatS = vv(substitute(F.matrix,R2)););
     	  I.cache.minPresMap = map(S,R,FmatS);  
	  -- This is the key map from old ring R to new Ring S
     	  I.cache.minPresMapInv = map(R,S,varsR);
     	  mytrim vv(substitute (ideal compress generators F(I),S)) -- question the need for mytrim!
     	  )
     )

minPres Ring := o -> (R) -> (
     --Input:  Any ring R.
     --Output:  A ring R'that is the input simplified.  So
     --R'=k[a1..an]/J' where a1..an are a subset of the 
     --vars of R and R' is isomorphic to R.  The maps from 
     --R to R' and from R' to R are encoded on R' as 
     --R'.minPresMap and R'.minPresMapInv.
     --Method:  First we get a map from the presentation of R to 
     --itself using minMapNew which in turn calls checkpoly.
     M := presentation R;
     F := finishMap(flatten entries M, new MutableList from first entries (vars ring ideal M)); 
     LF := flatten entries F.matrix;    
     l1 := apply(LF, f -> sum(exponents f));
     l := positions(l1,f -> if f===0 then false else (sum f) === 1); 
     varsR := apply(l,f->(ring M)_f);
     degreesR := apply(l,i->((monoid R).degrees)#i);
     oldMonOrder := (monoid R).Options.MonomialOrder;
     if (oldMonOrder === Lex or oldMonOrder === GRevLex or oldMonOrder === RevLex)
     then monOrder := oldMonOrder
     -- The else puts together the Product Order when the original 
     -- ideal is given in a produect order. 
     else (oldpieces := toList oldMonOrder;  
	  newpieces := {};
	  count := 0;
	  done := #oldpieces;
	  m := l;
	  while done > 0 do (
	       sm := #(select(m, i -> i < oldpieces#0 + count));
	       newpieces = append(newpieces, sm);
	       if sm == 0 then m = m else m = drop(m, sm);
	       count = count + oldpieces#0;
	       oldpieces = drop(oldpieces, 1);
	       done = #oldpieces;
	       );
	  newpieces = select(newpieces, i -> i != 0);
	  monOrder = ProductOrder newpieces;
	  );   	              	 
     if o.Variable === null then(
     	  S := (coefficientRing R)[varsR, Degrees => degreesR, MonomialOrder => monOrder];
     	  I := ideal(compress F(M));
     	  S2 := S/(mytrim(substitute(I,S)));
	  FmatS := substitute(F.matrix, S2);
	  )
     else (R2 := (coefficientRing R)[varsR];
	  y := o.Variable;
     	  var := splice{y_0..y_(#l-1)};
     	  S = (coefficientRing R)[var,Degrees => degreesR, MonomialOrder => monOrder];
     	  vv := map(S,R2,vars S);
     	  I = substitute (ideal(compress F(M)),R2);
     	  S2 = S/(mytrim vv(I));
     	  FmatR2 := vv(substitute(F.matrix,R2));
	  FmatS = substitute(FmatR2, S2);
	  );
     R.minPresMap = map(S2,R,FmatS); 
     R.minPresMapInv = map(R,S2,varsR);
     S2)
    
---------------------------
minPresMap = method()

minPresMap Ring := (R) -> (
     --Input:  A quotient ring.
     --Output:  A map from R to a ring isomorphic to R but has a minimal
     --presentation.
     finishMap(flatten entries presentation R, new MutableList from first entries (vars ideal presentation R)); 
          )

minPresMap Ideal := (I) -> ( 
     --Input:  An ideal.
     --Output:  A map from the ring of I, call it A, to a ring with 
     --fewer variables, say R such that A/I is isomorphic to R/minPres(I) 
     --via the output of this function.
      I = minPres I;
      I.cache.minPresMap
      )
           
      
      

