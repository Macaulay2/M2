-- This file written by Amelia Taylor <ataylor@stolaf.edu>

----- This file was last updated on March 12, 2005

--------------------------------------------------------------
-- This begins the code for minPres which takes both ideals and 
-- quotient rings as input.   

-- checkpoly, finishmap, monOrder and coreProgram are called 
-- in the top-level program minPres.  

checkpoly := (f)->(
     -- 1 Argument:  A polynomial.
     -- Return:      A list of the index of the first 
     --              (by index in the ring) variable that occurs 
     --              linearly in f and does not occur in any other 
     --              term of f and a polynomial with that term 
     --              eliminated.
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
     -- 2 Arguments:  A matrix and a new mutable list.
     -- Return:       a map from the ring corresponding to 
     --               entries in the matix to itself given by 
     --               entries in the matrix which have a linear
     --               term that does not occur elsewhere in the 
     --               polynomial. 
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

monOrder := (Ord, l) -> (
     -- 2 arguments: a monomial order and a list of variables.
     -- return:  a new monomial order corresponding to the subset of 
     --          of variables in l.
     if (Ord === Lex or Ord === GRevLex or Ord === RevLex)
     then newOrd := Ord
     -- The order for the original ring is a product order.  
     -- Build a new product order based on variables remaining 
     -- for the minimal presentation.
     else (oldpieces := toList Ord;  
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
	  newOrd = ProductOrder newpieces;
	  );
     newOrd
     )   	            

monOrder = (Ord, l) -> GRevLex=>#l
     
coreProgram := (I, newvar) -> (
     -- 2 Arguments:  An ideal and a variable, or null.
     -- Return:       A list consisting of an ideal, a 
     --               quotient ring, a polynomial ring, 
     --               two matrices and a list of variables.
     -- Note:         The ideal is the ideal promised by 
     --               minPres ideal and the polynomial ring, 
     --               the ring for this idea.  The quotient 
     --               ring similar for minPres ring.  The 
     --               matrices set up the maps.
     R := ring I;
     F := finishMap(flatten entries generators I, new MutableList from first entries (vars R)); 
     -- The key computation of the polynomials with linear 
     -- terms is complete.  Now build desired rings, ideals 
     -- and maps through this map.
     LF := flatten entries F.matrix;
     l1 := apply(LF, f -> sum(exponents f));
     l := positions(l1,f -> if f===0 then false else (sum f) === 1);
     -- l contains the indices for the variables remaining in 
     -- the minimal presentation.  l is used to set new rings, 
     -- including getting the correct monomial order.
     varsR := apply(l,f->R_f);  
     degreesS := apply(l,i->((monoid R).degrees)#i);
     newMonOrder := monOrder((monoid R).Options.MonomialOrder, l);
     -- The two cases cover if the user does not or does (respectively
     -- give a new variable name for the minimal presentation ring.
     if newvar === null then (
	  S := (coefficientRing R)[varsR, Degrees => degreesS, MonomialOrder => newMonOrder];
	  vv := map(S,S);
	  newI := trim vv(substitute (ideal compress generators F(I), S));
	  S2 := S/newI;
	  FmatS := substitute(F.matrix, S);
	  FmatS2 := substitute(F.matrix, S2))
     else (
	  R2 := (coefficientRing R)[varsR];
	  y := newvar;
	  var := splice{y_0..y_(#l-1)};
	  S = (coefficientRing R)[var,Degrees => degreesS, MonomialOrder => newMonOrder];
	  vv = map(S, R2,vars S);
	  J := substitute (ideal compress generators F(I), R2);
	  newI = trim vv(J);
	  S2 = S/newI;
	  FmatS = vv(substitute(F.matrix,R2));
	  FmatS2 = substitute(FmatS, S2);
	  );
     (newI, S, S2, FmatS, FmatS2, varsR)	      	   	
     )


minPres = method(Options=>{Variable => null})

minPres Ideal := o -> (I) -> (
     --1 Argument: Any ideal in a polynomial ring.
     --Return:     An ideal J in a polynomial ring S such that 
     --            S/J is isomorphic with R/I. Maps from R to S 
     --            and S to R are encoded in I.cache.minPresMap
     --            and I.cache.minPresMapInv respectively.
     --Method:     Generators of I that are linear and occur 
     --            only once in that generator are removed.  
     --            This top level program calls coreProgram.
     --            coreProgram calls monOrder and finishMap.
     --            finishMap calls checkpoly.
     if I == 0 then I else (
	  S := coreProgram(I,o.Variable);
	  I.cache.minPresMap = map(S_1, ring I, S_3);
     	  I.cache.minPresMapInv = map(ring I, S_1, S_5);
	  S_0
     	  )
     )

minPres Ring := o -> (R) -> (
     -- 1 Argument: Any quotient of a polynomial ring R.
     -- Return:     An quotient ring R' = S'/J isomorphic to 
     --             R. Maps from R to R' and R' to R are 
     --             encoded in R.minPresMap and R.minPresMapInv
     --             respectively.
     --Method:      Write R as S/I, then generators of I that 
     --             are linear and occur only once in that 
     --             generator are removed.  This top level 
     --             program calls coreProgram. 
     --             coreProgram calls monOrder and finishMap.
     --             finishMap calls checkpoly.
     M := presentation R;
     S := coreProgram(ideal M, o.Variable);
     R.minPresMap = map(S_2,R,S_4); 
     R.minPresMapInv = map(R,S_2,S_5);
     S_1)
    
---------------------------
minPresMap = method()

minPresMap Ring := (R) -> (
     --Input:   A quotient ring.
     --Output:  A map from R to a ring isomorphic 
     --         to R but has a minimal presentation.
     finishMap(flatten entries presentation R, new MutableList from first entries (vars ideal presentation R)); 
          )

minPresMap Ideal := (I) -> ( 
     --Input:   An ideal.
     --Output:  A map from the ring of I, call it A, 
     --         to a ring with fewer variables, say R 
     --         such that A/I is isomorphic to R/minPres(I) 
     --         via the output of this function.
      I = minPres I;
      I.cache.minPresMap
      )
