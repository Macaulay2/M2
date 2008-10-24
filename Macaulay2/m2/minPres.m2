-- This file written by Amelia Taylor <ataylor@stolaf.edu>

----- This file was last updated on June 22, 2006

--------------------------------------------------------------
-- This begins the code for minimalPresentation which takes both ideals and 
-- quotient rings as input.   

-- checkpoly, finishmap, monOrder and coreProgram are called 
-- in the top-level program minimalPresentation.  

checkpoly = (f)->(
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

finishMap = (A,L,xmap) -> (
     -- 2 Arguments:  A matrix and a new mutable list.
     -- Return:       a map from the ring corresponding to 
     --               entries in the matix to itself given by 
     --               entries in the matrix which have a linear
     --               term that does not occur elsewhere in the 
     --               polynomial. 
     count := #L;
     while count > 0 do (
	  M := map(A,A,matrix{toList xmap});
	  p := checkpoly(M(L_(count-1)));     
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

coreProgram = (I, newvar) -> (
     -- 2 Arguments:  An ideal and a variable, or null.
     -- Return:       A list consisting of an ideal, a 
     --               quotient ring, a polynomial ring, 
     --               two matrices and a list of variables.
     -- Note:         The ideal is the ideal promised by 
     --               minimalPresentation ideal and the polynomial ring, 
     --               the ring for this idea.  The quotient 
     --               ring similar for minimalPresentation ring.  The 
     --               matrices set up the maps.
     R := ring I;
     F := finishMap(R,flatten entries generators I, new MutableList from first entries (vars R)); 
     -- The key computation of the polynomials with linear 
     -- terms is complete.  Now build desired rings, ideals 
     -- and maps through this map.
     LF := flatten entries F.matrix;
     l := toList select(0..#LF-1, i -> LF#i == R_i);
     --MES--l1 := apply(LF, f -> sum(exponents f));
     --MES--l := positions(l1,f -> if f===0 then false else (sum f) === 1);
     -- l contains the indices for the variables remaining in 
     -- the minimal presentation.  l is used to set new rings, 
     -- including getting the correct monomial order.
     varsR := apply(l,f->R_f);  
     degreesS := apply(l,i->((monoid R).degrees)#i);
     newMonOrder := monOrder((monoid R).Options.MonomialOrder, l);
     -- The two cases cover if the user does not or does (respectively
     -- give a new variable name for the minimal presentation ring).
     if newvar === null then (
	  S := (coefficientRing R)(monoid [varsR, Degrees => degreesS, MonomialOrder => newMonOrder]);
	  vv := map(S,S);
	  newI := trim vv(substitute (ideal compress generators F(I), S));
	  S2 := S/newI;
	  FmatS := substitute(F.matrix, S);
	  FmatS2 := substitute(F.matrix, S2))
     else (
	  R2 := (coefficientRing R)(monoid [varsR]);
	  y := newvar;
	  var := splice{y_0..y_(#l-1)};
	  S = (coefficientRing R)(monoid [var,Degrees => degreesS, MonomialOrder => newMonOrder]);
	  vv = map(S, R2,vars S);
	  J := substitute (ideal compress generators F(I), R2);
	  newI = trim vv(J);
	  S2 = S/newI;
	  FmatS = vv(substitute(F.matrix,R2));
	  FmatS2 = substitute(FmatS, S2);
	  );
     (newI, S, S2, FmatS, FmatS2, varsR)	      	   	
     )

minimalPresentation Ideal := o -> (I) -> (
     --1 Argument: Any ideal in a polynomial ring.
     --Return:     An ideal J in a polynomial ring S such that 
     --            S/J is isomorphic with R/I. Maps from R to S 
     --            and S to R are encoded in I.cache.minimalPresentationMap
     --            and I.cache.minimalPresentationMapInv respectively.
     --Method:     Generators of I that are linear and occur 
     --            only once in that generator are removed.  
     --            This top level program calls coreProgram.
     --            coreProgram calls monOrder and finishMap.
     --            finishMap calls checkpoly.
     if I == 0 then I else (
	  S := coreProgram(I,o.Variable);
	  I.cache.minimalPresentationMap = map(S_1, ring I, S_3);
     	  I.cache.minimalPresentationMapInv = map(ring I, S_1, S_5);
	  S_0
     	  )
     )

minimalPresentation Ring := o -> (R) -> (
     -- 1 Argument: Any quotient of a polynomial ring R.
     -- Return:     An quotient ring R' = S'/J isomorphic to 
     --             R. Maps from R to R' and R' to R are 
     --             encoded in R.minimalPresentationMap and R.minimalPresentationMapInv
     --             respectively.
     --Method:      Write R as S/I, then generators of I that 
     --             are linear and occur only once in that 
     --             generator are removed.  This top level 
     --             program calls coreProgram. 
     --             coreProgram calls monOrder and finishMap.
     --             finishMap calls checkpoly.
     M := presentation R;
     S := coreProgram(ideal M, o.Variable);
     R.minimalPresentationMap = map(S_2,R,S_4); 
     R.minimalPresentationMapInv = map(R,S_2,S_5);
     S_2)

---------------------------
-- minimalPresentationMap2 = method()

-- minimalPresentationMap2 Ring := (R) -> (
     --Input:   A quotient ring.
     --Output:  A map from the polynomial ring to itself 
     --         that is the map used to form a minimal 
     --         presentation of R.  
--     finishMap(R,flatten entries presentation R, new MutableList from first entries (generators ideal presentation R))
--          )

--minimalPresentationMap2 Ideal := (I) -> ( 
     --Input:   An ideal.
     --Output:  A map from the ring of I, call it A, 
     --         to itself, that is the map used to form a minimal 
     --         presentation of R.  
--     finishMap(R,flatten entries generators I, new MutableList from first entries (generators I))
--     )


isReductor = (f) -> (
     inf := leadTerm f;
     part(1,f) != 0 and
     (set support(inf) * set support(f - inf)) === set{})

findReductor = (L) -> (
     L1 := select(L, isReductor);
     L2 := sort apply(L1, f -> (size f,f));
     if #L2 > 0 then L2#0#1)

reduceIdeal = (L) -> (
     L1 := select(L, isReductor);
     L2 := sort apply(L1, f -> (size f,f));
     if #L2 > 0 then (
	  g := L2#0#1;
	  << "reducing with " << g << endl << endl;
	  L = apply(L, f -> f % g))
     else (
	  print "cannot reduce ideal further";
	  L))

reduceLinears = method(Options => {Limit=>infinity})
reduceLinears Ideal := o -> (I) -> (
     -- returns (J,L), where J is an ideal,
     -- and L is a list of: (variable x, poly x+g)
     -- where x+g is in I, and x doesn't appear in J.
     -- also x doesn't appear in any poly later in the L list
     R := ring I;
     -- make sure that R is a polynomial ring, no quotients
     S := (coefficientRing R)[generators R, Weights=>{numgens R:-1}, Global=>false];
     IS := substitute(I,S);
     L := flatten entries generators IS;
     count := o.Limit;
     M := while count > 0 list (
       count = count - 1;
       g := findReductor L;
       if g === null then break;
       ing := leadTerm g;
       << "reducing using " << ing << endl << endl;
       L = apply(L, f -> f % g);
       (substitute(leadTerm g, R), substitute(-g+leadTerm g,R))
       );
     (substitute(ideal L,R), M)
     )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
