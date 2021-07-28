-- This file written by Amelia Taylor <ataylor@stolaf.edu>

needs "matrix1.m2"

----- This file was last updated on June 22, 2006

--------------------------------------------------------------
-- This begins the code for minimalPresentation which takes both ideals and 
-- quotient rings as input.   

-- checkpoly, finishmap, monOrder and coreProgram are called 
-- in the top-level program minimalPresentation.  

-*
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
     --               entries in the matrix to itself given by 
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
*-

reductorVariable = (f,excludes,onlyOnes) -> (
     -- inputs:
     --     f: a polynomial
     --     excludes: a (possibly empty) set of variables to NOT reduce
     --     onlyOnes: only return reductor if the lead coefficient is 1 or -1
     --       (e.g. needed if the coefficient ring is ZZ)
     -- output:
     --     a list of two elements {c,x}, where c*x appears as a term in f
     --     such that the variable x does not appear elsewhere in f.
     --     x will be a variable NOT in the excludes list.
     --     If no such term exists, {} is returned.
     -- assumption: all variables in the ring have degree 1.
     inf := part(1,f);
     restf := set support(f-inf);
     supInf := set support(inf);
     varList := toList (supInf-restf-excludes);
     -- either varList = {} and there are no linear terms whose
     -- variables don't occur elsewhere, otherwise we found such
     -- a linear term.
     if varList === {} then varList
     else (
     	  termf := terms f;
     	  s := select(termf, i -> member(leadMonomial i , varList));
     	  coef := s/leadCoefficient;
     	  pos := position(coef, i -> (i == 1) or (i == -1));
	  -- best to choose linear terms with coefficient 1 or -1 if
	  -- possible. 
     	  if pos =!= null then (coef_pos, leadMonomial s_pos)
     	  else if not onlyOnes then {coef_0, leadMonomial s_0}
          else {}
     	  )
     )
     
findReductor = (L,excludes,onlyOnes) -> (
     -- Inputs:
     --    L:List, of polynomials in a ring R
     --    excludes, a (possibly empty) set of variables
     --    onlyOnes: only return reductor if the lead coefficient is 1 or -1
     --       (e.g. needed if the coefficient ring is ZZ)
     -- Output:
     --    (x, x - 1/c f), where c*x is a term in f such that
     --      x does not appear in x - 1/c f, where f is some
     --      element of L.  x is restricted to be a variables not
     --      in the excludes set.
     --    null, if none exists.
     -- assumption: all variables in R have degree 1.
     L1 := sort apply(L, f -> (size f,f));
     redVar := {};
     L2 := select(1, L1, p -> (
	       redVar = reductorVariable(p#1, excludes, onlyOnes);
	       redVar =!= {})
	  );
     if redVar =!= {} then (redVar#1, redVar#1 - (1/(redVar#0))*L2#0#1)
     )

reduceLinears = method(Options => {Limit=>infinity})
reduceLinears(Ideal,Set) := o -> (I,excludes) -> (
     -- 1 argument: an ideal
     -- 1 optional argument: a limit on the recursion through the
     -- generators of the ideal.  
     -- Return: (J,L), where J is an ideal, and L is a list of:
     -- (variable x, poly x+g) where x+g is in I, and x doesn't appear
     -- in J and x does not appear in any polynomial later in the L list.
     R := ring I;
     onlyOnes := (coefficientRing R === ZZ); -- are there other cases when this is bad?
     L := flatten entries generators I;
     count := o.Limit;
     M := while count > 0 list (
       count = count - 1;
       g := findReductor(L, excludes, onlyOnes);
       if g === null then break;
--       << "---------------------------------" << endl;
--       << "reducing using " << g#0 << endl << endl;
--       << "  sending it to " << g#1 << endl << endl;
       F := map(R,R,{g#0 => g#1});
       L = apply(L, i -> F(i));
       g
       );
       -- Now loop through and improve M
     M = backSubstitute M;
--     << "------- backtracked ---------" << endl;
--     scan(M, g -> (
--       << "---------------------------------" << endl;
--       << "reducing using " << g#0 << endl << endl;
--       << "  sending it to " << g#1 << endl << endl;
--       ));
     (ideal matrix(R,{L}), M) -- same as (ideal L,M) except if L=={}
     )

backSubstitute = (M) -> (
     -- 1 argument: A list of pairs of variable and a polynomial. 
     -- Return: A list of pairs of the form (variable x, poly g)
     -- where x-g is in I, and x does not appear in any polynomial
     -- later. 
     --
     -- If M has length <= 1, then nothing needs to be done
     if #M <= 1 then M
     else (
     	  xs := set apply(M, i -> i#0);
     	  R := ring M#0#0;
     	  F := map(R,R, apply(M, g -> g#0 => g#1));
     	  H := new MutableHashTable from apply(M, g -> g#0 =>  g#1);
     	  scan(reverse M, g -> (
	       v := g#0;
	       restg := H#v;
	       badset := xs * set support restg;
	       while badset =!= set{} do (
		    restg = F(restg);
		    badset = xs * set support restg);
	       H#v = restg;
	       ));
         pairs H)
     )

minPressy = (I,excludes) -> (
     --  Returns: an ideal J in a polynomial ring over ZZ or a base field
     --  such that (ring I)/I is isomorphic to  (ring J)/J.
     --  Approach: look at generators of I, find those with linear
     --  terms that don't use the linear variables in any other
     --  terms. Then map that variable to remainder. Collect mapping
     --  information and cache in the ideal (ring in the case of the
     --  ring call).  
     --
     --  if the ring I is a tower, flatten it first, and if the ring has quotient
     -- elements, add them to flatI
     R := ring I;
     (flatI,F) := flattenRing I;
     flatR := ring flatI;
     (S,IS) := (flatR, flatI);
     if any(degrees flatR, d -> d =!= {1}) then (
     	  -- reset all the degrees to 1's, to use our reductor algorithm
	  S = newRing(flatR, Degrees=>{(numgens flatR : 1)});
	  IS = substitute(IS, vars S);
	  );
     excludes = set (generators S)_excludes;
     (J,H) := reduceLinears(IS,excludes);
     StoFlatR := map(flatR,S,vars flatR);
     J = ideal compress generators StoFlatR J; -- now in flatR
     H = apply(H, (a,b) -> (StoFlatR a, StoFlatR b)); -- everything in flatR now

     xs := set apply(H, index@@first); -- indices of the variables reduced out
     varskeep := sort (toList(set (generators S/index) - xs));
     newS := first selectVariables(varskeep, flatR);
     I.cache.minimalPresentationMapInv = map(R,newS,apply(varskeep, i -> R_i));

     vs := new HashTable from apply(#varskeep, i -> varskeep#i => i);
     trivialToNewS := map(newS, flatR, toList apply(numgens flatR, i -> if vs#?i then newS_(vs#i) else 0_newS));
     X := new MutableList from generators flatR; 
     scan(pairs vs, (v,i) -> X#v = newS_i);
     scan(H, (v,g) -> X#(index v) = trivialToNewS g);
     I.cache.minimalPresentationMap = map(newS, R, toList X);
     trivialToNewS J)

minPressyRing = (R, excludes) -> (
     -- 1 argument: A ring R (most often a quotient ring).
     -- Return:  A ring isomorphic to R using minimalPresentation on
     -- the presentation ideal of R.  For more information see
     -- minimalPresentation Ideal.
     I := ideal R;
     result := minPressy(I, excludes);
     finalRing := (ring result)/result;
     -- put the maps cached on I in the right place for the ring. 
     f := substitute(matrix I.cache.minimalPresentationMap, finalRing);
     fInv := substitute(matrix I.cache.minimalPresentationMapInv, R);
     R.minimalPresentationMap = map(finalRing, R, f);
     R.minimalPresentationMapInv = map (R, finalRing, fInv);
     finalRing
     )

checkExcludes = (R,excludes) -> (
     if instance(excludes,ZZ)
     then excludes = {excludes};
     if not instance(excludes,BasicList) 
     then error"expected an index or list of indices of variables in a ring";
     if any(excludes, e -> not instance(e,ZZ))
     then error "the Exclude list must consist of integer indices";
     )
minimalPresentation Ideal := prune Ideal := Ideal => opts -> (I) -> (
     checkExcludes(ring I,opts.Exclude);
     minPressy(I,opts.Exclude))

minimalPresentation Ring := prune Ring := Ring => opts -> (R) -> (
     checkExcludes(R,opts.Exclude);
     minPressyRing(R,opts.Exclude))

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
