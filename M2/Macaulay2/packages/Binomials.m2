-- -*- coding: utf-8 -*-
--  Binomials.m2
--
--  Copyright (C) 2009 Thomas Kahle <kahle@mis.mpg.de>
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or (at
--  your option) any later version.
--
--  This program is distributed in the hope that it will be useful, but
--  WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  General Public License for more details.
--
--  You should have received a copy of the GNU General Public License along
--  with this program; if not, write to the Free Software Foundation, Inc.,
--  59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newPackage(
	"Binomials",
    	Version => "0.5", 
    	Date => "June 2009",
    	Authors => {
	     {Name => "Thomas Kahle", Email => "kahle@mis.mpg.de", HomePage => "http://personal-homepages.mis.mpg.de/kahle/bpd"}},
    	Headline => "Spezialized routines for binomial Ideals",
	Configuration => { },
    	DebuggingMode => true
    	)
   
export {
     -- 'Official' functions
     binomialPrimaryDecomposition,
     binomialCellularDecomposition,
     binomialRadical,
     binomialMinimalPrimes,
     binomialAssociatedPrimes,
     binomialSolve,
     -- tests
     binomialIsPrime,
     binomialIsPrimary,
     isCellular,
     isBinomial,
     isPureDifference,
     -- input related
     makeBinomial,
     latticeBasisIdeal,
     -- cellular stuff:
     cellularBinomialAssociatedPrimes,
     -- cellularAssociatedLattices,
     cellularBinomialPrimaryDecomposition,
     cellularBinomialRadical,
     -- simple wrappers:
     BPD,
     BCD,
--     BCDisPrimary,
     -- auxillary functions:
     partialCharacter,
     -- Not in the interface:
--     axisSaturate,
--     cellVars,
--     Lsat,
--     idealFromCharacter,
--     saturatePChar,
--     satIdeals,
--     nonCellstdm,
--     maxNonCellstdm,
--     minimalPrimaryComponent,
--     binomialQuasiPower,
--     binomialQuotient,
--     projectToSubRing,
--     removeRedundant,

     -- Removed as of M2 v1.2
--     lcm,

     -- Options
     cellVariables, -- for partialCharacter
     returnPrimes, -- for binomialIsPrimary 
     returnPChars, -- for binomialIsPrimary
     returnCellVars, -- for binomialCellularDecomposition
     verbose -- produce more output
     }

needsPackage "FourTiTwo";
needsPackage "Cyclotomic"

axisSaturate = (I,i) -> (
-- By Ignacio Ojeda and Mike Stillman
-- For computing saturations w.r.t. a single variable:
-- Comments by TK:
    R := ring I;
    I1 := ideal(1_R);
    s := 0;
    f := R_i;
    while not(I1 == I) do (
	s = s + 1;
	I1 = I;
	-- This should be just the quotient. Is this faster ??
	I = ideal syz gb(matrix{{f}}|gens I,
            SyzygyRows=>1,Syzygies=>true););
    {s-1, I}
    )

-- Cellular decomposition of binomial ideals:
binomialCellularDecomposition = method (Options => {returnCellVars => false, verbose=>true})
binomialCellularDecomposition Ideal := Ideal => o -> I -> (
-- By Ignacio Ojeda and Mike Stillman     
-- Comments by TK
     R := ring I;
     n := numgens R;
     Answer := {};
     L := null;
     IntersectAnswer := ideal(1_R);
     ToDo := {{{1_R},toList(0..n-1),I}};
     -- Each entry of the ToDoList is a triple:
     -- #0 contains list of variables with respect to which is already saturated
     -- #1 contains variables to be considered for cell variables
     -- #2 is the ideal to decompose
     compo := 0;
     next := () -> (
	 if #ToDo === 0 then false
	 else (
	      L = ToDo#0;
	      ToDo = drop(ToDo,1);
	      if gens IntersectAnswer % L#2 == 0 then (
		   if o#verbose then (
			<< "redundant component" << endl;
			)
		   )
	      -- if its not redundant:
	      else if #(L#1) === 0 then ( -- #(L#1) counts 'remaining variables to check'
		   -- no variables remain to check :
		   -- We have an answer
                   compo = compo + 1; 
		   newone := trim L#2;
		   if o#verbose then (
			<< "cellular components found: " << compo << endl;);
		   if o#returnCellVars then Answer = append(Answer,{newone, delete(1_R,L#0)})
		   else Answer = append (Answer,newone);
		   IntersectAnswer = intersect(IntersectAnswer,newone);
		   -- if we have enough, stop after this iteration
		   if IntersectAnswer == I then ToDo = {})
	      else ( -- So, there are remaining variables #(L#1) is not zero
	           i := L#1#0; -- i is a variable under consideration
		   newL1 = drop(L#1,1); -- gets removed from the list
	           result = axisSaturate(L#2, i); -- compute saturation wrt i
		   J := result#1; -- Ideal
		   k := result#0; -- Saturation Exponent
		   if k > 0 then ( -- If a division was needed:
     	       	    	-- We add the monomial i^k to ideal under consideration		      	   	
			J2 = L#2 + ideal(R_i^k); 
     	       	    	-- And remove L#0 components from variables that we already
			-- considered before
			J2 = saturate(J2, product L#0);
			if J2 != ideal(1_R) then
			    -- If something is left after removing we have more to decompose J2
			    ToDo = prepend({L#0, newL1, J2},ToDo));
		       -- Continue with the next variable and add i to L#0
		   if J != ideal(1_R) then ToDo = prepend({L#0|{R_i}, newL1, J},ToDo);
		   );
	      true));
     while next() do ();
     Answer	      
     )

-- This function saturates an integer lattice. It expects 
-- the matrix A, whose image is the lattice. 
Lsat = A -> syz transpose syz transpose A;

isCellular = method (Options => {returnCellVars => false})
isCellular Ideal := Ideal => o -> I -> (
     R := ring I;
     cv := cellVars I;
     if cv == {} then prod := 1_R else prod = product cv;
     if I == saturate (I, prod) then (
	  -- Cellular Ideal
	  if o#returnCellVars then return cv
	  else return true;
	  )
     else return false;
     )

partialCharacter = method (Options => {cellVariables => null})
partialCharacter Ideal := Ideal => o -> I -> (
     -- Will compute the partial character associated to a cellular binomial Ideal.
     -- If the cell variables are known they can be given as an optional argument,
     -- to save cpu cycles.
     
     vs := {}; -- This will hold the lattice generators
     vsmat := matrix "0"; -- Holds the matrix whose image is L 
     cl := {}; -- This will hold the coefficients
     R := ring I;
     CoeffR := coefficientRing R; -- needed to form terms below
     scan (gens R, (v -> v = local v));
     II := ideal;
     
     -- print o.cellVariables;
     -- The input should be a cellular ideal 
     cv := null; -- Getting a local name
     if o#cellVariables === null then (
	  -- No cell variables are given -> compute them
	  cv = cellVars I;
	  )
     else cv = o#cellVariables;
     
     -- The cell ring:
     S := CoeffR[cv];
         
     -- If there are no cellular variables, 
     -- the ideal is monomial and the partial character is zero:
     if cv == {} then (
	  return ({}, matrix "0", {1});
	  );

     -- We intersect I with the ring k[E]
     -- In many cases this will be zero
     if #cv != #(gens R) then (
     	  II = kernel map (R/I,S);
	  )
     else (
	  II = I;
	  );

     -- The partial Character of the zero ideal is the 
     -- zero lattice.       
     if ( II == 0 ) then (
	  for i in cv do vs = vs | { 0_ZZ };
	  cl = {1_ZZ};
	  return (cv, transpose matrix {vs}, cl);
	  );
     
     -- So, II is not zero:
     -- Let ts be the list of generators
     ts := entries gens II;
     -- print ts;
     -- for each term, find the exponent vector
     oldmat := matrix "0";
     oldvs := {};
     for t in ts#0 do (
	  -- Want to check if we already have this generator included
	  
	  -- Save the old values
	  oldmat = vsmat;
	  oldvs = vs;
	  	  
	  -- compute new ones
	  -- print t;
	  -- print  {((exponents (t))#0 - (exponents (t))#1)};
	  vs = vs | {((exponents t)#0 - (exponents t)#1)};
	  -- print vs;
	  vsmat = transpose matrix vs;
	  
	  -- Do we need the new generator ?
	  if image oldmat == image vsmat then (
	       -- Undo changes:
	       vsmat = oldmat;
	       vs = oldvs;
	       )
	  else (
	       -- So we have a new generator : update coefficient list
	       coeffs := entries ((coefficients(t))#1);
	       F := coefficientRing ring coeffs#1#0;
	       coe := for c in coeffs list lift(c#0,F);
               cl = cl | { sub ( -coe#1 / coe#0, CoeffR) };
	       );
	  );
--    print coeffs;
--    print cl;
     
     -- back to the old ring
     -- is this needed ?
     use R;
     return (cv, transpose matrix vs , cl);
     )

isBinomial = I -> (
     ge := flatten entries gens I;
     for g in ge do (
          if #(terms g) > 2 then return false;	  
	  );
     return true;
     )

isPureDifference = I -> (
     ge := flatten entries gens I;
     for g in ge do (
	  coeffs := sort flatten entries (coefficients g)#1;
     	  if coeffs == {1} then continue;
	  if coeffs == { -1} then continue;
	  if coeffs == { -1 , 1} then continue;
	  return false;
	  );
     return true;
     )
     
cellVars = I -> (
     -- print "Warning, cellVars called, could be unessary";
     cv := {};
     for i in gens ring I do if saturate (I,i) != substitute(ideal(1), ring I) then cv=cv|{i};
     return cv;
     )

nonCellstdm = {cellVariables=>null} >> o -> I -> (
     cv2 := {};
     if o#cellVariables === null then (
	  print "CellVariables not given, Please consider precomputing them";
	  cv2 = cellVars I;
	  )
     else cv2 = o#cellVariables;
     -- Extracts the monomials in the non-Cell variables.
     R := ring I;
     scan (gens R, (v -> v = local v));     
     cv := set cv2; 
     -- Here go the non-cell variables
     ncv := toList (set gens R - cv);
     -- We map I to the subring: k[ncv]
     CoeffR := coefficientRing R;
     S := CoeffR[ncv];
     J := kernel map (R/I,S); -- image of I in the subring S
     Q = S/J; 
     slist = flatten entries flatten basis Q;
     use R;
     return slist;
     )

maxNonCellstdm = {cellVariables=>null} >> o -> I -> (
     -- Extracts the maximal elements in the set of monomials 
     cv := {};
     if o#cellVariables === null then (
	  print "CellVariables not given, Please consider precomputing them";
	  cv = cellVars I;
	  )
     else cv = o#cellVariables;

     nm := nonCellstdm (I,cellVariables=>cv);
     -- print nm;
     result := {};
     maxel := 0;
     while nm != {} do (
     	  maxel = max nm;
     
          -- Add maxel to the result
      	  result = result | {maxel};

          -- Delete everyone who is dividing maxel     
     	  nm = for m in nm list (if maxel // m != 0 then continue; m);
     );

     return result;
     )

makeBinomial = (R,m,c) -> (
     -- constructs the binomial associated to 
     -- exponent vector m and coefficient c in R
     var := gens R;
     posmon :=1;
     negmon :=1;
     for i in 0..#m-1 do (
     	  if m#i > 0 then (
		    posmon = posmon * var#i^(m#i)
		    )
	       else (
		    negmon = negmon * var#i^(-m#i)
		    );
	       );	  
     return posmon - c*negmon;
     )

idealFromCharacter = (R,A,c) -> (
     -- Constructs the Ideal I_+(c) in R
     -- R is a ring in which the ideal is returned
     -- The columns of A should contain exponent vectors of generators
     -- The vector c contains the corresponding coefficients which must lie
     -- in the coefficient ring of R !!!
     
     use R;
     var := gens R;
     if A == 0 then return ideal 0_R;
     cols := null;
     binomials :=null;
     
     idmat := matrix mutableIdentity(ZZ,#var);
     if A == idmat then (
	  -- If A is the unit matrix we are lucky,
	  -- no saturation is needed.

	  -- We coerce the coefficients to R:
	  c = apply (c, a -> (sub (a,R)));
     	  cols = entries transpose A;    
     	  binomials = for i in 0..numcols A-1 list makeBinomial (R,cols#i, c#i);	  
	  return ideal binomials
	  )
     else if set c === set {1} then (
	  -- all coefficients are one, we can use 4ti2.
	  return toricMarkov (transpose A, R, InputType => "lattice");
	  )
     else (
     	  -- The general case, fall back to saturation in M2:
	  c = apply (c, a -> (sub (a,R)));
     	  cols = entries transpose A;    
	--  for i in 0..numcols A-1 do print (R,cols#i,c#i);
     	  binomials = for i in 0..numcols A-1 list makeBinomial (R,cols#i, c#i);
     	  return saturate (ideal binomials, product var);
	  );
     )

latticeBasisIdeal = (R,L) -> (
     -- Constructs the lattice basis ideal (whose saturation is the toric ideal)
     -- Convention is that L's columns generate the lattice.
     use R;
     var := gens R;
     if L == 0 then return ideal 0_R;
     cols := null;
     binomials :=null;
     cols = entries transpose L;
     binomials = for i in 0..numcols L-1 list makeBinomial (R,cols#i, 1);
     return ideal binomials;
     )

saturatePChar = (va, A, c) -> (
     -- This function saturates a partial character.
     
     -- Currently a saturated character is distinguished from its 
     -- saturation as the saturation has a list as third entry.
     
     -- If the lattice is saturated, the character is saturated     
     if image Lsat A == image A then (
	  return (va, A, {c});
	  );
     
     -- The saturated lattice
     S := Lsat(A);
     -- The coefficient matrix :
     K := A // S;
     
     -- print K;
     -- Now we find the (binomal) equations for the saturated character:
     numvars := numrows K;
     varlist := for i in 0..numvars-1 list value ("m"|i);
     scan (varlist, (v -> v = local v));
     Q := QQ[varlist];
     eqs := idealFromCharacter(Q,K,c);
     
     -- print "The character defining equations are:";
     -- print eqs;
     -- print ring eqs;
     
     result = binomialSolve eqs;
--     print "In saturatePChar the result is";
--     print result;
     return (va, S, result);
     )

satIdeals = (va, A, d) -> (
     -- Computes all the ideals belonging to saturations of  
     -- a given partial character.
     satpc := saturatePChar(va, A, d);
     scan (satpc#0, (v -> v = local v));     
     -- The following should be the smallest ring containing all new
     -- coefficients but not smaller than QQ
     F := ring satpc#2#0#0;
     if F === ZZ then F = QQ;
     Q := F[satpc#0];
     satideals = apply (satpc#2 , (c) -> (
	       -- print {Q, satpc#1, c};
	       idealFromCharacter(Q,satpc#1,c)));
     return satideals;
     )

binomialRadical = I -> (
     	  cv := isCellular (I, returnCellVars=>true);
     	  if not cv === false then (
	       return cellularBinomialRadical (I,cellVariables=>cv)
	       )
      	  else (
	  -- In the general case
	  print "Input not cellular, computing minimial primes ...";
	  mp := binomialMinimalPrimes I;
	  -- print mp;
	  return ideal mingens intersect mp;
	  )
     )

cellularBinomialRadical = method (Options => {cellVariables => null}) 
cellularBinomialRadical Ideal := Ideal => o -> I -> (
     cv := {};
     if o#cellVariables === null then (
	  print "CellVariables not given, Please consider precomputing them";
	  cv = cellVars I;
	  )
     else cv = o#cellVariables;
     
     -- Computes the radical of a cellular binomial ideal
     R := ring I;
     scan (gens R, (v -> v = local v));
     -- Get the partial character of I
     pc := partialCharacter(I, cellVariables=>cv);
     noncellvars := toList(set (gens R) - pc#0);
     	       
     M := sub (ideal (noncellvars),R);
     
     -- We intersect I with the ring k[E]
     -- In many cases this will be zero
     -- The the radical missing the monomials:
     prerad := projectToSubRing (I,pc#0);
     return prerad + M;
     )

binomialIsPrimary = method (Options => {returnPrimes => false , returnPChars => false, cellVariables=> null})
binomialIsPrimary Ideal := Ideal => o -> I -> (
     -- Implements Alg. 9.4 in [ES96]
     -- I must be a cellular ideal, cellVariables can be given for speedup
     -- Returns the radical of I and whether I is primary
     -- if the option returnPrimes is true, then it will return 
     -- the radical in the affirmative case and two distinct associated primes
     -- otherwise
     -- if the option returnPChars is true then it will return partial Characters 
     -- of the primes instead. 
     -- If both are true then it will return characters.
     
     cv := {};
     if o#cellVariables === null then (
	  print "CellVariables not given, Please consider precomputing them";
	  cv = cellVars I;
	  )
     else cv = o#cellVariables;
     -- The ring of I :
     R := ring I;
     scan (gens R, (v -> v = local v));
     
     -- Only proper ideals are considered primary
     if I == ideal(1_R) then return false;      
     
     -- Get the partial character of I
     pc := partialCharacter(I, cellVariables=>cv);
     noncellvars := toList(set (gens R) - cv);
     
     M := sub (ideal (noncellvars),R);
     -- print ("The monomial ideal M: " | toString M);
     
     -- We intersect I with the ring k[E]
     -- The the radical missing the monomials:
     prerad := projectToSubRing (I,cv);
     
     rad := prerad + M;
     
     -- If the partial character is not saturated, the radical is not prime
     if image Lsat pc#1 != image pc#1 then (
	  print "The radical is not prime, as the character is not saturated";
	  satpc := saturatePChar pc;
	  if o#returnPChars then (
	       -- This one is the fastest, so check it first
	       return {{satpc#0,satpc#1,satpc#2#0}, {satpc#0,satpc#1,satpc#2#1}}
	       );
	  if o#returnPrimes then (
     	       F := ring satpc#2#0#0;
     	       S := F[satpc#0];
	       M = sub(M,S);
	       ap1 := idealFromCharacter (S,satpc#1,satpc#2#0) + M;
	       ap2 := idealFromCharacter (S,satpc#1,satpc#2#1) + M;
	       -- Return two distinct associated primes:
	       use R;
	       return {ap1,ap2};
     	       )	   	       
	  else return false;
	  );
     
     -- If the radical is prime, then there still might be embedded
     -- primes that properly contain the radical. The remaining part
     -- finds such primes by examining quotients w.r.t (maximal)
     -- standard monomials. 
     
     -- The list of maximally standard monomials:
     maxstdmon := maxNonCellstdm (I,cellVariables=>cv) / (i -> sub (i,R));
     -- print "The maximally standard monomials are:";
     -- print maxstdmon;
     
     for m in maxstdmon do (
	  q := quotient (I, m);
	  -- Mapping down to cellvars:
	  q2 := projectToSubRing (q,cv);
     	  -- I_+(sigma) was called prerad above:
	  if not isSubset(q2, prerad) then (
	       -- creating some local names:
	       qchar := partialCharacter (q,cellVariables=>cv);
	       satqchar := saturatePChar qchar;
	       if o#returnPChars then(
		    return {pc, {satqchar#0,satqchar#1,satqchar#2#0}}
		    );
	       if o#returnPrimes then (
		    F := ring satqchar#2#0#0;
     	       	    S := F[satqchar#0];
	       	    M = sub(M,S);
		    ap2 := idealFromCharacter (S,satqchar#1,satqchar#2#0);
		    use R;
		    return {rad, ap2 + M};
     	       	    )  
	       else return false;
	       );
	  );
     -- print "Ideal is primary";
     use R;
     if o#returnPChars then return {pc};
     if o#returnPrimes then return {rad};
     return true;	  
     )

binomialIsPrime = method (Options => {cellVariables=>null})
binomialIsPrime Ideal := Ideal => o -> I -> ( 
     -- A binomial ideal is prime if it is cellular and all its
     -- monomial generators have power one and the associated partial
     -- character is saturated.  (Corollary 2.6 in ES96 )

     -- Input: A Binomial Ideal and the cell variables if the ideal is cellular.
     -- Output: true if the ideal is a prime ideal, false otherwise
     cv := {};
     if o#cellVariables === null then (
	  cv = isCellular (I, returnCellVars=>true);
	  -- if not cellular then not prime
	  if cv === false then return false;
	  )
     -- if cell variables are given we believe that I is cellular:
     else cv = o#cellVariables;
     R := ring I;
     if I == ideal (1_R) then return false;
     pc := partialCharacter (I,cellVariables=>cv);
     ncv := toList(set (gens R) - cv);
     for v in ncv do (
	  if not isSubset(ideal (v) , I) then return false;
     	  );

     -- Is the partial character saturated ???     
     if image Lsat pc#1 != image pc#1 then return false;
     
     -- all tests passed:
     return true;
     )

binomialMinimalPrimes = method (Options => {verbose=>true})
binomialMinimalPrimes Ideal := Ideal => o -> I -> (
     -- The new algorithm, based on reduced celldecomp
     R := ring I;
     ge := gens R;
     n := numgens R;
     Answer := {};
     L := null;
     IntersectAnswer := ideal(1_R);
     ToDo := {{{1_R},toList(0..n-1),I}};
     compo := 0;
     next := () -> (
	 if #ToDo === 0 then false
	 else (
	      L = ToDo#0;
	      ToDo = drop(ToDo,1);
	      if gens IntersectAnswer % L#2 == 0 then (
		   if o#verbose then (
			<< "redundant component" << endl;
			);
		   )
	      else if #(L#1) === 0 then ( -- #(L#1) counts 'remaining variables to check'
                   compo = compo + 1; 		
		   newone := trim L#2;
		   if o#verbose then (
			<< "components found so far: " << compo << endl;);
		   Answer = append(Answer,{newone, delete(1_R,L#0)});
		   IntersectAnswer = intersect(IntersectAnswer,newone);
		   if IntersectAnswer == I then ToDo = {})
	      else ( -- So, there are remaining variables #(L#1) is not zero
	           i := L#1#0; -- i is a variable under consideration
		   newL1 = drop(L#1,1); -- gets removed from the list
	           result = axisSaturate(L#2, i); -- compute saturation wrt i
		   J := result#1; -- Ideal
		   k := result#0; -- Saturation Exponent
		   if k > 0 then ( -- If a division was needed:
			J2 = L#2 + ideal(R_i); 
			J2 = saturate(J2, product L#0);
			if J2 != ideal(1_R) then
			    ToDo = prepend({L#0, newL1, J2},ToDo));
		   if J != ideal(1_R) then (
			ToDo = prepend({L#0|{R_i}, newL1, J},ToDo);
			);
		   );
	      true));
     while next() do ();
     -- print Answer;
     
     if o#verbose then print "Decomposition done.";
          
     ncv := {};
     i := 0;
     j := #Answer;
     ME :=ideal; pc = {}; si := ideal; mp := {}; F := null; S:= null;
     for a in Answer do (
	  i = i+1;
	  print ("Finding minimal primes of cellular component: " | toString i | " of " | toString j);
	  ME := ideal(toList(set (gens R) - a#1));
	  pc := partialCharacter (a#0, cellVariables=>a#1);
	  -- Check whether we have a radical ideal already:
	  if image Lsat pc#1 == image pc#1 then (
	       si = {a#0};
	       )
	  else (
	       si = satIdeals pc
	       );
	  F = coefficientRing ring si#0;
	  if F === QQ then S = R else S = F[ge];
	  ME = sub (ME, S);
	  si = for i in si list sub(i,S);
	  si = si / (i -> trim (i + ME)); -- Adding monomials;
	  mp = mp | si;
	  );

     mp = joinCyclotomic mp;
     use R;
     return mp;
     );

removeEmbedded = l -> (
     -- Computes the minimal primes from a list of primes.  
     
     -- Algorithm: Copy the input list, then walk through the input
     -- list and remove from the copy of every element which contains the
     -- element at hand.
     
     ToDo := copy l;
     i := ideal;
     su := {};
     while #ToDo > 0 do (
--	  print ToDo;
--	  print l;
	  i = ToDo#0;
	  su = for i2 in l list (if (isSubset (i,i2)) and (i!=i2) then i2);
	  
     	  -- Remove any occurrences of redundant primes from l 
	  -- and the todolist;
	  for s in su do (
	       ToDo = delete (s, ToDo);
	       l = delete (s, l);
	       );
	  -- Remove i from the todolist;
	  ToDo = delete (i, ToDo);
	  );
     return l;
     )

cellularBinomialAssociatedPrimes = method (Options => {cellVariables => null}) 
cellularBinomialAssociatedPrimes Ideal := Ideal => o -> I -> ( 
     -- Computes the associated primes of cellular binomial ideal
     
     -- TODO: It could be faster by rearringing things in the m in ml
     
     R := ring I;
     scan (gens R, (v -> v = local v));
     
     cv := null;
     if o#cellVariables === null then (
	  -- No cell variables are given -> compute them
	  cv = cellVars(I);
	  )
     else cv = o#cellVariables;
     
     primes := {}; -- This will hold the list of primes
     ncv := toList(set (gens R) - cv); -- non-cell variables x \notin E
     ml := nonCellstdm(I,cellVariables=>cv); -- List of std monomials in ncv
     -- Coercing to R:
     ml = ml / ( m -> sub (m,R) );
     -- Mapping to the ring k[E]:
     prerad := projectToSubRing (I,cv);
     M := sub (ideal (ncv),R); 
     -- The monomial radical ideal 
     
     -- Here is a nice shortcut: if prerad is zero, we are done since
     -- all I:m will be zero after intesection with the cell ring, right?
     if prerad == ideal (0_R) then return {M};
     -- A dummy ideal and partial Characters:
     Im := ideal;
     pC := {}; sat = {};
     for m in ml do (
	  -- print m;
	  Im = projectToSubRing (I:m,cv);
	  -- Do we already know the cell variables in the following computation?
	  pC = partialCharacter(Im , cellVariables=>cv);
	  if pC#1 == 0 then (
	       primes = primes | {ideal(0_R)}; 
	       continue;
	       );
	  if image Lsat pC#1 == image pC#1 then (
	       sat = {Im};
	       )
	  else (
	       sat = satIdeals pC
	       );
	  primes = primes | sat;
	  );
     -- We need to remove redundant elements and join all associated primes in an apropriate new ring that contains all
     -- their coefficients.
        
     primes = joinCyclotomic(primes);
     M = sub (ideal ncv, ring primes#0);
     primes = primes / (I -> I + M);

     use R;
     return toList set primes;
     )

binomialAssociatedPrimes = I -> (
     if not isBinomial I then error "Input not binomial";

     ap := {};
     if isCellular I then return cellularBinomialAssociatedPrimes I
     else (
	  cd := binomialCellularDecomposition (I, returnCellVars => true);
     	  counter := 1;
     	  cdc := #cd;
     	  scan (cd , ( (i) -> (
	   	    print ("Computing associated primes of cellular component: " | toString counter | " of " | toString cdc);
		    counter = counter +1;
		    ap = ap | cellularBinomialAssociatedPrimes (i#0, cellVariables => i#1);
		    )
	       )
    	  ); -- apply
	  );
     
     return joinCyclotomic ap;
     )

cellularAssociatedLattices = I -> (
     -- Computes the associated lattices of a cellular binomial ideal
     -- Todo: Can we get the multiplicities too ?
     
     R := ring I;
     scan (gens R, (v -> v = local v));
     cv := cellVars I; -- cell variables E
     lats := {}; -- This will hold the list of lattices
     ncv := toList(set (gens R) - cv); -- non-cell variables x \notin E
     -- print "Noncellvars"; print ncv;
     ml := nonCellstdm(I,cellVariables=>cv); -- List of std monomials in ncv
     -- Coercing to R:
     ml = ml / ( m -> sub (m,R) );
     -- The ring k[E]:
     prerad := projectToSubRing (I,cv);
     -- A dummy ideal and partial Characters:
     Im := ideal;
     pc := {};
     redundant := true;
     -- For each monomial, check if I:m has a different lattice !
     for m in ml do (
	  -- print m;
	  Im = projectToSubring ((I:m),cv);
	  -- We already know the cell variables in the following computation
	  pc = partialCharacter(Im, cellVariables=>cv);
	  if #lats == 0 then (
	       lats = {pc#1};
	       continue;
	       )
	  else (
	       redundant = false;
	       scan (lats, (l -> (if image l == image pc#1 then redundant = true)))
     	       );
	  if redundant then continue
	  else (
	       lats = lats | {pc#1};
	       );
      	  ); -- for m in ml	    
     return {cv, lats};
     ) -- CellularAssociatedLattices

BCDisPrimary = I -> (
     print "Computing Cellular Decomposition";
     cd := binomialCellularDecomposition I;
     print "Testing for primaryness of components";
     i := 0;
     for c in cd do (
	  i = i+1;
	  print ("Component number " | i );
	  if binomialIsPrimary c == true then continue;
	  print "Following component is not primary: ";
	  print c;
	  return false;
	  );
     print "The cellular decomposition is primary !";
     return cd;
     )

minimalPrimaryComponent = method (Options => {cellVariables => null})
minimalPrimaryComponent Ideal := Ideal => o -> I -> (
     -- Input a cellular binomial ideal whose radical is prime.
     -- Ouptut, generators for Hull(I)

     cv := null;
     if o#cellVariables === null then (
	  -- No cell variables are given -> compute them
	  cv = cellVars(I);
	  )
     else cv = o#cellVariables;

     apc := binomialIsPrimary (I, returnPChars=>true, cellVariables => cv);
     if #apc == 1 then return I -- radical is only associated prime!
     else (
	  R := ring I;
	  -- A trick to not clobber the global variables
	  scan (gens R, (v -> v = local v));
	  
     	  pc1 := apc#0;
	  pc2 := apc#1;
	 
	  -- ap#0 and ap#1 correspond to 
	  -- distinct lattices L1 and L2
	  L1 := image pc1#1;
	  L2 := image pc2#1;

	  L := intersect {L1,L2};
	  -- The index of L inside L2 is finite if and only if their dimensions coincide
	  if rank L == rank L2 then (
	       print "finite index case !";
	       -- The finite index case :  
	       
	       -- Compute a binomial in J2 which is not in J1.
	       -- i.e. find a generator on which pc1 and pc2 take different values.
	       print pc1;
	       print pc2;
	       for i in 0..#(pc2#2)-1 do (
	       	    if pc1#2#i == pc2#2#i then continue
	       	    else (
		    	 -- Character differs. Form binomial:
		    	 b := makeBinomial (QQ[pc2#0], (entries transpose pc2#1)#i, pc2#2#i );
		    	 print b;
		    	 break;
		    	 );
	       	    );
	       -- Take the quotient of I with respect to b, such that the result is binomial
	       return minimalPrimaryComponent (binomialQuotient (I,b, cellVariables=>cv), cellVariables=>cv);
	       )
       	   else (
		-- print "infinite index case !";
		-- The case of infinite index :
		    
                -- Find an exponent vector which has infinite order:
		-- i.e. a vector m \in L2 such that image m \cap L has dimension < 1;
		-- One of the generators must have this property !
     		    
		 -- Here are the lattice generators
		 L2cols := entries transpose pc2#1;
		 -- print L2cols;
		 -- Try them one by one:
		 i := 0; -- Counter to determine which generator fits
		 for c in L2cols do (
		      -- The span of c:
		      imc := image transpose matrix {c};
		      if rank intersect {imc , L} < 1 then (
			   -- We have winner 
			   m := c;
			   break;
			   );
		      -- Lets try the next vector.
		      i = i+1;
		      );
		 -- print i;
     	         -- now i has the suitable index !
		 b = makeBinomial(QQ[pc2#0], L2cols#i, pc2#2#i);		    
		 -- print b;
	    	 -- Take the quotient of I with respect to b, such that the result is binomial
	    	 return minimalPrimaryComponent (binomialQuotient (I,b, cellVariables=>cv), cellVariables=>cv);
	    	 );
	    ) -- else path of if not binomialIsPrimary
     ) -- minimalPrimaryComponent

binomialQuotient = {cellVariables => null} >> o -> (I,b) -> (
     -- Algorithm A.3 in Ojeda / Sanchez
     -- Input I - Cellular Binomial Ideal 
     -- b -- Binomial in the cell variables of I which is a zerodivisor mod I
     -- Output : The quotient (I : b^[e]) for a suitably choosen e, such that the
     -- result is binomial
     
     R := ring I;
     scan (gens R, (v -> v = local v));
     
     cv := null;
     if o#cellVariables === null then (
	  -- No cell variables are given -> compute them
	  cv = cellVars(I);
	  )
     else cv = o#cellVariables;
     
     --First check if we can save a lot of time if already I:b is binomial,
     -- and no quasipowers have to be taken.
     quot :=  quotient (I , sub(ideal(b),R));
     if isBinomial quot then return quot;
          
     --Transporting the standardmonomials to R:
     ncvm := (i -> sub (i,R) ) \ nonCellstdm (I,cellVariables=>cv) ;
     -- print ncvm;
  
     U' := {}; -- U' as in the paper
     D  := {};
     J := ideal (0_R); -- initialize with zero ideal
     bexp := (exponents b)#0 - (exponents b)#1; -- exponent vector of b
     -- We will often need the image of bexp, so lets cache it
     bexpim := image transpose matrix {bexp};
     pc := {}; -- Will hold partial characters;
     -- CoeffR := coefficientRing R;
     -- S := CoeffR[cv]; -- k[\delta] in the paper
     
     for m in ncvm do(
	  quot = I:m;
	  	  
	  -- Mapping to k[delta] and taking character
	  quot = projectToSubRing (quot, cv);
	  pc = partialCharacter (quot, cellVariables=>cv);
	  
	  --determine whether the exponents of b are in the saturated lattice
	  if isSubset (bexpim, image Lsat pc#1) then (
     	       U' = U' | {m};
	       i := 1;
	       -- Computing the order of bexp in Lsat / L
	       while true do (
		    if isSubset (image transpose matrix {i * bexp} , image pc#1) then (
			 D = D | {i};
			 break;
			 )
		    else i = i+1;
		    );
	       print ("The order of " | toString bexp | "in " | toString image pc#1 | "is " | toString i);
	       -- print D;
	       );
	  ); -- loop over monomials
     -- Compute the least common multiple of the orders
     e := lcm D; -- e' in paper, but we dont need e later.
     print ("binomialQuasiPower" | toString (b,e));
     bqp := sub (binomialQuasiPower (b,e) , R); -- e'th quasipower
     print bqp;
     print ring bqp;
     print ( "Least common multiple : " | toString e);
     for m in U' do(
	  quot = quotient (I,m);
	  if bqp % quot == 0 then J = J + ideal(m);		
     	  );
     print J;
     return I + J;
     )     

--lcm = l -> (
--     if #l == 0 then return 1;
--     sublcm := lcm delete (l#0,l);
--     return l#0 * sublcm / gcd (l#0, sublcm);
--     )

binomialQuasiPower = (b,e) -> (
     -- returns the e-th quasipower of the binomial b
     -- i.e. (b_1)^e - (b_2)^e
     return ((terms b)#0)^e - (- (terms b)#1)^e;
     )

BCD = I -> binomialCellularDecomposition I 
BPD = I -> binomialPrimaryDecomposition I

binomialPrimaryDecomposition = I -> (
     -- The full binomial primary decomposition 
     -- starting from a not necessarily cellular binomial ideal
     
     if not isBinomial I then error "Input was not binomial !";
     
     print "Running cellular decomposition:";
     cd := binomialCellularDecomposition (I, returnCellVars => true);
     counter := 1;
     cdc := #cd;
     bpd := {};
     scan (cd , ( (i) -> (
	   	    print ("Decomposing cellular component: " | toString counter | " of " | toString cdc);
		    counter = counter +1;
		    bpd = bpd | cellularBinomialPrimaryDecomposition (i#0, cellVariables => i#1);
		    print "done";
		    )
	       )
    	  ); -- apply
     -- print bpd;
     
     bpd = joinCyclotomic bpd;
     print "Removing redundant components...";
     return removeRedundant bpd;
     )

cellularBinomialPrimaryDecomposition = method (Options => {cellVariables => null}) 
cellularBinomialPrimaryDecomposition Ideal := Ideal => o -> I -> ( 
     -- computes the binomial primary decomposition of a cellular ideal
     -- I needs to be cellular. Cell variables can be given to speed up
     -- Implements algorithm 9.7 in ES96, respectively A5 in OS97
     ap := {};
     cv := null;
     if o#cellVariables === null then cv = cellVars I
     else cv = o#cellVariables;
     ap = cellularBinomialAssociatedPrimes (I, cellVariables => cv);
     -- Projecting down the assoc. primes, removing monomials
     proj := (I) -> projectToSubRing (I,cv); 
     pap := ap / proj ;
     R := ring ap#0; -- All associated primes live in a common ring
     J := sub (I,R); -- get I over there to compute sums
     -- Compute and return minimal primary Components. We can reuse
     -- the cell Variables here by a Proposition in the writeup
     return pap / ( (P) -> minimalPrimaryComponent (J + P, cellVariables=>cv));
     )

removeRedundant = l -> (
     -- Removes redundant components from a list of ideals to be intersected
     -- TODO: The ideals given might live in different rings, which are 
     -- mathematically the same. We should handle this in a nice way.
     if #l == 0 then error "empty list given !";
     Answer := l#0; -- Will hold Intersection of everything in the end
     result := {l#0};
     l = drop (l,1); -- Drop l#0;
     isect := ideal; -- dummy 
     while #l > 0 do (
	  isect = intersect (Answer , l#0); -- intersect with next
	  -- if something was happenening, add l#0 to the result
	  if isect != Answer then (
	       result = result | {l#0};
	       Answer = isect;
	       -- print l#0;
	       )
	  else print "redundant component found !";
	  -- shorten the todolist
	  l = drop (l,1);
	  );
     return ideal \ mingens \ result;
     )

projectToSubRing = (I , delta) -> (
     -- projects an ideal down to the ring k[\delta] where delta is a
     -- the set of variables. Return after substituting back to the
     -- original ring !!
     R := ring I;
     scan (gens R, (v -> v = local v));
     CoeffR := coefficientRing R;
     S := CoeffR[delta];
     return sub(kernel map (R/I,S), R);
     )


-- The remaining code implements the solver for zero-dim'l pure
-- difference binomial ideals . We solve pure difference binomial
-- equations using modulo 1 arithmetics. The basic task is to solve
-- a^n = 1^{k/m}, whose solutions are the equivalence classes of:
-- k/nm, 1/n + k/m, 2/n + k/nm,... , (n-1)/n + k/nm
          
-- The following function implements this:
Rooter = (n,q) -> (
     -- INPUT:
     -- n an integer
     -- q a rational number between zero and one representing k/m
     -- OUTPUT:
     -- The list of root-exponents:
     -- k/nm, 1/n + k/m, 2/n + k/nm,... , (n-1)/n + k/nm
     k := 0/1;
     m := 1;
     if q != 0 then (
	  m = denominator sub(q,QQ);
	  k = numerator sub(q,QQ);
	  );
     val := 0;
     roots := for i in 0..n-1 list (
	  val = i/n + k/(n*m);
	  if val > 1 then val = val - floor val;
	  val
	  );
     return roots;
     );

SolveMore = (binom,psol) -> (
     -- This function extends a partial solution further
     -- INPUT: A partial solution and a binomial which after plugging
     -- in the partial solutions is univariate
     -- OUTPUT: An extended partial solution

     -- Since Lex is a global order the true monomial comes first, right ?
     mon := (terms binom)#0; -- The monomial in the new variable.
     
     -- we need the index of the variable that we will solve now
     -- <incomprehensable hack>
     ind := index (flatten entries gens radical monomialIdeal mon)#0;
     var := (flatten entries gens radical monomialIdeal mon)#0;
     -- </incomprehensable hack>
     
     rhs := (terms binom)#1; -- The right hand side which is a power
			     -- of a root of unity
			     
     -- TODO: Here we should check the sign 			     
     erhs := flatten exponents rhs;
     
     newsols := {}; -- A list accumulating extended solutions
      
     -- If the binomial contains a common variable in both of its
     -- monomials then zero is a solution for this variable We are
     -- looking at erhs at position ind to determine this case
     
     roots := {};
     
     rhsvarpow := erhs#ind;
     
     if rhsvarpow > 0 then (
	  -- zero is a solution for variable ind
	  -- We fork of a new solution with entry "n" and divide by 
	  -- the offending variable
	  roots = {"n"};
     	  mon = lift(mon / var^rhsvarpow, ring mon);
	  rhs = lift(rhs / var^rhsvarpow, ring rhs);
	  erhs = flatten exponents rhs;
      	  );

     emon := flatten exponents mon;
     -- one element list containing the exponent
     n := (toList (set emon - set {0}))#0; 

      -- This needs to be done for each entry in psol:
      for onesol in psol do (
	   roots = {};
	   -- now determine the right hand side exponent from the
	   -- partial solutions.
	   zeroflag := false;    
       	   q := for v in 0..#erhs-1 list (
		-- First case: variable does not appear -> exponent 0
		if (erhs#v == 0) then 0
		else if onesol#v === "n" then (
		     -- if erhs > 0 and onesol has a "n", then the rhs is zero!
     	       	     zeroflag = true;
		     break 
		     )
		-- otherwise exponent times old exponent
		
		else erhs#v * onesol#v
		);
	   
	   if zeroflag and #roots == 0 then (
		roots = roots | {"n"};
		)
	   else (
		if not zeroflag then q = sum q;
		);

       	   -- now everthing is set for the Rooter:
       	   roots = roots | Rooter (n,q);
       	   extensions := for r in roots list (
	    	for i in 0..#onesol-1 list if i==ind then r else onesol#i
	    	);
       	   newsols = newsols | extensions;
       	   );
      
      return newsols;	  
      )

binomialSolve = I -> (
     -- A ready to use solver for zero-dim'l pure difference Binomial
     -- Ideals INPUT: I, the ideal    
     -- OUTPUT: The list of solutions in QQ(some root of unity)
     -- Note: Solutions will be returned with multiplicities.
     if not isPureDifference I then (
	  error "Sorry, only implemented for pure difference binomial ideals";
	  );
	  
     R := ring I;
     cd := binomialCellularDecomposition (I,returnCellVars=>true,verbose=>false);
     exponentsols := flatten for c in cd list cellularBinomialExponentSolve (c#0,c#1);

     -- determine the least common denominator, ignoring nulls
     denoms := for i in flatten exponentsols list if i =!= null then denominator i else continue;
     -- If there are no denominators, the ideal was monomial
     -- and we return only (0,0,...,0) many times:
     if denoms === {} then (
	  zerosol:={for i in gens R list 0_R};
	  return for i in 1..#exponentsols list zerosol;
	  );
     lcd := lcm denoms;

     -- This is our standard. Coefficients are rational?
     C := QQ;     
     if lcd > 2 then (
	  -- print "Adjoining roots of unity is needed";
     	  C = cyclotomicField lcd;
	  );
     
     expo := q -> (
     -- This auxiallary function maps a quotient from QQ to its
     -- element in S
     if q === null then return 0_C;
     if q == 0 or q == 1 then return 1_C;
     if q == (1/2) then return -1_C;
     k := numerator sub(q,QQ);
     m := denominator sub(q,QQ);
     if m != lcd then k = k * sub(lcd / m,ZZ);
     return sub((C_0)^k,C);
     );
     
     sols := flatten exponentsols;
     sols = expo \ sols;
     sols = pack (#(gens ring I),sols);

     if lcd > 2 then ( 
	  print ("BinomialSolve created a cyclotomic field of order " | toString lcd ); 
	  );
    
     return sols; 
     )

cellularBinomialExponentSolve = (I,cv) -> (
     -- Solves a zero dimensional cellular pure difference binomial
     -- ideal by constructing the apropriate cyclotomic field 
     
     -- Input: a pure difference zero-dim'l binomial ideal and its list
     -- of cell variables
     
     -- Output: A list of solutions of the ideal 
     	  
     R := ring I;
     varlist := flatten entries vars R;
     RLex := newRing(R,MonomialOrder => Lex);
     if not dim I == 0 then error "Ideal to solve is not zero dimensional";
     
     -- First we need a Lex Groebner Basis of our ideal.     
     groeb := flatten entries gens gb sub(I,RLex);
     
--     print "This is the Groebner basis. Is it ordered correctly ??";
--     print groeb;
          
     -- The data structure for a partial solution is as follows: It is
     -- a list of n-tuples where n is the number of variables. These
     -- tuples contain either rational numbers at already solved
     -- positions or the symbol '*' indicating that this position is
     -- unsolved and the special symbol null indicating that the
     -- solution(not exponent) is zero

     -- For each variable we check if it is a non-cell variable, ie 
     -- each solution of the ideal has coordinate zero there
     -- We alse check how often we have to duplicate each solution in the
     -- end to account for monomials of higher order 
     dupnum := 1;     
     psols = {};
     for v in varlist do(
	  if isSubset (set {v},set cv) then(
	       -- Put side effects here:
	       -- Filling the list
	       psols = psols | {"*"};
	       )
	  else (
	       -- Put side effects here:
	       resu := axisSaturate(I, index v);
	       dupnum = dupnum * resu#0;
	       -- Filling the list
	       psols = psols | {null};
	       );
	  );
     
     -- If there are no cell variables: We are done
     if delete(null,psols) === {} then return for i in 0..(degree I)-1 list psols;
          
     -- The old solution for reference:
--     print "The following should coincide if no double sols";
--     print for v in varlist list if saturate(I,v) != I then null else "*"; 
--     print psols;
     
     -- make it a proper list of solutions
     psols = {psols};
     
     -- We solve on a log-scale for the exponents
     while #groeb > 0 do (
	  -- check if the current term is a binomial
	  if #(exponents groeb#0) > 1 then (
	       psols = SolveMore(groeb#0, psols);
	       );
     	  groeb = drop(groeb, 1);
	  );
     
     -- Now we duplicate:
     if dupnum > 1 then psols = for i in 1..dupnum list psols;
     return flatten psols;
     )


-- End of source code ---

beginDocumentation()

document { 
        Key => Binomials,
        Headline => "a package for binomial ideals",
        EM "Binomials", " is a package for binomial ideals."
        }
   
document {
     Key => {binomialPrimaryDecomposition},
     Headline => "Binomial Primary Decomposition",
     Usage => "binomialPrimaryDecomposition I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "l" => {"a list of binomial primary components of I"} },
     "This routine returns a minimal primary decomposition of a binomial ideal into binomial ideals.",
     EXAMPLE {
          "R = QQ[x,y,z]",
          "I = ideal (x*y-z, x*z-y^2)",
          "bpd = binomialPrimaryDecomposition I",
	  "intersect bpd == I"
          },
     "A synonym for this function is 'BPD'.",
     Caveat => {"Note that if the coefficient field needs to be extended, strange things can happen"},
     SeeAlso => BPD
     }

document {
     Key => BPD,
     Headline => "Binomial Primary Decomposition",
     "BPD is a synonym for binomialPrimaryDecomposition."
     }

document {
     Key => {binomialCellularDecomposition,
          (binomialCellularDecomposition,Ideal),
	  [binomialCellularDecomposition,returnCellVars],
	  [binomialCellularDecomposition,verbose]},
     Headline => "Binomial Cellular Decomposition",
     Usage => "binomialCellularDecomposition I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "l" => {"a list of cellular ideals whose intersection is I or 
	        a list of pairs of these ideals and their cellular variables 
		if the option returnCellVars => true is used"} },
     "A binomial ideal I is called cellular if modulo I every variable in 
     the polynomial ring is either a non-zerodivisor or nilpotent. 
     This routine returns a minimal cellular decomposition of a 
     binomial ideal.",
     EXAMPLE {
          "R = QQ[x,y,z]",
          "I = ideal (x*y-z, x*z-y^2)",
          "bcd = binomialCellularDecomposition I",
	  "intersect bcd == I",
     	  "binomialCellularDecomposition (I, returnCellVars=>true, verbose=>false)"
          },
     "A synonym for this function is 'BCD'.",
     "If the option ", TO verbose, " is set (default), then output about the number of components found so far will be generated.",
     SeeAlso => BCD
     }

document {
     Key => BCD,
     Headline => "Binomial Cellular Decomposition",
     "BCD is a synonym for binomialCellularDecomposition."
     }

document {
     Key => {binomialRadical},
     Headline => "Radical of a binomial ideal",
     Usage => "binomialRadical I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "l" => {"the radical of I"} },
     "If the input is a cellular binomial ideal then a very fast algorithm is used. 
     If one knows this and also the cellular variables then ", 
     TO cellularBinomialRadical, " should be used.",
     EXAMPLE {
          "R = QQ[x,y]",
	  "I = ideal (y^2, x*y-y, x^2-1)",
	  "binomialRadical I",
          },
     SeeAlso => cellularBinomialRadical,
     }

document {
     Key => {cellularBinomialRadical,
	  (cellularBinomialRadical,Ideal),
	  [cellularBinomialRadical,cellVariables]},
     Headline => "Radical of a cellular binomial ideal",
     Usage => "cellularBinomialRadical I",
     Inputs => {
          "I" => { "a cellular binomial ideal"} },
     Outputs => {
          "l" => {"the radical of I"} },
     "The radical of a cellular binomial ideal can be determined very quickly. If the 
     cellular variables are known they can be given as a list via the option ", TO cellVariables, ".",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal(y^3,y^2*z^2-x^3,x*y^2*z,x^3*z-x*y)",
	  "cv = isCellular (I,returnCellVars=>true)",
	  "cellularBinomialRadical (I,cellVariables=>cv)"
          },
     SeeAlso => binomialRadical,
     }

document {
     Key => {binomialMinimalPrimes,
	  (binomialMinimalPrimes,Ideal),
	  [binomialMinimalPrimes,verbose]},
     Headline => "minimal primes of a binomial Ideal",
     Usage => "binomialMinimalPrimes I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "l" => {"the list of minimal primes of I"} },
     "The binomial minimal primes of a binomial ideal over QQ exist only in extension fields.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal(y^3,y^2*z^2-x^3,x*y^2*z,x^3*z-x*y)",
	  "binomialMinimalPrimes I",
          },
     "If the option ", TO verbose, " is set (default), then output about the number of components found so far will be generated.",
     SeeAlso => binomialRadical,
     }    

document {
     Key => {binomialAssociatedPrimes},
     Headline => "Associated primes of a binomial ideal",
     Usage => "binomialAssociatedPrimes I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "l" => {"the list of associated primes of I"} },
     "First a cellular decomposition is run, then the associated primes of each cellular component are determined.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-y,y^2-x)",
	  "binomialAssociatedPrimes I",
          },
     SeeAlso => {binomialMinimalPrimes,cellularBinomialAssociatedPrimes},
     }    

--     -- tests

document {
     Key => {binomialIsPrime,
	  (binomialIsPrime,Ideal),
	  [binomialIsPrime,cellVariables]},
     Headline => "test for primeness of a binomial ideal",
     Usage => "binomialIsPrime I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "f" => {"true or false, depending on whether I is a binomial prime ideal"} },
     "A binomial ideal is prime only if it is cellular. If the cellular variables",
     "are known they can be given via the ", TO cellVariables, " option.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-y,y^2-x)",
	  "binomialIsPrime I",
          },
     SeeAlso => {binomialIsPrimary, cellVariables},
     }    

document {
     Key => {binomialIsPrimary,
	  (binomialIsPrimary,Ideal),
	  [binomialIsPrimary,cellVariables],
	  [binomialIsPrimary,returnPrimes],
	  [binomialIsPrimary,returnPChars]},
     Headline => "test for primaryness of a binomial ideal",
     Usage => "binomialIsPrimary I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "f" => {"true or false, depending on whether I is a binomial primary ideal"} },
     "A binomial ideal is primary only if it is cellular. If the cellular variables",
     "are known they can be given via the ", TO cellVariables, " option.", "If the ideal is not primary, ",
     "either 'false' or two distinct associated primes can be returned. The behaviour can be changed using the options ",
     TO returnPrimes, " and ", TO returnPChars, ".",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-1)",
	  "binomialIsPrimary (I,returnPrimes=>true)",
          },
     SeeAlso => {binomialIsPrimary, cellVariables, returnPrimes, returnPChars},
     }    

document {
     Key => {binomialSolve},
     Headline => "solving zero-dimensional binomial Ideals",
     Usage => "binomialSolve I",
     Inputs => {
          "I" => { "a pure difference binomial ideal"}},
     Outputs => {
          "l" => {"the list of solutions of I in QQ[ww]"} },
     "The solutions of a pure difference binomial ideal exist in a cyclotomic field. This function
     will solve the ideal and construct an apropriate cyclotomic field such that the solutions are contained.
     If no extension is needed then the symbol that was given will remain untouched",
     EXAMPLE {
	  "R = QQ[x,y,z,w]",
	  "I = ideal (x-y,y-z,z*w-1*w,w^2-x)",
	  "dim I",
	  "binomialSolve I",
	  "J = ideal (x^3-1,y-x,z-1,w-1)",
	  "binomialSolve J"
          },
     Caveat => {"The current implementation can only handle pure difference binomial ideals."},
     SeeAlso => Cyclotomic
     }

document {
     Key => {isCellular,
	  (isCellular,Ideal),
	  [isCellular,returnCellVars]},
     Headline => "testing for cellular binomial ideals",
     Usage => "isCellular I",
     Inputs => {
          "I" => { "a binomial ideal"}},
     Outputs => {
          "t" => {"true, or the list of cell variables if I is cellular, false otherwise."} },
     "This function is the standard way to compute the cellular variables.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal (x-z^2,y^4)",
	  "isCellular I",
	  "isCellular (I, returnCellVars=>true)"
          },
     SeeAlso => {cellularBinomialAssociatedPrimes,binomialCellularDecomposition}
     }

document {
     Key => {isBinomial,
	     isPureDifference},
     Headline => "testing for pure difference binomial ideals",
     Usage => "isBinomial I",
     Inputs => {
          "I" => { "an ideal"}},
     Outputs => {
          "t" => {"true if I is binomial, or pure difference respectively."} },
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "isBinomial ideal(x^2)",
	  "isBinomial ideal(x-y+z,z)",
	  "isBinomial ideal(x^3-x)",
	  "isPureDifference ideal (x-z,z-y)",
	  "isPureDifference ideal (x+z)",
	  "isPureDifference ideal (x^2)"
          },
     SeeAlso => {isCellular}
     }

--     -- input related

document {
     Key => {makeBinomial},
     Headline => "make a binomial from an exponent vector and a coefficient",
     Usage => "makeBinomial (R,m,c)",
     Inputs => {
          "R" => { "a ring"},
	  "m" => { "a vector of exponents, one for each generator of R"},
	  "c" => { "an element of the coefficient ring of R"}},
     Outputs => {
          "b" => {"The binomial defined by the input data, as an element of R."} },
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "makeBinomial (R, [1,-1,-2], 10)"
          }
     }

document {
     Key => {latticeBasisIdeal},
     Headline => "construct the ideal whose generators correspond to generators of an integer lattice",
     Usage => "latticeBasisIdeal (R,L)",
     Inputs => {
          "R" => { "a ring"},
	  "L" => { "an interger matrix whose columns span the lattice."}},
     Outputs => {
          "I" => {"The pure difference lattice basis ideal in R."} },
     "This function is only a very simple wrapper around ", TO makeBinomial ,
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "L = matrix {{1,1},{-3,0},{0,1}}",
	  "latticeBasisIdeal (R, L)"
          }
     }

--     -- cellular stuff:

document {
     Key => {cellularBinomialAssociatedPrimes,
	  (cellularBinomialAssociatedPrimes,Ideal),
	  [cellularBinomialAssociatedPrimes,cellVariables]},
     Headline => "Associated primes of a cellular binomial ideal",
     Usage => "cellularBinomialAssociatedPrimes I",
     Inputs => {
          "I" => { "a cellular binomial ideal"} },
     Outputs => {
          "l" => {"the list of associated primes of I"} },
     "If the cell variables are known, they can be given via the option ", TO cellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-1,y-x)",
	  "cv = isCellular (I,returnCellVars=>true)",
	  "cellularBinomialAssociatedPrimes (I,cellVariables=>cv)"
          },
     SeeAlso => binomialAssociatedPrimes,
     }    

--     cellularAssociatedLattices,

document {
     Key => {cellularBinomialPrimaryDecomposition,
	  (cellularBinomialPrimaryDecomposition,Ideal),
	  [cellularBinomialPrimaryDecomposition,cellVariables]},
     Headline => "Primary decomposition of a cellular binomial ideal",
     Usage => "cellularBinomialPrimaryDecomposition I",
     Inputs => {
          "I" => { "a cellular binomial ideal"} },
     Outputs => {
          "l" => {"the primary decomposition of I"} },
     "If the cell variables are known, they can be given via the option ", TO cellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^3-1,y-x)",
	  "cv = isCellular (I,returnCellVars=>true)",
	  "pd = cellularBinomialPrimaryDecomposition (I,cellVariables=>cv)",
	  "mingens \\ pd"
          },
     Caveat => {"This function will not return minimal generators for performance reasons."},
     SeeAlso => binomialAssociatedPrimes,
     }    

document {
     Key => {partialCharacter,
	  (partialCharacter,Ideal),
	  [partialCharacter,cellVariables]},
     Headline => "Computing the partial character of a cellular binomial ideal",
     Usage => "partialCharacter I",
     Inputs => {
          "I" => { "a cellular binomial ideal"} },
     Outputs => {
          "cv" => {"the cellular variables"},
	  "L" => {"A matrix whose columns are generators for the lattice supporting the character"},
	  "c" => {"The values that the character takes on the generator"}},
     "If the cell variables are known, they can be given via the option ", TO cellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^3-1,y-x)",
	  "cv = isCellular (I,returnCellVars=>true)",
	  "pc = partialCharacter (I,cellVariables=>cv)",
          },
     }    

document {
     Key => cellVariables,
     Headline => "cellular variables",
     "The cellular variables of a binomial ideal are the variables which are non-zerodivisors 
     module that ideal. With this option these variables, if known in advance, can be handed over 
     to specialized functions for cellular ideals. ",
     SeeAlso => {cellularBinomialPrimaryDecomposition,cellularBinomialAssociatedPrimes}
     }

document {
     Key => returnCellVars,
     Headline => "return the cellular variables",
     "The cellular variables of a binomial ideal are the variables which are non-zerodivisors 
     module that ideal. If this option is set to 'true' then binomialCellularDecomposition will
     return the set of variables for each of its outputs",
     EXAMPLE {
	  "R = QQ[x,y,z]",
          "I = ideal (x*y-z, x*z-y^2)",
          "bcd = binomialCellularDecomposition (I,returnCellVars=>true)",
          },
     }

document {
     Key => returnPrimes,
     Headline => "return two associated primes",
     "If binomialIsPrimary does not return true it can either return 'false' or two associated primes.
     If this option is set then two associated primes are returned. If returnPChars is set too, then partial
     characters will be returned.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
          "I = ideal (x^2-1)",
          "binomialIsPrimary (I,returnPrimes=>true)",
          },
     SeeAlso => {returnPChars, binomialIsPrimary}
     }

document {
     Key => returnPChars,
     Headline => "return two partial characters",
     "If binomialIsPrimary does not return true it can either return 'false' or two associated primes.
     If this option is set then two partial characters of distinct associated primes are returned. 
     If returnPrimes is set too, then partial characters will be returned.",
     EXAMPLE {
	  "R = QQ[x]",
          "I = ideal (x^2-1)",
          "binomialIsPrimary (I,returnPChars=>true)",
          },
     SeeAlso => {returnPrimes, binomialIsPrimary}
     }

document {
     Key => verbose,
     Headline => "generate informative output",
     "If this option is set, functions will generate additional output. Defaults to true"
     }

