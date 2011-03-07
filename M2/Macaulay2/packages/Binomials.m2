-- -*- coding: utf-8 -*-
--  Binomials.m2
--
--  Copyright (C) 2009-2011 Thomas Kahle <kahle@mis.mpg.de>
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
	Version => "0.7",
	Date => "January 2011",
	Authors => {{
		  Name => "Thomas Kahle",
		  Email => "kahle@mis.mpg.de",
		  HomePage => "http://www.thomas-kahle.de/bpd"}},
    	Headline => "Specialized routines for binomial Ideals",
	Configuration => { },
    	DebuggingMode => true,
	Reload=>true
    	)
   
export {
     -- 'Official' functions
     binomialPrimaryDecomposition,
     binomialCellularDecomposition,
     binomialUnmixedDecomposition,
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
     cellularBinomialUnmixedDecomposition,
     -- cellularAssociatedLattices,
     cellularBinomialPrimaryDecomposition,
     cellularBinomialRadical,
     -- cellularEmbeddedLatticeWitnesses,
     -- simple wrappers:
     BPD,
     BCD,
     BUD,
--     BCDisPrimary,
     -- auxillary functions:
     partialCharacter,
     idealFromCharacter,  -- should be renamed to ideal once M2 supports this
     randomBinomialIdeal,
     removeRedundant,
     -- Not in the interface:
--     axisSaturate,
--     cellVars,
--     Lsat,
--     saturatePChar,
--     satIdeals,
--     nonCellstdm,
--     maxNonCellstdm,
--     minimalPrimaryComponent,
--     binomialQuasiPower,
--     binomialQuotient,
     -- Removed as of M2 v1.2
     -- Uncomment this if you are using <= 1.1
     -- lcm

     -- Options
     cellVariables, -- for partialCharacter
     returnPrimes, -- for binomialIsPrimary 
     returnPChars, -- for binomialIsPrimary
     returnCellVars, -- for binomialCellularDecomposition
     verbose, -- produce more output
     
     --Types
     PartialCharacter--HashTable
     }

needsPackage "FourTiTwo";
needsPackage "Cyclotomic";

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
-- based on code by Ignacio Ojeda and Mike Stillman     
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
		   newL1 := drop(L#1,1); -- gets removed from the list
	           result := axisSaturate(L#2, i); -- compute saturation wrt i
		   J := result#1; -- Ideal
		   k := result#0; -- Saturation Exponent
		   if k > 0 then ( -- If a division was needed:
     	       	    	-- We add the monomial i^k to ideal under consideration		      	   	
			J2 := L#2 + ideal(R_i^k); 
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
Lsat = A -> LLL syz transpose LLL syz transpose A;

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

cellVars = method (Options => {cellVariables => null})
cellVars Ideal := Ideal => o -> I -> (
     -- This function compotes the cell variables for a cellular ideal.
     -- If a list of cellVars is given it maps them to the ring of I
     -- to ensure uniformity when changing rings.
     if o#cellVariables === null then (
	  cv := {};
	  for i in gens ring I do if saturate (I,i) != substitute(ideal(1), ring I) then cv=cv|{i};
	  return cv;
	  )
--     else return ((i) -> sub(i, ring I)) \ o#cellVariables;
     else return o#cellVariables;
     )

PartialCharacter = new Type of HashTable;
     --Setting up the PartialCharacter Type

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
     II := ideal;
     
     -- The input should be a cellular ideal 
     cv := cellVars(I, cellVariables=>o#cellVariables);
     ncv := toList (set gens R - cv);
     
     -- If there are no cellular variables, 
     -- the ideal is monomial and the partial character is zero:
     if cv == {} then (
	  return new PartialCharacter from {"J"=>{}, "L"=>matrix "0", "c"=>{1}};
	  );
     
     -- We need to construct this ring to properly extract coefficients below
     S := CoeffR(monoid [cv]);
     
     -- We intersect I with the ring k[E] and map to S
     if #ncv != 0 then (
     	  II = sub(eliminate (ncv, I),S);
	  )
     else (
	  -- S = R, stick with original def!
	  II = I;
	  );

     -- The partial Character of the zero ideal is the zero lattice.
     if ( II == 0 ) then (
	  for i in cv do vs = vs | { 0_ZZ };
	  cl = {1_ZZ};
	  return new PartialCharacter from {"J"=>cv, "L"=>transpose matrix {vs}, "c"=>cl};
	  );
     
     -- So, II is not zero:
     -- Let ts be the list of minimal generators, this uses that II\subset S !
     ts := entries mingens II;
     -- for each term, find the exponent vector
     oldmat := matrix "0";
     oldvs := {};
     for t in ts#0 do (
	  -- Want to check if we already have this generator included
	  
	  -- Save the old values
	  oldmat = vsmat;
	  oldvs = vs;
	  	  
	  -- compute new ones
	  vs = vs | {((exponents t)#0 - (exponents t)#1)};
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
     
     return (new PartialCharacter from {"J"=>cv, "L"=> transpose matrix vs , "c"=>cl});
     )

randomBinomialIdeal = (R,numge,maxdeg, maxwidth, homog) -> (	 
     -- Generate 'random' ideals for testing purposes. The distribution is completely heuristic and designed to serve
     -- internal purposes 
     -- Inputs: a ring R, the number of generators numgen, the maximal degree of each variable maxded,
     -- the maximal number of variables appearing in binomial, wether the output should be homogeneous
     
     -- Caveat: The result might simply be not homogeneous or of the given degree 
     -- due to deps between the random generators     
     
     -- Output: a 'random' binomial ideal.
     
     Rge := gens R;
     ng := #Rge;
     ge := {};
     ra := 0; split := 0;
     va := {}; m := {};
     z := for i in 0..ng-maxwidth-1 list 0;
     if homog then (
	  if odd maxwidth then maxwidth = maxwidth + 1;
	  for i in 0..numge do (
	       -- m will be a list of nonzero random exponents
     	       m = for j in 0..(maxwidth//2)-1 list (
	       	    ra = (random (2*maxdeg)) +1 ;
	       	    if ra > maxdeg then ra = -ra // 2;
	       	    ra
	       	    );
	       m = m | for j in 0..(maxwidth//2)-1 list (
		    ra = (random (2*maxdeg)) +1 ;
	       	    if ra > maxdeg then ra = -ra // 2;
	       	    -ra
		    );
     	       -- filling with zeros
	       m = random (m |z);
     	       ge = ge | {makeBinomial (R,m,1)};
  	       );  
	  )
     else (
     	  for i in 0..numge do (
	       -- m will be a list of nonzero random exponents
     	       m = for j in 0..maxwidth-1 list (
	       	    ra = (random (2*maxdeg)) +1 ;
	       	    if ra > maxdeg then ra = -ra // 2;
	       	    ra
	       	    );
     	       -- filling with zeros
	       m = random (m |z);
     	       ge = ge | {makeBinomial (R,m,1)};
  	       );
	  );
     return ideal (mingens ideal(ge))
     )

isBinomial = I -> (
     -- Checking binomiality is only proper with a gb.
     ge := flatten entries gens gb I;
     isBinomial:=true;
     for g in ge do (
          if #(terms g) > 2 then return false;
	  );
     return true;
     )

isPureDifference = I -> (
     -- Here we do allow monomials, because our algorithm also works
     -- for pure difference + monomials
     ge := flatten entries gens gb I;
     for g in ge do (
	  coeffs := sort flatten entries (coefficients g)#1;
	  if coeffs == {1} then continue;
	  if coeffs == { -1} then continue;
	  if coeffs == { -1 , 1} then continue;
	  return false;
	  );
     return true;
     )

     
nonCellstdm = {cellVariables=>null} >> o -> I -> (
     R := ring I;

     cv2 := cellVars(I, cellVariables=>o#cellVariables);

     -- Extracts the monomials in the non-Cell variables.
     cv := set cv2; 
     -- Here go the non-cell variables
     -- This use of baseName is intended to fix a problem where the variables in cv 
     -- are actual variables of a ring over a field extension. TODO Item: Understand this
     ncv := value \ toList (set (baseName \ (gens R)) - baseName \ cv);
     
     -- We map I to the subring: k[ncv]
     CoeffR := coefficientRing R;
     S := CoeffR(monoid [ncv]);
     J := kernel map (R/I,S); -- image of I in the subring S
     return basis (S/J);
     )

maxNonCellstdm = {cellVariables=>null} >> o -> I -> (
     -- Computes the maximal monomials in the non-cellular variables

     cv := cellVars(I, cellVariables=>o#cellVariables);
     nm := flatten entries nonCellstdm (I,cellVariables=>cv);
     -- The following code extracts the maximal elements in a list of monomials 
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
     posmon :=1_R;
     negmon :=1_R;
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

idealFromCharacter = method();
idealFromCharacter (Ring, PartialCharacter) := Ideal => (R, pc) -> (
     -- Constructs the lattice Ideal I_+(c) in R
     -- R is a ring in which the ideal is returned
     -- The columns of A should contain exponent vectors of generators
     -- The vector c contains the corresponding coefficients which must lie
     -- in the coefficient ring of R.
     
     var := gens R;
     if pc#"L" == 0 then return ideal 0_R;
     cols := null;
     binomials :=null;
     c:= null;
     
     idmat := matrix mutableIdentity(ZZ,#var);
     if pc#"L" == idmat then (
	  -- If A is the unit matrix we are lucky,
	  -- no saturation is needed.

	  -- We coerce the coefficients to R:
	  c = apply (pc#"c", a -> (sub (a,R)));
     	  cols = entries transpose pc#"L";
     	  binomials = for i in 0..numcols(pc#"L")-1 list makeBinomial (R,cols#i, c#i);	  
	  return ideal binomials
	  )
     else if set pc#"c" === set {1} then (
	  -- all coefficients are one, we can use 4ti2.
	  return toricMarkov (transpose pc#"L", R, InputType => "lattice");
	  )
     else (
     	  -- The general case, fall back to saturation in M2:
	  c = apply (pc#"c", a -> (sub (a,R)));
     	  cols = entries transpose pc#"L";    
     	  binomials = for i in 0..numcols(pc#"L")-1 list makeBinomial (R,cols#i, c#i);
     	  return saturate (ideal binomials, product var);
	  );
     )

latticeBasisIdeal = (R,L) -> (
     -- Constructs the lattice basis ideal (whose saturation is the lattice ideal)
     -- Convention is that L's columns generate the lattice.
     var := gens R;
     if L == 0 then return ideal 0_R;
     cols := null;
     binomials :=null;
     cols = entries transpose L;
     binomials = for i in 0..numcols L-1 list makeBinomial (R,cols#i, 1);
     return ideal binomials;
     )

saturatePChar = (pc) -> (
     -- This function saturates a partial character and returns the result
     -- as a list, even if the input was saturated.
          
     -- If the lattice is saturated, the character is saturated
     -- Note that this shortcircuits all problems with c being non-constant.
     if image Lsat pc#"L" == image pc#"L" then (
	  return {pc};
	  );
     
     -- The saturated lattice
     S := Lsat(pc#"L");
     -- The coefficient matrix :
     K := pc#"L" // S;
     
     -- print K;
     -- Now we find the (binomial) equations for the saturated character:
     numvars := numrows K;
     varlist := for i in 0..numvars-1 list value ("m"|i);
     Q := QQ(monoid [varlist]);
     eqs := idealFromCharacter (Q, (new PartialCharacter from {"J"=>gens Q, "L"=>K, "c"=>pc#"c"}));
     
     result := binomialSolve eqs;
     r := #result;
     i := 0;
     
     return(for i from 0 to r-1 list(
	  new PartialCharacter from {"J" => pc#"J", "L" => S, "c" => result#i}));
     )

satIdeals = (pc) -> (
     -- Computes all the ideals belonging to saturations of  
     -- a given partial character.
     satpc := saturatePChar(pc);
     -- The following should be the smallest ring containing all new
     -- coefficients but not smaller than QQ
     F := ring satpc#0#"c"#0;
     if F === ZZ then F = QQ;
     Q := F(monoid [satpc#0#"J"]);
     return for s in satpc list idealFromCharacter (Q, s);
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
	  return ideal mingens intersect mp;
	  )
     )

cellularBinomialRadical = method (Options => {cellVariables => null}) 
cellularBinomialRadical Ideal := Ideal => o -> I -> (
     cv := cellVars(I, cellVariables=>o#cellVariables);
     
     -- Computes the radical of a cellular binomial ideal
     R := ring I;
     -- Get the partial character of I
     pc := partialCharacter(I, cellVariables=>cv);
     noncellvars := toList(set (gens R) - pc#"J");
     	       
     M := sub (ideal (noncellvars),R);
     
     -- The binomial part of the radical 
     prerad := eliminate (noncellvars,I);

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
     
     R := ring I;
     -- Only proper ideals are considered primary
     if I == ideal(1_R) then return false;      
     
     -- Handling of cell variables
     cv := cellVars(I, cellVariables=>o#cellVariables);

     -- Get the partial character of I
     pc := partialCharacter(I, cellVariables=>cv);
     noncellvars := toList(set gens R - cv);
     
     M := sub (ideal (noncellvars),R);
     
     -- We intersect I with the ring k[E] to get the associated lattice ideal
     prerad := eliminate (noncellvars,I);
          
     rad := prerad + M;
     
     -- If the partial character is not saturated, the radical is not prime
     if image Lsat pc#"L" != image pc#"L" then (
	  print "The radical is not prime, as the character is not saturated";
	  satpc := saturatePChar pc;
	  if o#returnPChars then (
	       -- This one is the fastest, so check it first
	       return {{satpc#0#"J",satpc#0#"L",satpc#0#"c"}, {satpc#0#"J",satpc#0#"L",satpc#1#"c"}}
	       );
	  if o#returnPrimes then (
     	       F := ring satpc#0#"c"#0;
     	       S := F(monoid [satpc#0#"J"]);
	       M = sub(M,S);
	       ap1 := idealFromCharacter (S,satpc) + M;
	       ap2 := idealFromCharacter (S,satpc) + M;
	       -- Return two distinct associated primes:
	       return {ap1,ap2};
     	       )	   	       
	  else return false;
	  );
     
     -- If the radical is prime, then there still might be embedded
     -- primes that properly contain the radical. The remaining part
     -- finds such primes by examining quotients w.r.t (maximal)
     -- standard monomials. 
     
     -- The list of maximally standard monomials:
     maxlist := maxNonCellstdm (I,cellVariables=>cv);
     -- need to map to R to do colons with I
     f := map (R, ring maxlist#0);
     maxstdmon := maxlist / f;
     
     for m in maxstdmon do (
	  q := I:m;
	  -- Mapping down to k[E]:
	  q2 := eliminate (noncellvars,q);
     	  -- I_+(sigma) was called prerad above:
	  if not isSubset(q2, prerad) then (
	       -- creating some local names:
	       satqchar := saturatePChar partialCharacter (q,cellVariables=>cv);
	       if o#returnPChars then(
		    return {pc, {satqchar#0#"J",satqchar#0#"L",satqchar#0#"c"}}
		    );
	       if o#returnPrimes then (
		    F := ring satqchar#0#"c"#0;
     	       	    S := F(monoid [satqchar#0#"J"]);
	       	    M = sub(M,S);
		    ap2 := idealFromCharacter (S, satqchar);
		    return {rad, ap2 + M};
     	       	    )
	       else return false;
	       );
	  );
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
     cv := cellVars(I, cellVariables=>o#cellVariables);

     R := ring I;
     if I == ideal (1_R) then return false;
     pc := partialCharacter (I,cellVariables=>cv);
     ncv := toList(set (gens R) - cv);
     for v in ncv do (
	  if not isSubset(ideal (v) , I) then return false;
     	  );

     -- Is the partial character saturated ???     
     if image Lsat pc#"L" != image pc#"L" then return false;
     
     -- all tests passed:
     return true;
     )

binomialMinimalPrimes = method (Options => {verbose=>true})
binomialMinimalPrimes Ideal := Ideal => o -> I -> (
     -- The new algorithm, based on computing a cellular decomposition of the radical of I
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
		   newL1 := drop(L#1,1); -- gets removed from the list
	           result := axisSaturate(L#2, i); -- compute saturation wrt i
		   J := result#1; -- Ideal
		   k := result#0; -- Saturation Exponent
		   if k > 0 then ( -- If a division was needed:
			J2 := L#2 + ideal(R_i); 
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
     ME :=ideal; {* pc = {}; *} si := ideal; mp := {}; F := null; S:= null;
     for a in Answer do (
	  i = i+1;
	  print ("Finding minimal primes of cellular component: " | toString i | " of " | toString j);
	  ME := ideal(toList(set (gens R) - a#1));
	  pc := partialCharacter (a#0, cellVariables=>a#1);
	  -- Check whether we have a radical ideal already:
	  if image Lsat pc#"L" == image pc#"L" then (
	       si = {a#0};
	       )
	  else (
	       si = satIdeals pc
	       );
	  F = coefficientRing ring si#0;
	  if F === QQ then S = R else S = F(monoid [ge]);
	  ME = sub (ME, S);
	  si = for i in si list sub(i,S);
	  si = si / (i -> trim (i + ME)); -- Adding monomials;
	  mp = mp | si;
	  );

     mp = joinCyclotomic mp;
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

isBetween = (a,b,c) -> (
     	  -- Checks if a lies between b and c in divisibility order.
	  -- b and c need not be comparable, or sorted.
	  if (b%c == 0) then (
	       -- c divides b
	       if ( (a%c==0) and (b%a==0)) then return true;
	       )
	  else if (c%b == 0) then (
	       if ( (a%b==0) and (c%a==0)) then return true;
	       );
	  -- b and c are not comparable
	  return false;
     )

cellularBinomialAssociatedPrimes = method (Options => {cellVariables => null, verbose=>true}) 
cellularBinomialAssociatedPrimes Ideal := Ideal => o -> I -> ( 
     -- Computes the associated primes of cellular binomial ideal
     -- It returns them in a polynomial ring with the same variables as ring I,
     -- but potentially extended coefficient ring !

     -- TODO: It could be faster by rearringing things in the m in ml

     -- Innovation: Use memoize to speed up determination of the associated primes:
     
     R := ring I;
     
     cv := cellVars(I, cellVariables=>o#cellVariables);
          
     primes := {}; -- This will hold the list of primes
     ncv := toList(set (gens R) - cv); -- non-cell variables x \notin E
     stdm := nonCellstdm(I,cellVariables=>cv); -- List of std monomials in ncv
     -- mapping to R:
     f := map (R, ring stdm);
     ml := flatten entries f stdm;
     
     if o#verbose then(
	  if #ml == 1 then << "1 monomial to consider for this cellular component " << endl
     	  else <<  #ml << " monomials to consider for this cellular component" << endl;
	  );

     -- Saves all witness monomials for a given partialCharacter
     seenpc := new MutableHashTable;

     -- A dummy ideal and partial Characters:
     Im := ideal;
     pC := {}; sat := {};
     -- save 1 as the bottom witness
     seenpc#(partialCharacter (I, cellVariables=>cv))={1_R};
     todolist := delete(1_R, ml);
     -- While we have monomials to check
     while #todolist > 0 do (
--	  print ("On todolist: " | toString (#todolist));
	  -- sample a random monomial:
	  i := random(0, #todolist-1);
	  m := todolist#i;
	  Im = I:m;
	  pC = partialCharacter(Im, cellVariables=>cv);
	  if seenpc#?pC then (
	       -- We have seen this lattice: Time to prune the todolist
--	       print ("Todolist items before: " | toString (#todolist));
	       for n in seenpc#pC do (
		    todolist = select (todolist , (mm -> not isBetween (mm, n, m))
		    ));
--	       print ("Todolist items after: " | toString (#todolist));
	       -- add m to the pruning list
	       todolist = delete(m, todolist);
	       addmon := true;
	       for mmm in seenpc#pC do if m%mmm==0 then (addmon = false; break);
	       if addmon then seenpc#pC = seenpc#pC | {m};
--	       print (#(seenpc#pC));
	       )   
	  else (
	       -- a new associated lattice
	       seenpc#pC = {m};
	       todolist = delete(m, todolist);
	       )
	  );
--     print ("Todolist items: " | toString (#todolist));
     for pc in keys seenpc do (
	  sat = satIdeals pc;
	  -- If the coefficientRing is QQ, we map back to R
	  F := coefficientRing ring sat#0;
	  if F === QQ then (
	       f = map (R, ring sat#0);
	       sat = sat / f ;
	       )
	  else (
	       -- otherwise to the extended ring
	       -- this is necessary since satIdeals does not know about the non-cell variables
	       S := F monoid R;
	       f = map (S, ring sat#0);
	       sat = sat / f;
	       );
	  primes = primes | sat;
	  );
     -- We need to remove duplicate elements and join all associated primes in an apropriate new ring that contains all
     -- their coefficients.
     primes = joinCyclotomic primes;
     M := sub (ideal ncv, ring primes#0);
     primes = primes / (I -> I + M);

     -- Computation of mingens is necessary as unique or toList + set combi won't do without
     return unique (ideal \ mingens \ primes);
     )

binomialAssociatedPrimes = I -> (
     if not isBinomial I then error "Input not binomial";
     -- TODO: Even the stupid algorithm can be improved by carrying on the cell Variables!
     cv := isCellular (I,returnCellVars=>true);
     -- This is no real check. If CellVariables are given we dont check for speed reasons
     if cv === false then (
	  print "Not yet implemented";
	  print "I will compute a primary decomposition and take radicals!";
	  bpd := BPD I;
	  print "Primary Decomposition found, taking radicals now:";
	  -- TODO: Since binomialRadical tries to preserve the input ring this step 
	  -- will actually change from ring I to 'ring bpd' on the user visible level.
	  return binomialRadical \ bpd;
	  )
     else (
	  return cellularBinomialAssociatedPrimes (I, cellVariables=>cv);
	  );
     )

cellularAssociatedLattices = method (Options => {cellVariables => null})
cellularAssociatedLattices Ideal := Ideal => o -> I -> (
     -- Computes the some associated lattices of a cellular binomial ideal
     -- WARNING: The definition might differ from that in the paper with Ezra Miller
     -- Todo: Can we get the multiplicities too ?
     
     R := ring I;
     cv := cellVars(I, cellVariables=>o#cellVariables);
     lats := {}; -- This will hold the list of lattices
     coeffs := {}; -- This will hold the values of the characters
     ncv := toList(set (gens R) - cv); -- non-cell variables x \notin E
     -- print "Noncellvars"; print ncv;
     ml := flatten entries nonCellstdm(I,cellVariables=>cv); -- List of std monomials in ncv
     -- Coercing to R:
     f := map (R, ring ml#0);
     ml = ml/f;
     -- A dummy ideal and partial Characters:
     Im := ideal;
     pc := {};
     redundant := true;
     -- For each monomial, check if I:m has a different lattice !
     for m in ml do (
	  -- print m;
	  Im = I:m;
	  -- We already know the cell variables in the following computation
	  pc = partialCharacter(Im, cellVariables=>cv);
	  if #lats == 0 then (
	       lats = {pc#"L"};
	       coeffs = {pc#"c"};
	       continue;
	       )
	  else (
	       redundant = false;
	       scan (lats, (l -> (if image l == image pc#"L" then redundant = true)))
     	       );
	  if redundant then continue
	  else (
	       lats = lats | {pc#"L"};
	       coeffs = coeffs | {pc#"c"};
	       );
      	  ); -- for m in ml	    
     return {cv, lats, coeffs};
     ) -- CellularAssociatedLattices

cellularEmbeddedLatticeWitnesses = method (Options => {cellVariables => null})
cellularEmbeddedLatticeWitnesses Ideal := Ideal => o -> I -> (
     -- Given an ideal whose radical is prime this function will produce
     -- witness monomials for embedded lattices.
     -- Throw in these monomials to get rid of additional associated primes,
     -- i.e. compute Hull.

     R := ring I;
     cv := cellVars(I, cellVariables=>o#cellVariables);
     witnesses := {};
     lats := {}; -- This will hold the list of lattices
     ncv := toList(set (gens R) - cv); -- non-cell variables x \notin E
     -- print "Noncellvars"; print ncv;
     ml := flatten entries nonCellstdm(I,cellVariables=>cv); -- List of std monomials in ncv
     -- Should be sorted by total degree to make the shortcut heuristics optimal.
     ml = sort(ml, DegreeOrder=>Ascending);
     -- Coercing to R:
     f := map (R, ring ml#0);
     ml = ml / f;
     -- A dummy ideal and partial Characters:
     Im := ideal;
     pc := {};
     redundant := true;
     bottomlattice := partialCharacter (I, cellVariables=>cv);
     -- For each monomial, check if I:m has a different lattice !
     todolist := ml;
--     print ("Number of monomials to consider : " | toString (#todolist));
     while (#todolist > 0) do (
	  i := random(0, #todolist-1); -- a random monomial
	  m := todolist#i;
	  -- Now two possibilities:
	  -- if m witnesses an embedded lattice: Remove everything that is above m;
	  Im = I:m;
	  pc = partialCharacter(Im, cellVariables=>cv);
	  if (image pc#"L" == image bottomlattice#"L") then (
	       -- m is the same like 1. Remove everything between them
	       todolist = select (todolist, (mm) -> not isBetween(mm, 1,m))
	       )
	  else (
	       -- found a witness
	       witnesses = witnesses | {m};
	       todolist = for t in todolist list if t%m==0 then continue else t
	       );
--	  print ("Number of monomials to consider : " | toString (#todolist));
	  ); -- while
     return witnesses;
     ) -- cellularEmbeddedLatticeWitnesses


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

     cv := cellVars(I, cellVariables=>o#cellVariables);
     if cv === false then error "Input to minimalPrimaryComponent was not cellular!";

     return I + ideal (cellularEmbeddedLatticeWitnesses (I, cellVariables=>cv));
     )

minimalPrimaryComponent2 = method (Options => {cellVariables => null})
minimalPrimaryComponent2 Ideal := Ideal => o -> I -> (
     -- Input a cellular binomial ideal whose radical is prime.
     -- Ouptut, generators for Hull(I)

     cv := cellVars(I, cellVariables=>o#cellVariables);
     if cv === false then error "Input to minimalPrimaryComponent was not cellular!";

     apc := binomialIsPrimary (I, returnPChars=>true, cellVariables => cv);
     if #apc == 1 then return I -- radical is only associated prime!
     else (
	  R := ring I;
	  CoeffR := coefficientRing R;
	  -- A trick to not clobber the global variables

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
	       print "Only very few examples reach this part of the code";
	       print "PLEASE, send a copy of your input to kahle@mis.mpg.de";
	       print "Thank you!";
	       -- The finite index case :  
	       
	       -- Compute a binomial in J2 which is not in J1.
	       -- i.e. find a generator on which pc1 and pc2 take different values.
	       local b;
	       for i in 0..#(pc2#2)-1 do (
	       	    if pc1#2#i == pc2#2#i then continue
	       	    else (
		    	 -- Character differs. Form binomial:
		    	 b = makeBinomial (CoeffR(monoid [pc2#0]), (entries transpose pc2#1)#i, pc2#2#i );
		    	 break;
		    	 );
	       	    );
	       -- Take the quotient of I with respect to b, such that the result is binomial
	       return minimalPrimaryComponent2 (binomialQuotient (I,b, cellVariables=>cv), cellVariables=>cv);
	       )
       	   else (
		-- The case of infinite index :
		    
                -- Find an exponent vector which has infinite order:
		-- i.e. a vector m \in L2 such that image m \cap L has dimension < 1;
		-- One of the generators must have this property !
     		    
		 -- Here are the lattice generators
		 L2cols := entries transpose pc2#1;
		 -- Try them one by one:
		 i := 0; -- Counter to determine which generator fits
		 for c in L2cols do (
		      -- The span of c:
		      imc := image transpose matrix {c};
		      if rank intersect {imc , L} < 1 then (
			   -- We have winner 
			   break;
			   );
		      -- Lets try the next vector.
		      i = i+1;
		      );
     	         -- now i has the suitable index !
		 b = makeBinomial(CoeffR(monoid [pc2#0]), L2cols#i, pc2#2#i);
	    	 -- Take the quotient of I with respect to b, such that the result is binomial
		 return minimalPrimaryComponent2 (binomialQuotient (I,b, cellVariables=>cv), cellVariables=>cv);
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

     --First check if we can save a lot of time if already I:b is binomial,
     -- and no quasipowers have to be taken.
     quot :=  quotient (I , sub(ideal(b),R));
     if isBinomial quot then return quot;
     
     cv := cellVars(I, cellVariables=>o#cellVariables);
     ncv := toList(set (gens R) - cv); -- non-cell variables x \notin E
 
     --Transporting the standardmonomials to R:
     stdm := nonCellstdm (I,cellVariables=>cv);
     f := map (R, ring stdm#0);
     ncvm := stdm / f ;
  
     U' := {}; -- U' as in the paper
     D  := {};
     J := ideal (0_R); -- initialize with zero ideal
     -- We map b to the correct ring to extract exponents:
     CoeffR := coefficientRing R;
     S := CoeffR(monoid [cv]); -- k[\delta] in the paper
     bexp := (exponents sub(b,S))#0 - (exponents sub(b,S))#1; -- exponent vector of b
     -- We will often need the image of bexp, so lets cache it
     bexpim := image transpose matrix {bexp};
     pc := {}; -- Will hold partial characters;
     -- Will hold the quotients compututed on the way:
     quotlist := {};
              
     for m in ncvm do(
	  quot = I:m;
	  quotlist = quotlist | {(m,quot)};
	  -- taking character
	  pc = partialCharacter (quot, cellVariables=>cv);
	  	  
	  --determine whether the exponents of b are in the saturated lattice
	  if isSubset (bexpim, image Lsat pc#"L") then (
     	       U' = U' | {m};
	       i := 1;
	       -- Computing the order of bexp in Lsat / L
	       while true do (
		    if isSubset (image transpose matrix {i * bexp} , image pc#"L") then (
			 D = D | {i};
			 break;
			 )
		    else i = i+1;
		    );
	       -- print ("The order of " | toString bexp | "in " | toString image pc#"L" | "is " | toString i);
	       -- print D;
	       );
	  ); -- loop over monomials
     -- Compute the least common multiple of the orders
     e := lcm D; -- e' in paper, but we dont need e later.
     bqp := sub (binomialQuasiPower (b,e) , R); -- e'th quasipower
     -- print ( "Least common multiple : " | toString e);
     -- print U';
     quothash := hashTable(quotlist);
     for m in U' do(
	  if bqp % quothash#m == 0 then J = J + ideal(m);		
     	  );
     return I + J;
     )     

-- Uncomment this if you are using <= 1.1
-- lcm = l -> (
--     if #l == 0 then return 1;
--     sublcm := lcm delete (l#0,l);
--     return lift(l#0 * sublcm / gcd (l#0, sublcm), ZZ);
--     )

binomialQuasiPower = (b,e) -> (
     -- returns the e-th quasipower of the binomial b
     -- i.e. (b_1)^e - (b_2)^e
     return ((terms b)#0)^e - (- (terms b)#1)^e;
     )

BCD = I -> binomialCellularDecomposition I 
BPD = I -> binomialPrimaryDecomposition I
BUD = I -> binomialUnmixedDecomposition I

binomialUnmixedDecomposition = method (Options => {verbose=>true})
binomialUnmixedDecomposition Ideal := Ideal => o -> I -> (
     if not isBinomial I then error "Input was not binomial !";
     vbopt := o#verbose;

     if vbopt then print "Running cellular decomposition:";
     cd := binomialCellularDecomposition (I, returnCellVars => true, verbose=>vbopt);
     counter := 1;
     cdc := #cd;
     bud := {};
     if vbopt then print "Decomposing cellular components:";
     scan (cd , ( (i) -> (
		    if vbopt then (
	   	    	 print ("Decomposing cellular component: " | toString counter | " of " | toString cdc);
		    	 counter = counter +1;);
		    bud = bud | cellularBinomialUnmixedDecomposition (i#0, cellVariables => i#1,verbose=>vbopt);
		    if vbopt then (
			 print "done";
			 );
		    ) -- right hand side of lambda term
	       ) -- lambda term
    	  ); -- scan
     if vbopt then print "Removing redundant components...";
     return removeRedundant (bud, verbose=>vbopt );
     )

binomialPrimaryDecomposition = method (Options => {verbose=>true})
binomialPrimaryDecomposition Ideal := Ideal => o -> I -> (
     -- The full binomial primary decomposition 
     -- starting from a not necessarily cellular binomial ideal
     
     -- TODO: Improve this! The first cellular component should always be prime. Are other too ??
     
     if not isBinomial I then error "Input was not binomial !";
     vbopt := o#verbose;
     
     if vbopt then print "Running cellular decomposition:";
     cd := binomialCellularDecomposition (I, returnCellVars => true, verbose=>vbopt);
     counter := 1;
     cdc := #cd;
     bpd := {};
     if vbopt then print "Decomposing cellular components:";
     scan (cd , ( (i) -> (
		    if vbopt then (
	   	    	 print ("Decomposing cellular component: " | toString counter | " of " | toString cdc);
		    	 counter = counter +1;);
		    bpd = bpd | cellularBinomialPrimaryDecomposition (i#0, cellVariables => i#1,verbose=>vbopt);
		    if vbopt then (
			 print "done";
			 );
		    ) -- right hand side of lambda term
	       ) -- lambda term
    	  ); -- scan
      
     bpd = joinCyclotomic bpd;
     if vbopt then print "Removing redundant components...";
     return removeRedundant (bpd, verbose=>vbopt );
     )

cellularBinomialUnmixedDecomposition = method (Options => {cellVariables => null, verbose=>true}) 
cellularBinomialUnmixedDecomposition Ideal := Ideal => o -> I -> ( 
     -- computes the unmixed decomposition of a cellular ideal
     -- I needs to be cellular. Cell variables can be given to speed up
     -- Implements algorithm 9.7 in ES96, respectively A5 in OS97
     vbopt := o#verbose;
     cv := cellVars(I, cellVariables=>o#cellVariables);
     ncv := toList (set gens ring I - cv);
     
     -- Get the associated lattices (or better characters)
     aldata := cellularAssociatedLattices (I, cellVariables=>cv);

     -- We generate a list of pairs representing characters for the lattices:
     al := for i in 0..#(aldata#1)-1 list (aldata#1#i, aldata#2#i);
     
     -- 1 Lattice: Unmixed
     if #al == 1 then return {I};

     R := ring I;
     CoeffR := coefficientRing R;
          
     -- Now find a chain among the associated lattices
     -- but finite index containment is not sufficient.
     -- We only need to check pairwise containments.
     
     pair := null;
     for l1 in al do (
	  for l2 in delete(l1,al) do (
     	       if (isSubset (image l1#0, image l2#0)) and (rank image l1#0 < rank image l2#0) then (
		    -- found a comparable pair and thus embedded primes:
		    pair = (l1,l2);
		    break;
		    )
	       )
	  );
     -- If no pair was found I is unmixed an we are done
     if pair === null then return {I};
     
     l1 := pair#0;
     l2 := pair#1;
     -- Identify a lattice vector in l2, but not l1
     L2cols := entries transpose l2#0;
     i := 0; -- Counter to identify a generator
     for col in L2cols do (
	  imc := image transpose matrix {col};
	  if rank intersect {imc, image l1#0} < 1 then (
	       -- found a vector
	       break;
	       );
	  -- Else keep going.
	  i = i+1;
	  );
     -- now i contains a suitable index
     b := sub(makeBinomial(CoeffR(monoid [cv]), L2cols#i, l2#1#i), R);
     
     -- We can't follow Section 4.1 of Ojeda/Sanchez because there is no 
     -- effictive criterion to decide that mb^[e] will never lie in I, no
     -- matter how divisible e is.
    
     -- We take the approach of computing e_b by actually coloning.
     -- Stop condition: Find a binomial b^[e] such that I:b^[e] = I:b^[e]^\infty
     -- and is binomial.
     
     e :=1;
     Itest := I:b;
     while not ((isBinomial Itest) and ( Itest: binomialQuasiPower(b,e) == Itest)) do(
	  e = e + 1;
	  Itest = I:binomialQuasiPower (b,e);
	  );
     
     -- Now we have the right quotient and the right quasi power.
     -- So we start the recursion:
     return flatten { 
	  cellularBinomialUnmixedDecomposition (Itest, cellVariables=>cv),
	  cellularBinomialUnmixedDecomposition (I+ ideal binomialQuasiPower (b,e), cellVariables=>cv)
	  };
     )

cellularBinomialPrimaryDecomposition = method (Options => {cellVariables => null, verbose=>true}) 
cellularBinomialPrimaryDecomposition Ideal := Ideal => o -> I -> ( 
     -- computes the binomial primary decomposition of a cellular ideal
     -- I needs to be cellular. Cell variables can be given to speed up
     -- Implements algorithm 9.7 in ES96, respectively A5 in OS97
     vbopt := o#verbose;
     cv := cellVars(I, cellVariables=>o#cellVariables);
     ncv := toList (set gens ring I - cv);
     ap := cellularBinomialAssociatedPrimes (I, cellVariables => cv,verbose=>vbopt);
     -- Projecting down the assoc. primes, removing monomials
     if #ncv>0 then (
     	  f := map (ring ap#0, ring ncv#0);
     	  proj := (II) -> eliminate (f \ ncv,II);
     	  ap = ap / proj ;
	  );
     R := ring ap#0; -- All associated primes live in a common ring
     J := sub (I,R); -- get I over there to compute sums
     mJ := sub (ideal product cv, R);
     -- Here, contrary to what is stated in ES'96, we can not assume that J+P is cellular.
     -- However, since Hull only wants the minimal primary component we can cellularize!
     -- TODO: Can this be skipped in some cases to be predetermined?
     return ap / ( (P) -> minimalPrimaryComponent ( saturate (P + J , mJ), cellVariables=>cv));
     )

removeRedundant = method (Options => {verbose => true})
removeRedundant List := List => o -> l -> (
     -- Removes redundant components from a list of ideals to be intersected
     -- TODO: The ideals given might live in different rings, which are 
     -- mathematically the same. We should handle this in a nice way.
     
     -- Algorithm: For each ideal in the list, remove all ideals above it.
     if #l == 0 then error "empty list given !";
     
     result := for i in l list {i,false};
     flist := for i in result list if i#1===false then i else continue;
     
     p:= Ideal;
     -- While we have not considered elements:	  
     while #(flist) > 0 do (
	  if o#verbose then << #flist << " Ideals to check" << endl;
     	  p = flist#0;
     	  result = for f in result list (
	       if isSubset (p#0,f#0) then continue
	       else f
     	  );
          -- inserting p, but flagged
     	  result = append (result,(p#0,true));
	  -- Updating the todolist
	  flist = for i in result list if i#1===false then i else continue;
	  );
     if o#verbose then << #l-#result << " redundant ideals removed. Computing mingens of result.";
     result = for i in result list ideal mingens i#0;
     return result;
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
     psols := {};
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
     Key => {binomialPrimaryDecomposition,
	  (binomialPrimaryDecomposition, Ideal),
	  [binomialPrimaryDecomposition, verbose] },
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
     Key => {binomialUnmixedDecomposition,
	  (binomialUnmixedDecomposition, Ideal),
	  [binomialUnmixedDecomposition, verbose] },
     Headline => "Binomial Unmixed Decomposition",
     Usage => "binomialUnmixedDecomposition I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          "l" => {"a list of unmixed components of I"} },
     "This routine returns an unmixed decomposition of a binomial ideal into binomial ideals.",
     EXAMPLE {
          "R = QQ[x,y,z]",
          "I = ideal (x^2, x*y, y^2, x*(z^3-1), y*(z^2-1))",
          "bud = binomialUnmixedDecomposition I",
	  "intersect bud == I"
          },
     "A synonym for this function is 'BUD'.",
     SeeAlso => BUD
     }

document {
     Key => BPD,
     Headline => "Binomial Primary Decomposition",
     "BPD is a synonym for binomialPrimaryDecomposition."
     }

document {
     Key => BUD,
     Headline => "Binomial Unmixed Decomposition",
     "BUD is a synonym for binomialUnmixedDecomposition."
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
     SeeAlso => cellularBinomialRadical
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
     SeeAlso => binomialRadical
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
     SeeAlso => binomialRadical
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
     SeeAlso => {binomialMinimalPrimes,cellularBinomialAssociatedPrimes}
     }    

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
     "A binomial ideal is prime only if it is cellular. If the cellular variables ",
     "are known they can be given via the ", TO cellVariables, " option.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-y,y^2-x)",
	  "binomialIsPrime I",
          },
     SeeAlso => {binomialIsPrimary, cellVariables}
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
     "A binomial ideal is primary only if it is cellular. If the cellular variables ",
     "are known they can be given via the ", TO cellVariables, " option. ", "If the ideal is not primary, ",
     "either 'false' or two distinct associated primes can be returned. The behaviour can be changed using the options ",
     TO returnPrimes, " and ", TO returnPChars, ".",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-1)",
	  "binomialIsPrimary (I,returnPrimes=>true)",
          },
     SeeAlso => {binomialIsPrimary, cellVariables, returnPrimes, returnPChars}
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
	  [cellularBinomialAssociatedPrimes,cellVariables],
	  [cellularBinomialAssociatedPrimes,verbose]},
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
     SeeAlso => binomialAssociatedPrimes
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
     SeeAlso => binomialAssociatedPrimes
     }    

document {
     Key => {cellularBinomialUnmixedDecomposition,
	  (cellularBinomialUnmixedDecomposition,Ideal),
	  [cellularBinomialUnmixedDecomposition,cellVariables]},
     Headline => "Unmixed decomposition of a cellular binomial ideal",
     Usage => "cellularBinomialUnmixedDecomposition I",
     Inputs => {
          "I" => { "a cellular binomial ideal"} },
     Outputs => {
          "l" => {"an unmixed decomposition of I"} },
     "If the cell variables are known, they can be given via the option ", TO cellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x*(y^3-1),x^2)",
	  "cv = isCellular (I,returnCellVars=>true)",
	  "ud = cellularBinomialUnmixedDecomposition (I,cellVariables=>cv)"
          },
     SeeAlso => binomialUnmixedDecomposition
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
          "pc" => {"the ", TO PartialCharacter, }},
     "If the cell variables are known, they can be given via the option ", TO cellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^3-1,y-x)",
	  "cv = isCellular (I,returnCellVars=>true)",
	  "pc = partialCharacter (I,cellVariables=>cv)",
          }
     }    

document {
     Key => {randomBinomialIdeal},
     Headline => "Random Binomial Ideals",
     Usage => "randomBinomialIdeal (R,n,d,w,h)",
     Inputs => {
          "I" => { "a ring for the output"},
	  "n" => { "number of generators of the output "},
	  "d" => { "maximum degree of each variable" },
	  "w" => { "number of variables in each generator "},
	  "h" => { "should the generators be 'as homogeneous as possible'"} },
     Outputs => {
          "I" => {"a random ideal"} },
     "The exponents are drawn at random from {-d,...,d}. All coefficients are set to 1.",
     EXAMPLE {
	  "R = QQ[a..x]",
	  "randomBinomialIdeal (R,6,2,4,true)",
     	  "randomBinomialIdeal (R,3,4,10,false)"
          },
     "This function is mostly for internal testing purposes. Don't expect anything from it.",
     Caveat => "Minimal generators are produced. These can be less than n and of higher degree. They also need not be homogeneous"
     }    

document {
     Key => {removeRedundant,
	  (removeRedundant,List),	  
	  [removeRedundant,verbose]},
     Headline => "Remove redundant ideals from a decomposition",
     Usage => "removeRedundant L",
     Inputs => {
          "L" => { "A list of ideals"} },
     Outputs => {
          "l" => {"A list with some redundant ideals removed"} },
     EXAMPLE {
	  "R = QQ[a,b]",
	  "L = {ideal(a^4),ideal(a^3),ideal(a^5),ideal(b^2*a) }",
	  "removeRedundant L",
          },
     "This function is mostly for internal purposes.",
     Caveat => "The resulting list is NOT irredundant, because I_1 \\subset I_2 \\cap I_3 is not checked."
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
          }
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


document {
     Key => PartialCharacter,
     Headline => "the class of all partial characters",
     
     "In ", TO Binomials , " the partial character of a cellular binomial ideal is given as an object of class,", TT "PartialCharacter", "which is
     given with the following three descriptions:",
     
     UL {
	  {"J the cellular variables"},
	  {"L a matrix whose colmns are generators for the lattice"},
	  {"c the values that the character takes on the generator"},
    
         }
        
     }


--     -- tests

TEST ///
R = QQ[a..f]
I = ideal(b*c-d*e,b*e*f-a*c,a*d*f-d*e,a*b*f-c*d,d^2*e-e,a*d*e-d*e,a*c*e-d*f) 
bpd = BPD I;
assert (intersect bpd == sub(I,ring bpd#0))
///

TEST ///
R = QQ[c,d,x,y,z,w];
I = ideal(x^3*d^2*w-c*z^2,x^5*y^2-w^7,w^3-z^8,z^2-d*w*x^7)
time bpd = binomialPrimaryDecomposition (I,verbose=>false);
assert (intersect bpd == I)
///

TEST ///
S = QQ[R00,U00,R01,D01,U01,R02,D02,L10,U10,L11,D11,U11,L12,D12];
I = ideal (U00*R01-R00*U10,R01*D11-D01*R00,D11*L10-L11*D01,
           L10*U00-U10*L11,U01*R02-R01*U11,R02*D12-D02*R01,
	   D12*L11-L12*D02,L11*U01-U11*L12);
bpd = BPD I;
assert (intersect bpd == I)
///

TEST ///
R = QQ[a..h]
I = ideal(d*g*h-e*g*h,a*b*g-c*f*h,a*b*c-e*g*h,c*f*h^2-d*f,e^2*g*h-d*h,b*d*f*h-c*g,a*d*f*g-c*e,b*c*e*g-a*f,a*b*e*f-c*d);
bpd = binomialPrimaryDecomposition (I,verbose=>false);
assert (intersect bpd == I); 
///

TEST ///
-- Cyclotomic stuff
R = QQ[x,y,z]; I = ideal (x^2*y-z^2, x^2-z^3, y^4-1); 
bpd = BPD (I,verbose=>false);
assert (intersect bpd == sub(I, ring bpd#0));
///

TEST ///
-- Unmixed Decomposition
R = QQ[x,y,z];
I = ideal (x^2, y^2, x*y, x*(z^3-1), y*(z^2-1))
bud = BUD (I, verbose=>false);
assert(intersect bud == I);
assert(dim \ flatten (associatedPrimes \ bud) == {1,0,0,0})
///
