-- -*- coding: utf-8 -*-
--  Binomials.m2
--
--  Copyright (C) 2009-2014 Thomas Kahle <thomas.kahle@jpberlin.de>
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
--  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
--
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

newPackage(
	"Binomials",
	Version => "1.2.1",
	Date => "January 2018",
	Authors => {{
		  Name => "Thomas Kahle",
		  Email => "thomas.kahle@jpberlin.de",
		  HomePage => "http://www.thomas-kahle.de"}},
    	Headline => "specialized routines for binomial ideals",
	Keywords => {"Commutative Algebra"},
	PackageImports => {"FourTiTwo", "Cyclotomic", "LLLBases", "MinimalPrimes", "Elimination"},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	     "journal URI" => "https://msp.org/jsag/",
	     "article title" => "Decompositions of binomial ideals",
	     "acceptance date" => "2012-02-06",
	     "published article URI" => "https://msp.org/jsag/2012/4-1/p01.xhtml",
	     "published article DOI" => "10.2140/jsag.2012.4.1",
	     "published code URI" => "https://msp.org/jsag/2012/4-1/jsag-v4-n1-x01-code.zip",
	     "release at publication" => "6c927c4f11724e29840c889e5ac7a426b17685ab",
	     "version at publication" => "1.0",
	     "volume number" => "4",
	     "volume URI" => "https://msp.org/jsag/2012/4-1/"
	     }
    	)
   
export {
     -- 'Official' functions
     "binomialPrimaryDecomposition",
     "binomialCellularDecomposition",
     "binomialUnmixedDecomposition",
     "binomialRadical",
     "binomialMinimalPrimes",
     "binomialAssociatedPrimes",
     "binomialSolve",
     -- tests
     "binomialIsPrime",
     "binomialIsPrimary",
     "cellularBinomialIsPrimary",
     "isCellular",
     "isBinomial",
     "isUnital",
     -- input related
     "makeBinomial",
     "latticeBasisIdeal",
     -- cellular stuff:
     "cellularBinomialAssociatedPrimes",
     "cellularBinomialUnmixedDecomposition",
     -- "cellularAssociatedLattices",
     "cellularBinomialPrimaryDecomposition",
     "cellularBinomialRadical",
     -- simple wrappers:
     "BPD",
     "BCD",
     "BUD",
     -- auxiliary functions:
     "partialCharacter",
     "idealFromCharacter",  -- should be renamed to ideal once M2 supports this
     "randomBinomialIdeal",
     "extractInclusionMinimalIdeals",
     -- Not in the interface:
--     "axisSaturate",
--     "cellVars",
--     "cellularEmbeddedLatticeWitnesses",
--     "Lsat",
--     "saturatePChar",
--     "satIdeals",
--     "nonCellstdm",
--     "maxNonCellstdm",
--     "minimalPrimaryComponent",
--     "binomialFrobeniusPower",

     -- Options
     "CellVariables", -- for partialCharacter
     "ReturnPrimes", -- for cellularBinomialIsPrimary 
     "ReturnPChars", -- for cellularBinomialIsPrimary
     "ReturnCellVars", -- for binomialCellularDecomposition
     
     --Types
     "PartialCharacter"--HashTable
     }

axisSaturate = (I,i) -> (
-- By Ignacio Ojeda and Mike Stillman
-- For computing saturations w.r.t. a single variable:
    R := ring I;
    I1 := ideal(1_R);
    s := 0;
    f := R_i;
    while not(I1 == I) do (
	s = s + 1;
	I1 = I;
	I = ideal syz gb(matrix{{f}}|gens I,
            SyzygyRows=>1,Syzygies=>true););
    {s-1, I}
    )

-- Cellular decomposition of binomial ideals:
binomialCellularDecomposition = method (Options => {ReturnCellVars => false, Verbose=>false})
binomialCellularDecomposition Ideal := Ideal => o -> I -> (
-- based on code by Ignacio Ojeda and Mike Stillman     
     R := ring I;
     if I == ideal (1_R) then return {};
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
		   if o#Verbose then (
			<< "redundant component" << endl;
			)
		   )
	      -- if its not redundant:
	      else if #(L#1) === 0 then ( -- #(L#1) counts 'remaining variables to check'
		   -- no variables remain to check :
		   -- We have an answer
                   compo = compo + 1; 
		   newone := trim L#2;
		   if o#Verbose then (
			<< "cellular components found: " << compo << endl;);
		   if o#ReturnCellVars then Answer = append(Answer,{newone, delete(1_R,L#0)})
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

isCellular = method (Options => {ReturnCellVars => false})
isCellular Ideal := Ideal => o -> I -> (
     -- This function checks if a binomial ideal is cellular
     -- In the affirmative case it can return the cellular (regular) variables.
     R := ring I;
     cv := cellVars I;
     if cv == {} then prod := 1_R else prod = product cv;
     if I == saturate (I, prod) then (
	  -- I is cellular 
	  if o#ReturnCellVars then return cv
	  else return true;
	  )
     else false)

cellVars = method (Options => {CellVariables => null})
cellVars Ideal := Ideal => o -> I -> (
     -- This function computes the cell variables for a cellular ideal if necessary.
     if o#CellVariables === null then (
	  cv := {};
	  for i in gens ring I do if saturate (I,i) != substitute(ideal(1), ring I) then cv=cv|{i};
	  return cv;
	  )
     else o#CellVariables)

--  Setting up the PartialCharacter Type
PartialCharacter = new Type of HashTable;

partialCharacter = method (Options => {CellVariables => null})
partialCharacter Ideal := Ideal => o -> I -> (
     -- Will compute the partial character associated to a cellular binomial ideal.
     -- If the cell variables are known they can be given as an optional argument.
     
     vs := {}; -- This will hold the lattice generators
     vsmat := matrix "0"; -- Holds the matrix whose image is L 
     cl := {}; -- This will hold the coefficients
     R := ring I;
     CoeffR := coefficientRing R; -- needed to form terms below
     II := ideal;
     
     -- The input should be a cellular ideal 
     cv := cellVars(I, CellVariables=>o#CellVariables);
     ncv := toList (set gens R - cv);
     
     -- If there are no cellular variables, 
     -- the ideal is monomial and the partial character is zero:
     if cv == {} then (
	  return new PartialCharacter from {"J"=>{}, "L"=>matrix "0", "c"=>{1}};
	  );
     
     -- We need to construct this ring to properly extract coefficients below
     S := CoeffR(monoid [cv]);
     
     -- intersect I with the ring k[cv] and map to S
     if #ncv != 0 then (
     	  II = sub(eliminate (ncv, I),S);
	  )
     else (
	  -- S = R, stick with original def!
	  II = I;
	  );

     -- The partial character of the zero ideal is on the zero lattice.
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
     -- Inputs: a ring R, the number of generators numgen, the maximal degree of each variable maxdeg,
     -- the maximal number of variables appearing in binomial, whether the output should be homogeneous
     
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
     ideal (mingens ideal(ge)))

isBinomial = I -> (
     -- Checking binomiality with a reduced gb.
     ge := flatten entries gens gb I;
     for g in ge do (
          if #(terms g) > 2 then return false;
	  );
     true)

isUnital = I -> (
     -- A unital ideal is generated by unital binomials and monomials.
     ge := flatten entries gens gb I;
     for g in ge do (
	  coeffs := sort flatten entries (coefficients g)#1;
	  if coeffs == {1} then continue;
	  if coeffs == { -1} then continue;
	  if coeffs == { -1 , 1} then continue;
	  return false;
	  );
     true)

     
nonCellstdm = {CellVariables=>null} >> o -> I -> (
     -- extracts the (finite) set of nilpotent monomials 
     -- modulo a cellular binomial ideal.
     R := ring I;

     cv := set cellVars(I, CellVariables=>o#CellVariables);
     -- Extracting  the monomials in the non-Cell variables.
     -- Problem: They may not live in R because R was extended on the way.
     -- This use of baseName is intended to fix a problem where the variables in cv 
     -- are actual variables of a ring over a field extension.
     ncv := value \ toList (set (baseName \ (gens R)) - baseName \ cv);
     
     -- We map I to the subring: kk[ncv]
     CoeffR := coefficientRing R;
     S := CoeffR(monoid [ncv]);
     J := kernel map (R/I,S); 
     basis (S/J))

maxNonCellstdm = {CellVariables=>null} >> o -> I -> (
     -- Computes the maximal monomials in the nilpotent variables

     cv := cellVars(I, CellVariables=>o#CellVariables);
     nm := flatten entries nonCellstdm (I,CellVariables=>cv);
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
     result)

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
     posmon - c*negmon)

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
     binomials := null;
     c := null;
     k := ring pc#"c"#0;
     
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
     else if set pc#"c" === set {1_k} then (
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
     ideal binomials)

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
     -- The following is the smallest ring containing all new
     -- coefficients but not smaller than QQ
     F := ring satpc#0#"c"#0;
     if F === ZZ then F = QQ;
     Q := F(monoid [satpc#0#"J"]);
     for s in satpc list idealFromCharacter (Q, s))

binomialRadical = I -> (
     cv := isCellular (I, ReturnCellVars=>true);
     if not cv === false then (
	  return cellularBinomialRadical (I,CellVariables=>cv)
	  );
     -- In the general case
     print "Input not cellular, computing minimal primes ...";
     mp := binomialMinimalPrimes I;
     ideal mingens intersect mp)

cellularBinomialRadical = method (Options => {CellVariables => null}) 
cellularBinomialRadical Ideal := Ideal => o -> I -> (
     -- Computes the radical of a cellular binomial ideal
     R := ring I;
     cv := cellVars(I, CellVariables=>o#CellVariables);
     -- Get the non-cellular variables
     noncellvars := toList(set (gens R) - cv);
     M := sub (ideal (noncellvars),R);
     I + M)

binomialIsPrimary = I -> (
     -- Check if an arbitrary binomial ideal is primary
     -- first check for cellularity, then run the specialized check if the ideal is cellular.
     cv := isCellular (I, ReturnCellVars=>true);
     -- Can't check with logical comparison because the return value could be a list
     if cv === false then return false
     else cellularBinomialIsPrimary (I, CellVariables=>cv))

cellularBinomialIsPrimary = method (Options => {ReturnPrimes => false , ReturnPChars => false, CellVariables=> null})
cellularBinomialIsPrimary Ideal := Ideal => o -> I -> (
     -- Implements Alg. 9.4 in [ES96]
     -- I must be a cellular ideal, CellVariables can be given for speedup
     -- Returns the radical of I and whether I is primary
     -- if the option ReturnPrimes is true, then it will return 
     -- the radical in the affirmative case and two distinct associated primes
     -- otherwise
     -- if the option ReturnPChars is true then it will return partial Characters 
     -- of the primes instead. 
     -- If both are true then it will return characters.
     
     R := ring I;
     -- Only proper ideals are considered primary
     if I == ideal(1_R) then return false;      
     
     -- Handling of cell variables
     cv := cellVars(I, CellVariables=>o#CellVariables);

     -- Get the partial character of I
     pc := partialCharacter(I, CellVariables=>cv);
     noncellvars := toList(set gens R - cv);
     
     M := sub (ideal (noncellvars),R);
     -- the radical:
     rad := I + M;
     
     -- If the partial character is not saturated, the radical is not prime
     if image Lsat pc#"L" != image pc#"L" then (
	  print "The radical is not prime, as the character is not saturated";
	  satpc := saturatePChar pc;
	  if o#ReturnPChars then (
	       -- This one is the fastest, so check it first
	       return {{satpc#0#"J",satpc#0#"L",satpc#0#"c"}, {satpc#0#"J",satpc#0#"L",satpc#1#"c"}}
	       );
	  if o#ReturnPrimes then (
     	       F := ring satpc#0#"c"#0;
     	       S := F(monoid [satpc#0#"J"]);
	       M = sub(M,S);
	       ap1 := idealFromCharacter (S,satpc#0) + M;
	       ap2 := idealFromCharacter (S,satpc#1) + M;
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
     maxlist := maxNonCellstdm (I,CellVariables=>cv);
     -- need to map to R to do colons with I
     f := map (R, ring maxlist#0);
     maxstdmon := maxlist / f;
     
     for m in maxstdmon do (
	  q := I:m;
	  -- Mapping down to k[E]:
--	  q2 := eliminate (noncellvars,q);
     	  q2 := q + M;
     	  -- I_+(sigma) was called prerad above:
	  if not isSubset(q2, rad) then (
	       -- creating some local names:
	       satqchar := saturatePChar partialCharacter (q,CellVariables=>cv);
	       if o#ReturnPChars then(
		    return {pc, {satqchar#0#"J",satqchar#0#"L",satqchar#0#"c"}}
		    );
	       if o#ReturnPrimes then (
		    F := ring satqchar#0#"c"#0;
     	       	    S := F(monoid [satqchar#0#"J"]);
	       	    M = sub(M,S);
		    ap2 := idealFromCharacter (S, satqchar);
		    return {rad, ap2 + M};
     	       	    )
	       else return false;
	       );
	  );
     if o#ReturnPChars then return {pc};
     if o#ReturnPrimes then return {rad};
     true)

binomialIsPrime = method (Options => {CellVariables=>null})
binomialIsPrime Ideal := Ideal => o -> I -> ( 
     -- A binomial ideal is prime if it is cellular and all its
     -- monomial generators have power one and the associated partial
     -- character is saturated.  (Corollary 2.6 in ES96 )

     -- Input: A Binomial Ideal and the cell variables if the ideal is cellular.
     -- Output: true if the ideal is a prime ideal, false otherwise
     
     -- test for cellularity:
     -- if cellular variables are given then we believe that I is cellular
     cv := null;
     if o#CellVariables === null then (
	  cv = isCellular (I, ReturnCellVars=>true);
     	  if cv === false  then return false)
     else cv = o#CellVariables;
     
     -- Check if non-cellular variables are all contained:
     R := ring I;
     ncv := toList(set (gens R) - cv); -- nilpotent variables x \notin E
     if not isSubset(promote(ideal ncv, R), I) then return false;

     -- Test if the partial character saturated:
     pc := partialCharacter (I, CellVariables=>cv);
     if image Lsat pc#"L" != image pc#"L" then return false;
     true)

binomialMinimalPrimes = method (Options => {Verbose=>false})
binomialMinimalPrimes Ideal := Ideal => o -> I -> (
     -- Algorithm from "Decompositions of Binomial Ideals" (AISM), 
     -- based on computing a cellular decomposition of the radical of I.
     if not isBinomial I then error "Input was not binomial";
     R := ring I;
     if I == ideal (1_R) then return {};
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
		   if o#Verbose then (
			<< "redundant component" << endl;
			);
		   )
	      else if #(L#1) === 0 then ( -- #(L#1) counts 'remaining variables to check'
                   compo = compo + 1; 		
		   newone := trim L#2;
		   if o#Verbose then (
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
     
     if o#Verbose then print "Decomposition done.";
          
     ncv := {};
     i := 0;
     j := #Answer;
     ME :=ideal; -* pc = {}; *- si := ideal; mp := {}; F := null; S:= null;
     for a in Answer do (
	  i = i+1;
	  if o#Verbose  then (
	       print ("Finding minimal primes of cellular component: " | toString i | " of " | toString j));
	  ME := ideal(toList(set (gens R) - a#1));
	  pc := partialCharacter (a#0, CellVariables=>a#1);
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

     extractInclusionMinimalIdeals (joinCyclotomic mp, Verbose=>o#Verbose))

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
     false)

cellularBinomialAssociatedPrimes = method (Options => {CellVariables => null, Verbose=>false}) 
cellularBinomialAssociatedPrimes Ideal := Ideal => o -> I -> ( 
     -- Computes the associated primes of cellular binomial ideal
     -- It returns them in a polynomial ring with the same variables as ring I,
     -- but potentially extended coefficient ring.

     R := ring I;
     
     cv := cellVars(I, CellVariables=>o#CellVariables);
          
     primes := {}; -- This will hold the list of primes
     ncv := toList(set (gens R) - cv); -- nilpotent variables x \notin E
     stdm := nonCellstdm(I,CellVariables=>cv); -- List of std monomials in ncv
     -- mapping to R:
     f := map (R, ring stdm);
     ml := flatten entries f stdm;
     
     if o#Verbose then(
	  if #ml == 1 then << "1 monomial to consider for this cellular component " << endl
     	  else <<  #ml << " monomials to consider for this cellular component" << endl;
	  );
     
     -- For a given partialCharacter, this hash table saves a witness monomial,
     -- and the corresponding lattice ideal.  The ideal is saved to potentially
     -- skip the ideal saturation further down.
     seenpc := new MutableHashTable;

     -- A dummy ideal and partial Characters:
     Im := ideal;
     pC := {}; sat := {};
     -- save 1 as the bottom witness
     seenpc#(partialCharacter (I, CellVariables=>cv))=({1_R}, I);
     todolist := delete(1_R, ml);
     -- While we have monomials to check
     while #todolist > 0 do (
--	  print ("On todolist: " | toString (#todolist));
	  -- sample a random monomial:
	  i := random(0, #todolist-1);
	  m := todolist#i;
	  Im = I:m;
	  pC = partialCharacter(Im, CellVariables=>cv);
	  if seenpc#?pC then (
	       -- We have seen this lattice: Time to prune the todolist
--	       print ("Todolist items before: " | toString (#todolist));
	       for n in seenpc#pC#0 do (
		    todolist = select (todolist , (mm -> not isBetween (mm, n, m))
		    ));
--	       print ("Todolist items after: " | toString (#todolist));
	       -- add m to the pruning list
	       todolist = delete(m, todolist);
	       addmon := true;
	       for mmm in seenpc#pC#0 do if m%mmm==0 then (addmon = false; break);
	       if addmon then seenpc#pC = (seenpc#pC#0 | {m}, seenpc#pC#1);
--	       print (#(seenpc#pC));
	       )   
	  else (
	       -- a new associated lattice
	       seenpc#pC = ({m} , Im);
	       todolist = delete(m, todolist);
	       )
	  );
--     print ("Todolist items: " | toString (#todolist));
     for pc in keys seenpc do (
	  -- If the lattice of pc is saturated, then we can skip the saturation (which would compute
	  -- a Markov basis (slow))
	  if image pc#"L" == image Lsat pc#"L" then (
	       primes = primes | {cellularBinomialRadical (seenpc#pc#1, CellVariables=>pc#"J")}
	       )
	  else (
	       -- need to actually saturate and potentially extend coefficients
	       sat = satIdeals pc;
	       -- If the coefficientRing is QQ, we map back to R
	       F := coefficientRing ring sat#0;
	       if F === QQ then (
	       	    f = map (R, ring sat#0);
	       	    sat = sat / f ;
	       	    )
	       else (
	       	    -- otherwise to the extended ring
	       	    -- this is necessary since satIdeals does not know about the nilpotent variables
	       	    S := F monoid R;
	       	    f = map (S, ring sat#0);
	       	    sat = sat / f;
	       	    );
	       primes = primes | sat;
	       )
	  );
     -- We need to remove duplicate elements and join all associated primes in an appropriate new ring that contains all
     -- their coefficients.
     primes = joinCyclotomic primes;
     M := sub (ideal ncv, ring primes#0);
     primes = primes / (I -> I + M);

     -- Computation of mingens is necessary as unique or toList + set combi won't do without
     unique (ideal \ mingens \ primes))

binomialAssociatedPrimes = I -> (
     if not isBinomial I then error "Input not binomial";
     cv := isCellular (I,ReturnCellVars=>true);
     if cv === false then (
	  print "Not yet implemented";
	  print "I will compute a primary decomposition and take radicals!";
	  bpd := BPD I;
	  print "Primary Decomposition found, taking radicals now:";
	  return binomialRadical \ bpd;
	  )
     else cellularBinomialAssociatedPrimes (I, CellVariables=>cv))

cellularAssociatedLattices = method (Options => {CellVariables => null})
cellularAssociatedLattices Ideal := Ideal => o -> I -> (
     -- Computes the associated lattices of a cellular binomial ideal
     -- WARNING: The definition might differ from the final definition in [KM11]
     
     R := ring I;
     cv := cellVars(I, CellVariables=>o#CellVariables);
     lats := {}; -- This will hold the list of lattices
     coeffs := {}; -- This will hold the values of the characters
     ncv := toList(set (gens R) - cv); -- nilpotent variables x \notin E
     -- print "Noncellvars"; print ncv;
     ml := flatten entries nonCellstdm(I,CellVariables=>cv); -- List of std monomials in ncv
     -- Coercing to R:
     f := map (R, ring ml#0);
     ml = ml/f;
     -- A dummy ideal and partial Characters:
     Im := ideal;
     pc := {};
     redundant := true;
     -- For each monomial, check if I:m shows an unseen lattice.
     for m in ml do (
	  -- print m;
	  Im = I:m;
	  -- We already know the cell variables in the following computation
	  pc = partialCharacter(Im, CellVariables=>cv);
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
     {cv, lats, coeffs}) -- CellularAssociatedLattices

cellularEmbeddedLatticeWitnesses = method (Options => {CellVariables => null})
cellularEmbeddedLatticeWitnesses Ideal := Ideal => o -> I -> (
     -- Given a cellular binomial ideal whose radical is prime this
     -- function will produce witness monomials for embedded
     -- lattices.  Throw in these monomials to get rid of
     -- additional associated primes, i.e. compute Hull.

     R := ring I;
     cv := cellVars(I, CellVariables=>o#CellVariables);
     witnesses := {};
     lats := {}; -- This will hold the list of lattices
     ncv := toList(set (gens R) - cv); -- nilpotent variables x \notin E
     ml := flatten entries nonCellstdm(I,CellVariables=>cv); -- List of std monomials in ncv
     -- Coercing to R:
     f := map (R, ring ml#0);
     ml = ml / f;
     -- A dummy ideal and partial Characters:
     Im := ideal;
     pc := {};
     redundant := true;
     bottomlattice := partialCharacter (I, CellVariables=>cv);
     -- For each monomial, check if I:m has a different lattice:
     todolist := ml;
--     print ("Number of monomials to consider : " | toString (#todolist));
     while (#todolist > 0) do (
	  i := random(0, #todolist-1); -- a random monomial
	  m := todolist#i;
	  -- Now two possibilities:
	  -- if m witnesses an embedded lattice: Remove everything that is above m;
	  Im = I:m;
	  pc = partialCharacter(Im, CellVariables=>cv);
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
     witnesses) -- cellularEmbeddedLatticeWitnesses


minimalPrimaryComponent = method (Options => {CellVariables => null})
minimalPrimaryComponent Ideal := Ideal => o -> I -> (
     -- Input a cellular binomial ideal whose radical is prime.
     -- Output, generators for Hull(I)

     cv := cellVars(I, CellVariables=>o#CellVariables);
     if cv === false then error "Input to minimalPrimaryComponent was not cellular!";

     I + ideal (cellularEmbeddedLatticeWitnesses (I, CellVariables=>cv)))


binomialFrobeniusPower = (b,e) -> (
     -- returns the e-th Frobenius power of the binomial b
     -- i.e. (b_1)^e - (b_2)^e
     ((terms b)#0)^e - (- (terms b)#1)^e)

BCD = I -> binomialCellularDecomposition I 
BPD = I -> binomialPrimaryDecomposition I
BUD = I -> binomialUnmixedDecomposition I

binomialUnmixedDecomposition = method (Options => {Verbose=>false})
binomialUnmixedDecomposition Ideal := Ideal => o -> I -> (
     if not isBinomial I then error "Input was not binomial !";
     vbopt := o#Verbose;

     if vbopt then print "Running cellular decomposition:";
     cd := binomialCellularDecomposition (I, ReturnCellVars => true, Verbose=>vbopt);
     counter := 1;
     cdc := #cd;
     bud := {};
     if vbopt then print "Decomposing cellular components:";
     scan (cd , ( (i) -> (
		    if vbopt then (
	   	    	 print ("Decomposing cellular component: " | toString counter | " of " | toString cdc);
		    	 counter = counter +1;);
		    bud = bud | cellularBinomialUnmixedDecomposition (i#0, CellVariables => i#1,Verbose=>vbopt);
		    if vbopt then (
			 print "done";
			 );
		    ) -- right hand side of lambda term
	       ) -- lambda term
    	  ); -- scan
     if vbopt then print "Removing some redundant components...";
     -- In principle this does not make the intersection irredundant,
     -- but we don't want to run an exponential algorithm at this
     -- point.
     extractInclusionMinimalIdeals (bud, Verbose=>vbopt))

binomialPrimaryDecomposition = method (Options => {Verbose=>false})
binomialPrimaryDecomposition Ideal := Ideal => o -> I -> (
     -- The full binomial primary decomposition 
     -- starting from a not necessarily cellular binomial ideal
     
     if not isBinomial I then error "Input was not binomial !";
     if I == ideal (1_(ring I)) then return {};

     vbopt := o#Verbose;
     if vbopt then print "Running cellular decomposition:";
     cd := binomialCellularDecomposition (I, ReturnCellVars => true, Verbose=>vbopt);
     counter := 1;
     cdc := #cd;
     bpd := {};
     if vbopt then print "Decomposing cellular components:";
     scan (cd , ( (i) -> (
		    if vbopt then (
	   	    	 print ("Decomposing cellular component: " | toString counter | " of " | toString cdc);
		    	 counter = counter +1;);
		    bpd = bpd | cellularBinomialPrimaryDecomposition (i#0, CellVariables => i#1,Verbose=>vbopt);
		    if vbopt then (
			 print "done";
			 );
		    ) -- right hand side of lambda term
	       ) -- lambda term
    	  ); -- scan
      
     bpd = joinCyclotomic bpd;
     if vbopt then print "Removing some redundant components...";
     -- In principle this does not make the intersection irredundant,
     -- but we don't want to run an exponential algorithm at this
     -- point.
     extractInclusionMinimalIdeals (bpd, Verbose=>vbopt))

cellularBinomialUnmixedDecomposition = method (Options => {CellVariables => null, Verbose=>false}) 
cellularBinomialUnmixedDecomposition Ideal := Ideal => o -> I -> ( 
     -- computes the unmixed decomposition of a cellular ideal
     -- as defined in Ojeda/Sanchez
     vbopt := o#Verbose;
     cv := cellVars(I, CellVariables=>o#CellVariables);
     ncv := toList (set gens ring I - cv);
     
     -- Get the associated lattices (or better characters)
     aldata := cellularAssociatedLattices (I, CellVariables=>cv);

     -- We generate a list of pairs representing characters for the lattices:
     al := for i in 0..#(aldata#1)-1 list (aldata#1#i, aldata#2#i);
     
     -- if there is only one lattice, then I was unmixed
     if #al == 1 then return {I};

     R := ring I;
     CoeffR := coefficientRing R;
          
     -- Now find a chain among the associated lattices
     -- but finite index containment is not sufficient.
     -- We only need to check pairwise containments.
     pair := null;
     for l1 in al do (
	  -- break out if the inner loop assigned a pair:
	  if pair === null then break;
	  for l2 in delete(l1,al) do (
     	       if (isSubset (image l1#0, image l2#0)) and (rank image l1#0 < rank image l2#0) then (
		    -- found a comparable pair and thus embedded primes:
		    pair = (l1,l2);
		    -- break the inner loop.
		    break)));
     -- If no pair was found, then I is unmixed an we are done
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
     -- effective criterion to decide that mb^[e] will never lie in I, no
     -- matter how divisible e is.
    
     -- We take the approach of computing e_b by actually coloning.
     -- Stop condition: Find a binomial b^[e] such that I:b^[e] = I:b^[e]^\infty
     -- and this ideal is binomial.
     e :=1;
     Itest := I:b;
     while not ((isBinomial Itest) and ( Itest: binomialFrobeniusPower(b,e) == Itest)) do(
	  e = e + 1;
	  Itest = I:binomialFrobeniusPower (b,e);
	  );
     
     -- Now we have the right quotient and the right Frobenius power.
     -- So we start the recursion:
     flatten { 
	  cellularBinomialUnmixedDecomposition (Itest, CellVariables=>cv),
	  cellularBinomialUnmixedDecomposition (I + ideal binomialFrobeniusPower (b,e), CellVariables=>cv)
	  })

cellularBinomialPrimaryDecomposition = method (Options => {CellVariables => null, Verbose=>false}) 
cellularBinomialPrimaryDecomposition Ideal := Ideal => o -> I -> ( 
     -- computes the binomial primary decomposition of a cellular ideal
     -- I needs to be cellular. Cell variables can be given to speed up
     -- Implements algorithm 9.7 in ES96, respectively A5 in OS97
     vbopt := o#Verbose;
     cv := cellVars(I, CellVariables=>o#CellVariables);
     ncv := toList (set gens ring I - cv);
     ap := cellularBinomialAssociatedPrimes (I, CellVariables => cv,Verbose=>vbopt);
     -- If cv coincides with gens R, then the associated primes are their own minimal primary
     -- components (since in characteristic zero lattice ideals are radical):
     if #ncv == 0 then return ap
     else (
     	  -- Remove monomials from associated primes to get the lattice ideals
     	  f := map (ring ap#0, ring ncv#0);
     	  proj := (II) -> eliminate (f \ ncv,II);
     	  ap = ap / proj
	  );
     R := ring ap#0; -- All associated primes live in a common ring
     J := sub (I,R); -- get I over there to compute sums
     -- Here, contrary to what is stated in ES'96, we can not assume that J+ap#i is cellular.
     -- However, since Hull only wants the minimal primary component we can cellularize.

     -- Saturate product cv or saturate variable by variable?
     -- It seems to depend on the example which one is faster :(
--      cvsaturate := (p) -> (
-- 	  todo := cv;
-- 	  resu := p;
-- 	  while #todo > 0 do (
-- 	       resu = saturate (resu, sub(todo#0, R));
-- 	       todo = drop(todo,1)
-- 	       );
-- 	  resu);
     cvsaturate := (p) -> saturate (p, sub (product cv, R));
     ap / ( (P) -> minimalPrimaryComponent ( cvsaturate (P + J), CellVariables=>cv)))

extractInclusionMinimalIdeals = method (Options => {Verbose=>false})
extractInclusionMinimalIdeals List := List => o -> l -> (
    -- Computes the inclusion minimal elements in a list of ideals
    -- (like the minimal primes) Algorithm: For each ideal in the
    -- list, remove all ideals above it.  Note: This does not make an
    -- arbitrary intersection of ideals irredundant.  For example it
    -- would not reduce <x-y> \cap <x,y^2> \cap <x^2,y> where each of
    -- the last two components is redundant given the other two.

     if #l == 0 then return {};
     
     -- List to store the result, the flag marks elements that have been treated.
     result := for i in l list (i,false);

     -- While we have previously unconsidered elements:
     unconsidered := #l;
     while unconsidered > 0 do (
	 if o#Verbose then << unconsidered << " Ideals to check" << endl;
     	 p := (select(1, result, p -> p#1==false))#0 ; -- select returns list
	 result = for f in result list (
	     -- Check if p is contained in f which makes f redundant
	     if isSubset (p#0,f#0) then continue
	     else f);
         -- insert p again (flagged true) since it was removed before.
	 result = append (result,(p#0,true));
	 unconsidered = #(select (result, pp -> pp#1==false)));
     if o#Verbose then << #l-#result << " redundant ideals removed. Computing mingens of result.";
     for i in result list ideal mingens i#0)

-- The remaining code implements the solver for zero-dimensional unital
-- binomial ideals . We solve unital binomial equations using
-- modulo 1 arithmetic. The basic task is to solve a^n = 1^{k/m},
-- whose solutions are the equivalence classes of: k/nm, 1/n +
-- k/m, 2/n + k/nm,... , (n-1)/n + k/nm
          
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
     roots)

SolveMore = (binom,psol) -> (
     -- This function extends a partial solution further
     -- INPUT: A partial solution and a binomial which after plugging
     -- in the partial solutions is univariate
     -- OUTPUT: An extended partial solution

     -- Since Lex is a global order the true monomial comes first.
     mon := (terms binom)#0; -- The monomial in the new variable.
     
     -- we need the index of the variable that we will solve now
     -- <incomprehensable hack>
     ind := index (flatten entries gens radical monomialIdeal mon)#0;
     var := (flatten entries gens radical monomialIdeal mon)#0;
     -- </incomprehensable hack>
     
     rhs := (terms binom)#1; -- The right hand side which is a power
			     -- of a root of unity
			     
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

       	   -- now everything is set for the Rooter:
       	   roots = roots | Rooter (n,q);
       	   extensions := for r in roots list (
	    	for i in 0..#onesol-1 list if i==ind then r else onesol#i
	    	);
       	   newsols = newsols | extensions;
       	   );
      newsols)

binomialSolve = I -> (
     -- solver for zero-dim'l unital difference Binomial
     -- Ideals INPUT: I, the ideal    
     -- OUTPUT: The list of solutions in QQ(some root of unity)
     -- Note: Solutions will be returned with multiplicities.
     if not isUnital I then (
	  error "Sorry, only implemented for unital binomial ideals";
	  );
	  
     R := ring I;
     cd := binomialCellularDecomposition (I,ReturnCellVars=>true,Verbose=>false);
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
     -- This auxiliary function maps a quotient from QQ to its
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
     pack (#(gens ring I),sols))

cellularBinomialExponentSolve = (I,cv) -> (
     -- Solves a zero dimensional cellular unital binomial ideal
     -- by constructing the appropriate cyclotomic field
     
     -- Input: a unital zero-dim'l binomial ideal and its list of
     -- cell variables
     
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

     -- For each variable we check if it is a nilpotent variable, i.e.
     -- each solution of the ideal has coordinate zero there
     -- We also check how often we have to duplicate each solution in the
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
     flatten psols)

-- End of source code ---

beginDocumentation()

document {
        Key => Binomials,
        Headline => "a package for binomial ideals",

        EM "Binomials", " is a package for binomial ideals with a particular
        focus on intersection decompositions and associated primes.  For
        instance, if the input is a unital binomial ideal (that is generated
        by monomials and differences of monomials) then the function",
        TO binomialPrimaryDecomposition, "computes a primary decomposition into
        binomial ideals. To this end a cyclotomic field extension of the
        coefficient field may be necessary which is automatically constructed
        using the package ",TO Cyclotomic, ".", EM " Binomials", " also
        implements the data type ", TO partialCharacter, " (see [ES96]) and
        several convenience functions to transform binomials into exponent
        vectors and vice versa.  Those may be useful for manual inspection of
        binomial ideals.", "There is no special datatype for binomial ideals
        implemented, one just uses ", TO ideal, "s.",
	
	BR{},BR{},
	BOLD "Literature \n",
	UL {
	  LI {"[ES96] ", EM "Binomial ideals ", "(D. Eisenbud, B.Sturmfels, 1996).\n"},
	  LI {"[DMM10] ", EM "Combinatorics of binomial primary decomposition ", "(A. Dickenstein, L. Matusevich, E.Miller, 2010)\n"},
	  LI {"[OS00] ", EM "Cellular Binomial Ideals. Primary Decomposition of Binomial Ideals ", "(I. Ojeda, R. Piedra-Sanchez, 2000)\n"},
	  LI {"[Alt00] ", EM "The chain property for the associated primes of A-graded ideals ", "(K. Altmann, 2000)\n"},
	  LI {"[KM11] ", EM "Decompositions of commutative monoid congruences and binomial ideals ", "(T. Kahle, E. Miller, 2011)"}}}
   
document {
     Key => {binomialPrimaryDecomposition,
	  (binomialPrimaryDecomposition, Ideal)},
     Headline => "Binomial Primary Decomposition",
     Usage => "binomialPrimaryDecomposition I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          {"a list of binomial primary components of I"} },
     "This routine returns a primary decomposition of I into binomial ideals.",
     EXAMPLE {
          "R = QQ[x,y,z]",
          "I = ideal (x*y-z, x*z-y^2)",
          "bpd = binomialPrimaryDecomposition I",
	  "intersect bpd == I"
          },
     "A synonym for this function is ", TO BPD, ".",
     Caveat => {"Currently it can not be guaranteed that the decomposition is irredundant, although serious attempts are made to reduce redundancy."},
     SeeAlso => BPD}

document {
     Key => {binomialUnmixedDecomposition,
	  (binomialUnmixedDecomposition, Ideal)},
     Headline => "Binomial Unmixed Decomposition",
     Usage => "binomialUnmixedDecomposition I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          {"a list of unmixed components of I"} },
     "This routine returns an unmixed decomposition of a binomial ideal into binomial ideals. 
     The implemented algorithm is a variant of Algorithm 4 in [OS00].",
     EXAMPLE {
          "R = QQ[x,y,z]",
          "I = ideal (x^2, x*y, y^2, x*(z^3-1), y*(z^2-1))",
          "bud = binomialUnmixedDecomposition I",
	  "intersect bud == I"
          },
     "A synonym for this function is ", TO BUD, ".",
     Caveat=> "Apart from unmixedness, properties of the output decomposition are 
     defined only by the course of the algorithm, in particular it 
     is not mesoprimary decomposition of [KM11].",
     SeeAlso => BUD}

document {
     Key => BPD,
     Headline => "Binomial Primary Decomposition",
     "BPD is a synonym for ", TO binomialPrimaryDecomposition, "."}

document {
     Key => BUD,
     Headline => "Binomial Unmixed Decomposition",
     "BUD is a synonym for ", TO binomialUnmixedDecomposition, "."}

document {
     Key => {binomialCellularDecomposition,
          (binomialCellularDecomposition,Ideal),
	  [binomialCellularDecomposition,ReturnCellVars]},
     Headline => "Binomial Cellular Decomposition",
     Usage => "binomialCellularDecomposition I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          {"a list of cellular ideals whose intersection is I or 
	    a list of pairs of these ideals and their cellular variables 
	    if the option ReturnCellVars => true is used"} },
     "A binomial ideal I is called cellular if modulo I every variable in 
     the polynomial ring is either a non-zerodivisor or nilpotent. 
     This routine returns a minimal cellular decomposition of a 
     binomial ideal.",
     EXAMPLE {
          "R = QQ[x,y,z]",
          "I = ideal (x*y-z, x*z-y^2)",
          "bcd = binomialCellularDecomposition I",
	  "intersect bcd == I",
     	  "binomialCellularDecomposition (I, ReturnCellVars=>true, Verbose=>false)"
          },
     "A synonym for this function is ", TO BCD, ".",
     "If the option ", TO Verbose, " is set (default), then output about the 
     number of components found so far will be generated.",
     SeeAlso => BCD}

document {
     Key => BCD,
     Headline => "Binomial Cellular Decomposition",
     "BCD is a synonym for ", TO binomialCellularDecomposition ,"."}

document {
     Key => {binomialRadical},
     Headline => "Radical of a binomial ideal",
     Usage => "binomialRadical I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
	  {"the radical of I"}},
     "If the input is a cellular binomial ideal then a very fast algorithm is used. 
     If one knows this and also the cellular variables then ", 
     TO cellularBinomialRadical, " should be used.",
     EXAMPLE {
          "R = QQ[x,y]",
	  "I = ideal (y^2, x*y-y, x^2-1)",
	  "binomialRadical I"
          },
     SeeAlso => cellularBinomialRadical}

document {
     Key => {cellularBinomialRadical,
	  (cellularBinomialRadical,Ideal)},
     Headline => "Radical of a cellular binomial ideal",
     Usage => "cellularBinomialRadical I",
     Inputs => {
          "I" => {"a cellular binomial ideal"} },
     Outputs => {
          {"the radical of I"} },
     "The radical of a cellular binomial ideal can be determined very quickly. If the 
     cellular variables are known they can be given as a list via the option ", TO CellVariables, ".",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal(y^3,y^2*z^2-x^3,x*y^2*z,x^3*z-x*y)",
	  "cv = isCellular (I,ReturnCellVars=>true)",
	  "cellularBinomialRadical (I,CellVariables=>cv)"
          },
     SeeAlso => binomialRadical}

document {
     Key => {binomialMinimalPrimes,
	  (binomialMinimalPrimes,Ideal)},
     Headline => "minimal primes of a binomial Ideal",
     Usage => "binomialMinimalPrimes I",
     Inputs => {
          "I" => {"a binomial ideal"}},
     Outputs => {
          {"the list of minimal primes of I"} },
     "The binomial minimal primes of a binomial ideal over QQ exist only in extension fields.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal(y^3,y^2*z^2-x^3,x*y^2*z,x^3*z-x*y)",
	  "binomialMinimalPrimes I",
          },
     "If the option ", TO Verbose, " is set (default), then output about the 
     number of components found so far will be generated.",
     SeeAlso => binomialRadical}

document {
     Key => {binomialAssociatedPrimes},
     Headline => "Associated primes of a binomial ideal",
     Usage => "binomialAssociatedPrimes I",
     Inputs => {
          "I" => {"a binomial ideal"} },
     Outputs => {
          {"the list of associated primes of I"} },
     "First a cellular decomposition is run, then the associated primes of each cellular component are determined.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-y,y^2-x)",
	  "binomialAssociatedPrimes I",
          },
     SeeAlso => {binomialMinimalPrimes,cellularBinomialAssociatedPrimes}}

document {
     Key => {binomialIsPrime,
	  (binomialIsPrime,Ideal)},
     Headline => "test for primeness of a binomial ideal",
     Usage => "binomialIsPrime I",
     Inputs => {
          "I" => {"a binomial ideal"} },
     Outputs => {
          {"true or false, depending on whether I is a binomial prime ideal"} },
     "A binomial ideal is prime only if it is cellular. If the cellular variables ",
     "are known they can be given via the ", TO CellVariables, " option.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-y,y^2-x)",
	  "binomialIsPrime I",
          },
     SeeAlso => {cellularBinomialIsPrimary, CellVariables}}

document {
     Key => binomialIsPrimary,
     Headline => "test for primary binomial ideals",
     Usage => "binomialIsPrime I",
     Inputs => {
          "I" => {"a binomial ideal"}},
     Outputs => {
          {"true or false, depending on whether I is a primary binomial ideal"} },
     "A binomial ideal is prime only if it is cellular. If the cellular variables ",
     "are known, the function ", TO cellularBinomialIsPrimary, " should be used.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal(x-y,z^3)",
	  "binomialIsPrimary I",
          },
     SeeAlso => {cellularBinomialIsPrimary, CellVariables}}

document {
     Key => {cellularBinomialIsPrimary,
	  (cellularBinomialIsPrimary,Ideal)},
     Headline => "test for primaryness of a binomial ideal",
     Usage => "cellularBinomialIsPrimary I",
     Inputs => {
          "I" => { "a binomial ideal"} },
     Outputs => {
          {"true or false, depending on whether I is a binomial primary ideal"} },
     "A binomial ideal is primary only if it is cellular. If the cellular variables ",
     "are known they can be given via the ", TO CellVariables, " option. ", "If the ideal is not primary, ",
     "either 'false' or two distinct associated primes can be returned. The behaviour can be changed using the options ",
     TO ReturnPrimes, " and ", TO ReturnPChars, ".",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-1)",
	  "cellularBinomialIsPrimary (I,ReturnPrimes=>true)",
          },
     SeeAlso => {cellularBinomialIsPrimary, CellVariables, ReturnPrimes, ReturnPChars}}

document {
     Key => {binomialSolve},
     Headline => "solving zero-dimensional binomial Ideals",
     Usage => "binomialSolve I",
     Inputs => {
          "I" => {"a unital binomial ideal"}},
     Outputs => {
          {"the list of points in the zero locus of I in QQ[ww]"} },
     "The solutions of a set of unital binomial equations exist in a cyclotomic field. This function
     will compute the variety of a unital binomial ideal and construct an appropriate cyclotomic 
     field containing the entire variety (as a subset of the algebraic closure of QQ).",
     EXAMPLE {
	  "R = QQ[x,y,z,w]",
	  "I = ideal (x-y,y-z,z*w-1*w,w^2-x)",
	  "dim I",
	  "binomialSolve I",
	  "J = ideal (x^3-1,y-x,z-1,w-1)",
	  "binomialSolve J"
          },
     Caveat => {"The current implementation can only handle unital binomial ideals."},
     SeeAlso => Cyclotomic}

document {
     Key => {isCellular,
	  (isCellular,Ideal)},
     Headline => "testing for cellular binomial ideals",
     Usage => "isCellular I",
     Inputs => {
          "I" => {"a binomial ideal"}},
     Outputs => {
          {"true, or the list of cell variables if I is cellular, false otherwise."} },
     "This function is the standard way to compute the cellular variables.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "I = ideal (x-z^2,y^4)",
	  "isCellular I",
	  "isCellular (I, ReturnCellVars=>true)"
          },
     SeeAlso => {cellularBinomialAssociatedPrimes,binomialCellularDecomposition}}

document {
     Key => {isBinomial,
	     isUnital},
     Headline => "testing for unital binomial ideals",
     Usage => "isBinomial I; isUnital I",
     Inputs => {
          "I" => {"an ideal"}},
     Outputs => {
          {"true if I is binomial, or unital respectively."} },
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "isBinomial ideal(x^2)",
	  "isBinomial ideal(x-y+z,z)",
	  "isBinomial ideal(x^3-x)",
	  "isUnital ideal (x-z,z-y)",
	  "isUnital ideal (x+z)",
	  "isUnital ideal (x^2)"
          },
     SeeAlso => {isCellular}}

-- input related functions
document {
     Key => {makeBinomial},
     Headline => "make a binomial from an exponent vector and a coefficient",
     Usage => "makeBinomial (R,m,c)",
     Inputs => {
          "R" => {"a ring"},
	  "m" => {"a vector of exponents, one for each generator of R"},
	  "c" => {"an element of the coefficient ring of R"}},
     Outputs => {
          {"The binomial defined by the input data, as an element of R."} },
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "makeBinomial (R, [1,-1,-2], 10)"}}

document {
     Key => {latticeBasisIdeal},
     Headline => "construct the ideal whose generators correspond to generators of an integer lattice",
     Usage => "latticeBasisIdeal (R,L)",
     Inputs => {
          "R" => {"a ring"},
	  "L" => {"an integer matrix whose columns span the lattice."}},
     Outputs => {
          {"The unital lattice basis ideal in R, defined by L"} },
     "This function is only a very simple wrapper around ", TO makeBinomial,
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "L = matrix {{1,1},{-3,0},{0,1}}",
	  "latticeBasisIdeal (R, L)"}}

-- cellular stuff:
document {
     Key => {cellularBinomialAssociatedPrimes,
	  (cellularBinomialAssociatedPrimes,Ideal)},
     Headline => "Associated primes of a cellular binomial ideal",
     Usage => "cellularBinomialAssociatedPrimes I",
     Inputs => {
          "I" => {"a cellular binomial ideal"} },
     Outputs => {
          {"the list of associated primes of I"} },
     "If the cell variables are known, they can be given via the option ", 
     TO CellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^2-1,y-x)",
	  "cv = isCellular (I,ReturnCellVars=>true)",
	  "cellularBinomialAssociatedPrimes (I,CellVariables=>cv)"
          },
     SeeAlso => binomialAssociatedPrimes}    

document {
     Key => {cellularBinomialPrimaryDecomposition,
	  (cellularBinomialPrimaryDecomposition,Ideal)},
     Headline => "Primary decomposition of a cellular binomial ideal",
     Usage => "cellularBinomialPrimaryDecomposition I",
     Inputs => {
          "I" => {"a cellular binomial ideal"}},
     Outputs => {
          {"a binomial primary decomposition of I"}},
     "If the cell variables are known, they can be given via the option ", 
     TO CellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^3-1,y-x)",
	  "cv = isCellular (I,ReturnCellVars=>true)",
	  "pd = cellularBinomialPrimaryDecomposition (I,CellVariables=>cv)",
	  "mingens \\ pd"
          },
     Caveat => {"This function will not return minimal generators for performance reasons."},
     SeeAlso => binomialAssociatedPrimes}

document {
     Key => {cellularBinomialUnmixedDecomposition,
	  (cellularBinomialUnmixedDecomposition,Ideal)},
     Headline => "Unmixed decomposition of a cellular binomial ideal",
     Usage => "cellularBinomialUnmixedDecomposition I",
     Inputs => {
          "I" => {"a cellular binomial ideal"}},
     Outputs => {
          {"an unmixed decomposition of I"}},
     "If the cell variables are known, they can be given via the option ", 
     TO CellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x*(y^3-1),x^2)",
	  "cv = isCellular (I,ReturnCellVars=>true)",
	  "ud = cellularBinomialUnmixedDecomposition (I,CellVariables=>cv)"
          },
     SeeAlso => binomialUnmixedDecomposition}

document {
     Key => {partialCharacter,
	  (partialCharacter,Ideal)},
     Headline => "Computing the partial character of a cellular binomial ideal",
     Usage => "partialCharacter I",
     Inputs => {
          "I" => {"a cellular binomial ideal"}},
     Outputs => {
          {"the ", TO PartialCharacter}},
     "If the cell variables are known, they can be given via the option ", 
     TO CellVariables, " otherwise they are computed.",
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^3-1,y-x)",
	  "cv = isCellular (I,ReturnCellVars=>true)",
	  "pc = partialCharacter (I,CellVariables=>cv)",
          },
     Caveat => {"If the input is not cellular the behaviour is undefined. Cellularity is not checked."}}

document {
     Key => {idealFromCharacter,
	   (idealFromCharacter,Ring,PartialCharacter)},
     Headline => "Generate a lattice ideal from a character.",
     Usage => "idealFromCharacter (R, rho)",
     Inputs => {
	  "R" => {"a ring to contain the output ideal"},
	  "rho" => {"a ", TO partialCharacter } },
     Outputs => {
	  {"the lattice ideal corresponding to rho"}
	  },
     Caveat => {"The variables occurring in rho#\"J\" must be variables of R."},
     EXAMPLE {
	  "R = QQ[x,y]",
	  "I = ideal(x^3-1,y-x)",
	  "cv = isCellular (I,ReturnCellVars=>true)",
	  "pc = partialCharacter (I,CellVariables=>cv)",
	  "idealFromCharacter (R,pc) == I"}}

document {
     Key => {randomBinomialIdeal},
     Headline => "Random Binomial Ideals",
     Usage => "randomBinomialIdeal (R,n,d,w,h)",
     Inputs => {
          "I" => {"a ring for the output"},
	  "n" => {"number of generators of the output "},
	  "d" => {"maximum degree of each variable" },
	  "w" => {"number of variables in each generator "},
	  "h" => {"should the generators be 'as homogeneous as possible'"} },
     Outputs => {
           {"a random ideal"} },
     "The exponents are drawn at random from {-d,...,d}. All coefficients are set to 1.",
     EXAMPLE {
	  "R = QQ[a..x]",
	  "randomBinomialIdeal (R,6,2,4,true)",
     	  "randomBinomialIdeal (R,3,4,10,false)"
          },
     "This function is mostly for internal testing purposes. Don't expect anything from it.",
     Caveat => "Minimal generators are produced. These can be less than n and of 
     higher degree. They also need not be homogeneous."}

document {
     Key => {extractInclusionMinimalIdeals,
	  (extractInclusionMinimalIdeals,List)},
     Headline => "Extract inclusion minimal ideals from a list of ideals",
     Usage => "extractInclusionMinimalIdeals L",
     Inputs => {
          "L" => {"a list of ideals"} },
     Outputs => {
          {"the list with some redundant ideals removed"}},
     EXAMPLE {
	  "R = QQ[a,b]",
	  "L = {ideal(a^4),ideal(a^3),ideal(a^5),ideal(b^2*a) }",
	  "extractInclusionMinimalIdeals L",
          },
     "This function is mostly for internal purposes.",
     Caveat => "The resulting list may be not irredundant, because I_1 
     \\subset I_2 \\cap I_3 is not checked."}

document { Key => {CellVariables, [partialCharacter,CellVariables],
     [cellularBinomialRadical,CellVariables], [binomialIsPrime,CellVariables],
     [cellularBinomialIsPrimary,CellVariables], [cellularBinomialAssociatedPrimes,CellVariables],
     [cellularBinomialPrimaryDecomposition,CellVariables],
     [cellularBinomialUnmixedDecomposition,CellVariables]},
     Headline => "cellular variables",
     "The cellular variables of a binomial ideal are the variables which are non-zerodivisors modulo
     that ideal. With this option these variables, if known in advance, can be handed over to
     specialized functions for cellular ideals. ",
     SeeAlso => {cellularBinomialPrimaryDecomposition,cellularBinomialAssociatedPrimes}}

document {
     Key => {ReturnCellVars,
	  [isCellular,ReturnCellVars]},
     Headline => "return the cellular variables",
     "The cellular variables of a binomial ideal are the variables which are non-zerodivisors 
     module that ideal. If this option is set to 'true' then binomialCellularDecomposition will
     return the set of variables for each of its outputs",
     EXAMPLE {
	  "R = QQ[x,y,z]",
          "I = ideal (x*y-z, x*z-y^2)",
          "bcd = binomialCellularDecomposition (I,ReturnCellVars=>true)"}}

document {
     Key => {ReturnPrimes,
     	  [cellularBinomialIsPrimary,ReturnPrimes]},
     Headline => "return two associated primes",
     "If cellularBinomialIsPrimary does not return true it can either return 'false' or two associated primes.
     If this option is set then two associated primes are returned. If ReturnPChars is set too, then partial
     characters will be returned.",
     EXAMPLE {
	  "R = QQ[x,y,z]",
          "I = ideal (x^2-1)",
          "cellularBinomialIsPrimary (I,ReturnPrimes=>true)",
          },
     SeeAlso => {ReturnPChars, cellularBinomialIsPrimary}}

document {
     Key => {ReturnPChars,
	  [cellularBinomialIsPrimary,ReturnPChars]},
     Headline => "return two partial characters",
     "If cellularBinomialIsPrimary does not return true it can either return 'false' or two associated primes.
     If this option is set then two partial characters of distinct associated primes are returned. 
     If ReturnPrimes is set too, then partial characters will be returned.",
     EXAMPLE {
	  "R = QQ[x]",
          "I = ideal (x^2-1)",
          "cellularBinomialIsPrimary (I,ReturnPChars=>true)",
          },
     SeeAlso => {ReturnPrimes, cellularBinomialIsPrimary}}

document {
     Key => {[binomialPrimaryDecomposition, Verbose],
	  [binomialUnmixedDecomposition, Verbose],
	  [binomialCellularDecomposition,Verbose],
	  [binomialMinimalPrimes,Verbose],
	  [cellularBinomialAssociatedPrimes,Verbose],
	  [cellularBinomialPrimaryDecomposition,Verbose],
	  [extractInclusionMinimalIdeals,Verbose],
	  [cellularBinomialUnmixedDecomposition,Verbose]},
     Headline => "generate informative output",
     "If this option is set, functions will generate additional output. Defaults to false"}

document {
     Key => PartialCharacter,
     Headline => "the class of all partial characters",
     "In ", TO Binomials , " the partial character of a cellular binomial ideal is represented 
     as an object of class ", TO PartialCharacter,".  It contains the following three data:",
     UL { {"J -- the cellular variables"},
	  {"L -- a matrix whose columns are generators for the lattice"},
	  {"c -- the list of values the character takes on the generators"}}}

----- TESTS -----
TEST ///
R = QQ[a..f]
I = ideal(b*c-d*e,b*e*f-a*c,a*d*f-d*e,a*b*f-c*d,d^2*e-e,a*d*e-d*e,a*c*e-d*f) 
bpd = BPD I;
assert (intersect bpd == sub(I,ring bpd#0))
///

TEST ///
R = QQ[c,d,x,y,z,w];
I = ideal(x^3*d^2*w-c*z^2,x^5*y^2-w^7,w^3-z^8,z^2-d*w*x^7)
time bpd = binomialPrimaryDecomposition (I,Verbose=>false);
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
bpd = binomialPrimaryDecomposition (I,Verbose=>false);
assert (intersect bpd == I); 
///

TEST ///
-- Cyclotomic stuff
R = QQ[x,y,z]; I = ideal (x^2*y-z^2, x^2-z^3, y^4-1); 
bpd = BPD (I,Verbose=>false);
assert (intersect bpd == sub(I, ring bpd#0));
///

TEST ///
-- Unmixed Decomposition
R = QQ[x,y,z];
I = ideal (x^2, y^2, x*y, x*(z^3-1), y*(z^2-1))
bud = BUD (I, Verbose=>false);
assert(intersect bud == I);
///

TEST ///
-- minimal primes:
-- The 1.0 version of Binomials.m2 would return 6 minimal primes here
-- because redundancy was not taken care of properly
R = QQ[a,b,c,d,x]
I = ideal (a^2 - b^2, c^2 - d^2, x*(a*d-b*c), x*(a*c-b*d))
mp = binomialMinimalPrimes I
assert (intersect mp == I)
assert (#mp == 4)
///

TEST ///
-- remove redundant:
R = QQ[x]
I1 = ideal (x)
I2 = ideal (x^2)
I3 = ideal (x^3)
for L in permutations {I1,I2,I3} do (
    assert (#(extractInclusionMinimalIdeals L) == 1);
    )
///

TEST ///
R = QQ[x,y]
assert(binomialIsPrime ideal x^2 == false)
assert(binomialIsPrime ideal (x^2-y^2) == false)
assert(binomialIsPrime ideal (x-y) == true)
///

end
------------------------------------------------------------
restart
uninstallPackage "Binomials"
installPackage "Binomials"
check "Binomials"

restart
needsPackage "Binomials";
S = QQ[x,y];
b = makeBinomial (S, [2,-3], 5)
isBinomial ideal b
I = ideal(x^2-x*y, x*y-y^2);
isCellular I
binomialIsPrimary I
binomialRadical I
binomialPrimaryDecomposition I

L = binomialPrimaryDecomposition ideal(x^3-1)
L#0

P = binomialPrimaryDecomposition ideal (x^10000 * (y-1), x^10001)
radical P#0
P#1
