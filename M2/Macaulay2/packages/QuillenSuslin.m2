--------------------------------------------------------------------------
-- PURPOSE : QuillenSuslin package for Macaulay2 provides the ability to 
-- compute a free basis for a projective module over a polynomial ring 
-- with coefficients in Q, Z or Z/p for a prime integer p. 
--
-- Copyright (C) 2013 Brett Barwick and Branden Stone
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License version 2
-- as published by the Free Software Foundation.--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--------------------------------------------------------------------------

newPackage(
	"QuillenSuslin",
    	Version => "1.7", 
    	Date => "May 10, 2013",
    	Authors => {
	     {Name => "Brett Barwick", Email => "bbarwick@uscupstate.edu", HomePage => "http://faculty.uscupstate.edu/bbarwick/"},
	     {Name => "Branden Stone", Email => "branden.stone@gtri.gatech.edu", HomePage => "http://bstone.github.io/"}
	     },
    	Headline => "the Quillen-Suslin algorithm for bases of projective modules",
	Keywords => {"Commutative Algebra"},
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "https://msp.org/jsag/",
	     "article title" => "Computing free bases for projective modules",
	     "acceptance date" => "2013-09-18",
	     "published article URI" => "https://msp.org/jsag/2013/5-1/p05.xhtml",
	     "published article DOI" => "10.2140/jsag.2013.5.26",
	     "published code URI" => "https://msp.org/jsag/2013/5-1/jsag-v5-n1-x05-code.zip",
	     "release at publication" => "8a3b2962b97153977eeaf6f92b5f48e246dd8e69",	    -- git commit number in hex
	     "version at publication" => "1.7",
	     "volume number" => "5",
	     "volume URI" => "https://msp.org/jsag/2013/5-1/"
	     },
        PackageExports => {"Complexes"},
        PackageImports => {"MinimalPrimes"},
    	DebuggingMode => false
    	)

export {

-- Options
     "CheckProjective",
     "CheckUnimodular",
     
-- Helper methods
     "isProjective",
     "isUnimodular",
     "maxMinors",
          
-- Main methods for QuillenSuslin algorithm
     "changeVar",
     "computeFreeBasis",
     "completeMatrix",
     "getMaxIdeal",
     "horrocks",
     "patch",
     "qsAlgorithm",
     "qsIsomorphism"
     
}

------------------------------------------------------------
-- Small helper methods ------------------------------------
------------------------------------------------------------

-- Method: coeffVarFF
-- Input: (RingElement,RingElement) -- (rational function, variable)
-- Output: List -- list of {coeff,degree} where f is treated as a polynomial
--     	   in var with coefficients in the rational function field of the
--     	   other variables.
-- Description:
-- As long as the denominator of f does not involve var, this method
-- computes a list of the coefficients of f and the corresponding degree
-- in var when f is treated as a polynomial in var with coefficients in the
-- rational function field defined by the other variables.

coeffVarFF = method()
coeffVarFF(RingElement,RingElement) := (f,var) -> (
     local coeffList; local degList; local denom; local num; local R;
     R = ring var;
     if f == 0 then return {{0_R,0}}; -- If f is identically zero, then return {{0,0}}.
     f = sub(f,frac R);
     num = numerator f;
     coeffList = flatten entries transpose last coefficients(num,Variables => {var});
     degList = flatten (degrees (coefficients(num,Variables => {var}))#1)#0;
     denom = denominator f;
     return apply(#coeffList, i -> {(coeffList#i)/denom,degList#i});
)


-- Method: commonDenom
-- Input: Matrix
-- Output: RingElement
-- Description:
-- Finds the least common denominator of all entries of a matrix
-- over a fraction field by finding the lcm of all denominators.

commonDenom = method()
commonDenom(Matrix) := M -> (
     return lcm flatten apply(numrows M, i -> apply(numcols M, j -> denominator M_(i,j))); -- Make a list of the denominators of each element of the matrix, then find the lcm.
)


-- Method: degVar
-- Input: (RingElement, RingElement)
-- Output: ZZ
-- Description:
-- Returns the degree of a multivariate polynomial
-- with respect to a particular variable.  Also works over certain fraction
-- fields where the denominator does not involve the variable.

degVar = method()
degVar(RingElement,RingElement) := (f,var) -> (
     local R;
     R = ring var;
     if f == 0 then return 0; -- Just return 0 if f is identically zero.
     f = sub(f,frac R); -- This is just to make the method work over fraction fields.
     return (((degrees((coefficients(numerator f,Variables => {var}))#0))#1)#0)#0; -- If f is just a polynomial, then numerator(f) = f.
)


-- Method: factorList
-- Input: RingElement - polynomial to obtain factors of
-- Output: List - list of factors of given polynomial
-- Description:
-- Returns the factors of a polynomial in a polynomial ring
-- as a list.  Does not include multiplicities.
-- If the polynomial has a constant factor and the polynomial
-- ring is over the integers, then this method also factors
-- the constant factor into primes.  If the polynomial ring is
-- over a field, then any constant factors are disregarded.

factorList = method()
factorList(RingElement) := f -> (
     local factors; local constantFactor; local primeFactors;
     local Phi; 
     
     factors = apply(toList factor f, i -> i#0); -- List the factors of f.
     if coefficientRing(ring f) === ZZ then (
	  constantFactor = position(factors, i-> isConstant i); -- Determine the index, if it exists, of a constant factor in factors.
	  if constantFactor =!= null then (
	       primeFactors = factorList(sub(factors#constantFactor,ZZ)); -- Obtain the prime factors of the constant factor.
	       factors = delete(factors#constantFactor,factors); -- Delete the original constant factor from the list (it may not be prime).
	       Phi = map( ring f, ring primeFactors#0); 
   	       scan(reverse primeFactors, i -> factors = prepend(Phi(i), factors)); -- This line just makes sure all of the integer factors are considered as elements of the original ring when the list is returned.
	  );
     );
     if isField coefficientRing(ring f) then scan(factors, i -> if isConstant i then factors = delete(i,factors)); -- If we are working over a field, get rid of any constant factors.
     return factors;
)

factorList(ZZ) := f -> (
     return apply(toList factor f,i -> i#0);
)


-- Method: findAlmostMonicPoly
-- Input: (Ideal,List) - (ideal, list of variables for term order)
-- Output: RingElement - almost monic polynomial in the ideal
-- Description:
-- Given an ideal (of height at least 2) in the polynomial 
-- ring ZZ[varList], this method returns an "almost monic"
-- polynomial in the ideal, where "almost monic" means that
-- it becomes monic in the last variable after an invertible
-- change of variables.

findAlmostMonicPoly = method(Options => {Verbose => 0})
findAlmostMonicPoly(Ideal,List) := opts -> (I,varList) -> (
     local amList; local genList; local I'; local genListS; 
     local minAMDegPos; local ringVars; local ringVarsS; local R;
     local s; local S; local subs; local verbosity; local subR1;
     local subS1; local subsMap;
     
     if #varList == 0 then error "List of variables expected as second argument.";
     R = ring I;
     verbosity = opts.Verbose;
     genList = flatten entries gens I;
     if verbosity >= 2 then print("findAlmostMonicPoly: Finding an 'almost monic polynomial' with respect to "|toString varList|" in "|toString I|".");
     s = scan(genList, i -> if isAlmostMonic(i,varList) then break sub(i,R)); -- If one of the generators is already almost monic, just return it.
     if s =!= null then (
	  if verbosity >= 2 then print "findAlmostMonicPoly: One of the generators was already 'almost monic' with respect to the given variables.";
	  s;
     );
     ringVars = gens ring I;
     subR1 = map(R,ring first ringVars);
     varList = apply(varList, i -> subR1 i);
     S = ZZ(monoid [ringVars,MonomialOrder => Lex]); -- Force the polynomial ring to use the Lex ordering.
     subS1 = map(S,ring first ringVars);
     ringVarsS = apply(ringVars, i -> subS1 i);
     subs = mutableMatrix(S,1,#ringVars);
     -- The next line creates the change of variables that treats the last entry of varList as the "heaviest" variable in the term order (turns it into x), and so on.
     scan(varList, i -> subs_(0,position(ringVars,j -> j == i)) = subS1((reverse ringVars)#(position(varList,j -> j == i))));
     subsMap = map(S,S,matrix subs)*map(S,R);
     I' = sub(ideal apply(genList,i -> subsMap i),S); -- Change variables and substitute into S.
     genListS = flatten entries gens gb I'; -- One can prove that a strong G.B. for I' must contain an almost monic polynomial w.r.t. reverse ringVars.
     amList = delete(,apply(genListS, i -> if isAlmostMonic(i,reverse ringVarsS) then sub(sub(i,matrix{reverse ringVarsS}),R)));
     if amList == {} then error "Check that the ideal has height at least two in the polynomial ring.";
     minAMDegPos = minPosition apply(amList, i -> degVar(i,last varList)); -- Find the position of the almost monic polynomial of smallest degree in last varList.
     return amList#minAMDegPos;
)


-- Method: isAlmostMonic
-- Input: (RingElement,List) - (polynomial, order of variables)
-- Output: Boolean
-- Description:
-- Given a polynomial f in R[varList], this method
-- determines whether it is "almost monic" in the sense
-- that the leading coefficient with respect to the block
-- order is 1.

isAlmostMonic = method()
isAlmostMonic(RingElement,List) := (f,varList) -> (
     local R; local ringVars; local S; local subs; local subR1;
     local subS1;
     R = ring f;
     ringVars = gens ring f;
     subR1 = map(R,ring first varList);
     varList = apply(varList, i -> subR1 i);
     S = (coefficientRing R)(monoid [ringVars,MonomialOrder => Lex]); -- Force the polynomial ring to use the Lex ordering.
     subs = mutableMatrix(S,1,#ringVars);
     subS1 = map(S,ring first varList);
     -- The next line creates the change of variables that treats the last entry of varList as the "heaviest" variable in the term order (turns it into x), and so on.
     scan(varList, i -> subs_(0,position(ringVars,j -> j == i)) = subS1((reverse ringVars)#(position(varList,j -> j == i))));
     return leadCoefficient sub(sub(f,matrix subs),S) == 1; -- Check whether the leading coefficient with respect to this term order is 1.
)


-- Method: isLocalUnit
-- Input: (RingElement,Ideal) -- (rational function,ideal)
-- Output: Boolean
-- Description:
-- Determines whether the given rational function in frac(R) is a unit
-- in the localization R_I.

isLocalUnit = method()
isLocalUnit(RingElement,Ideal) := (f,I) -> (
     local R;
     R = ring I;
     f = sub(f,frac R);
     return ((numerator f) % I != 0 and (denominator f) % I != 0);
)


-- Method: isMonic
-- Input: (RingElement, RingElement) - (polynomial, variable)
-- Output: Boolean
-- Description:
-- Given a polynomial f in R[x_1,...,x_n], this method
-- determines whether f is monic with respect to a
-- particular variable.

isMonic = method()
isMonic(RingElement,RingElement) := (f,var) -> (
     return leadCoeffVar(f,var) == 1;
)


-- Method: isProjective   -- Test 0
-- Input: Module
-- Output: Boolean
-- Description:
-- Output: Returns 'true' if P is projective, 'false'
-- if P is not projective.

isProjective = method()
isProjective(Module) := P -> (
     return maxMinors presentation P == ideal(1_(ring P)); -- If the maximal minors of the presentation matrix generate the unit ideal then the module is projective, see for example Bruns-Herzog Prop. 1.4.9.
)


-- Method: isUnimodular  -- Test 1
-- Input: Matrix
-- Output: Boolean
-- Description:
-- Checks whether a given matrix is unimodular. (ie. its
-- maximal minors generate the unit ideal.)

isUnimodular = method()
isUnimodular(Matrix) := M -> (
     return minors(min(numcols M,numrows M),M) == ideal(1_(ring M));
)


-- Method: laurentCoeffList
-- Input: (RingElement,RingElement) - (polynomial,variable)
-- Output: List - List of lists of the form {deg in var, coefficient}
-- Description:
-- For a Laurent polynomial, this method returns a list of lists
-- {{i,coeff(var^i)}} of degrees of the given variable that appear
-- in f and also the corresponding coefficients.

laurentCoeffList = method()
laurentCoeffList(RingElement,RingElement) := (f,var) -> (
     local coeff; local coeffDenom; local coeffList; local degreeList;
     local denom; local denomDeg; local num; local R;
     
     R = frac((coefficientRing ring f)(monoid [gens ring f]));
     f = sub(f,R);
     var = sub(var,R);
     num = numerator f;
     denom = denominator f;
     denomDeg = degVar(denom,var);
     coeffDenom = denom*(var^(-denomDeg)); -- Get rid of the variable from the denominator.
     coeff = coefficients(num,Variables => {var});
     degreeList = apply(reverse flatten entries (coeff)#0, i -> degVar(i,var) - denomDeg);
     coeffList = apply(reverse flatten entries transpose (coeff)#1, i -> i*(coeffDenom)^-1);
     return apply(#degreeList, i -> {degreeList#i,coeffList#i});
)


-- Method: laurentNormalize
-- Input: (Matrix,RingElement) - (matrix,variable to normalize with respect to)
-- Output: (Matrix,Matrix,Matrix) - (an invertible matrix of Laurent polynomials,
--         change of variables, inverse change of variables.)
-- Description:
-- Given a matrix over a Laurent polynomial ring, this method computes
-- an invertible matrix of Laurent polynomials and an invertible change
-- of variables so that multiplying by the matrix and applying the change
-- of variables makes the original matrix into a matrix with polynomial
-- entries.

laurentNormalize = method()
laurentNormalize(Matrix,RingElement) := (f',var) -> (
     local D; local degSeqList; local denom; local denomDegSeq;
     local dotList; local E; local Etemp; local f2; local f3; local j;
     local invSubList; local invSubs; local invSubs1; local invSubs2;
     local l; local leastDegTerm; local lVector; local minCoeff;
     local minExp; local minSolList; local numDegSeq; local numTerms;
     local R; local S; local subList; local subMatrix; local subs;
     local subs1; local subs2; local usedVars; local varList;
     local phi; local phiD; 
     
     
     R = ring f';
     S = frac((coefficientRing ring f')(monoid [gens ring f']));
   
     phi = map(S,R); 
     f := phi f';
     var = phi var;
   
     varList = gens S;
     usedVars = unique support f_(0,0); -- Need to use 'unique support' since for a rational function, the 'support' command returns the concatenation of the support of the numerator and the support of the denominator.
     if #usedVars == 0 then return (map source f', vars R, vars R);
     if not member(var,usedVars) then error "Error: Expected the given variable to be in the support of the first polynomial.";
     if numcols f < 2 then error "Error: Expected the given row to have at least 2 columns.";
     -- The following code creates a list of lists where each interior list is the degree vector of a term of
     -- the Laurent polynomial f_(0,0).
     numTerms = terms numerator f_(0,0);
     denom = denominator f_(0,0);
     denomDegSeq = apply(#numTerms, i -> apply(varList, j -> degVar(denom,j)));
     numDegSeq = apply(numTerms, i -> apply(varList, j -> degVar(i,j)));
     degSeqList = numDegSeq - denomDegSeq;
     lVector = matrix{apply(#usedVars, i -> 1)};
     dotList = apply(degSeqList, i -> (matrix{i}*transpose lVector)_(0,0)); -- Create a list of the dot product of the current lVector with each degree vector.
     l = 1;
     while dotList != unique dotList do (
	  l = l+1;
	  lVector = matrix{apply(#usedVars, i -> l^i)};
	  dotList = apply(degSeqList, i -> (matrix{i}*transpose lVector)_(0,0)); -- Create a list of the dot product of the current lVector with each degree vector.
     );
     -- Once we exit the while loop, the terms of f_(0,0) will have all distinct powers of var with Laurent monomial coefficients.
     -- Now we construct the appropriate change of variables.
     subList = gens ring f;
     invSubList = gens ring f;
     j = 1;
     scan(subList, i -> if member(i,usedVars) and i != var then (
	  i = i*var^(l^j);
     	  j = j+1;
     ));
     j = 1;
     scan(invSubList, i -> if member(i,usedVars) and i != var then (
	  i = i*var^(-l^j);
     	  j = j+1;
     ));
     subs1 = matrix{subList};
     invSubs1 = matrix{invSubList};
     f2 = sub(f,subs1); -- Now each term of f2_(0,0) has a unique power of var.
     minCoeff = (laurentCoeffList(f2_(0,0),var))#0; -- Get the smallest power of var occurring in f2_(0,0) and also its coefficient.
     D = mutableIdentity(ring f,numcols f);
     -- Need numcols f >= 2 here.
     phiD = map(S, ring minCoeff#1); 
     D_(0,0) = ( (phiD(minCoeff#1)) * var^(minCoeff#0))^-1;  
     D_(1,1) = ( (phiD(minCoeff#1)) * var^(minCoeff#0)); 
     f3 = f2*matrix(D); -- Now f3_(0,0) has constant term 1 in var and only positive powers of var. (ie. it's a polynomial in var whose coefficients are Laurent polynomials in the other variables.)
     -- Since the constant term of f3_(0,0) is 1, we can use elementary column operations to make all other polynomials have strictly positive degree in var.
     E = matrix mutableIdentity(ring f,numcols f);
     scan(1..(numcols f - 1), i -> (
	  leastDegTerm = (laurentCoeffList(f3_(0,i),var))#0;
	  while leastDegTerm#0 < 1 do ( -- sub is needed as ring(leastDegTerm#1) changes during the while loop
	       Etemp = mutableIdentity(ring f,numcols f);
	       Etemp_(0,i) = -sub(leastDegTerm#1,S) * var^(leastDegTerm#0); -- Make an elementary matrix to kill the lowest degree term in f3_(0,i).
	       Etemp = matrix Etemp;
	       f3 = f3*Etemp;
     	       E = E*Etemp;
	       leastDegTerm = (laurentCoeffList(f3_(0,i),var))#0;
	  );
     ));
     -- After this scan, every entry of f3 except the first has strictly positive degree in var.	       
     -- Now we construct the invertible change of variables which makes f3 become a matrix of polynomials.
     usedVars = delete(var,usedVars);
     if #usedVars > 0 then (
     	  minSolList = {};
     	  scan(drop(laurentCoeffList(f3_(0,0),var),1), i -> 
	       minSolList = minSolList|apply(usedVars, j -> -(((laurentCoeffList(i#1,j))#0)#0)/(i#0))
     	  );
          scan(1..(numcols f - 1), i ->
	       scan(laurentCoeffList(f3_(0,i),var), j ->
	       	    minSolList = minSolList|apply(usedVars, k -> -(((laurentCoeffList(j#1,k))#0)#0)/(j#0))
	      )
     	  );
          minExp = max(ceiling max minSolList,0);
     ) else minExp = 0;
     subMatrix = vars ring f;
     subs2 = sub(subMatrix,{var => var*(product usedVars)^minExp});
     invSubs2 = sub(subMatrix,{var => var*(product usedVars)^-minExp});
     subs = sub(subs1,subs2);
     invSubs = sub(invSubs2,invSubs1);
     return (sub(sub(matrix(D)*E,invSubs1),R),sub(subs,R),sub(invSubs,R));
)


-- Method: leadCoeffVar
-- Input: (RingElement,RingElement) - (polynomial, variable)
-- Output: RingElement -- leading coefficient with respect to given variable
-- Description:
-- Returns the leading coefficient of a multivariate
-- polynomial with respect to a particular variable.

leadCoeffVar = method()
leadCoeffVar(RingElement,RingElement) := (f,var) -> (
     if f == 0 then return 0_(ring f); -- If f is identically zero, then return 0.
     return (last coefficients(f,Variables => {var}))_(0,0); 
)


-- Method: leadCoeffVarFF
-- Input: (RingElement,RingElement) -- (polynomial, variable)
-- Output: RingElement -- leading coefficient of f with respect to var
-- Description:
-- This method returns the leading coefficient of f with respect to var
-- when f is viewed as a polynomial in var with coefficients in the
-- rational function field of the other variables. (Assumes that var
-- does not appear in the denominator of f.

leadCoeffVarFF = method()
leadCoeffVarFF(RingElement,RingElement) := (f,var) -> (
     return ((coeffVarFF(f,var))#0)#0;
)


-- Method: maxMinors -- Test 2
-- Input: Matrix
-- Output: Ideal
-- Description:
-- Computes the ideal of maximal minors of the given matrix.

maxMinors = method()
maxMinors(Matrix) := M -> (
     return scan(reverse (0..min(numrows M,numcols M)), i -> if minors(i,M) != 0 then break minors(i,M));
)


-- Method: trimResolution
-- Input: (Module,Complex) -- projective module over a polynomial ring, given as a cokernel.  Also a free resolution of the module.
-- Output: (Matrix,Matrix) -- (Map from R^m -> R^n, Projection map from R^n -> P)
-- Description:
-- Given a projective module P which is presented as a cokernel,
-- this method iteratively trims the length of the resolution
-- until it has the form 0 -> R^m -> R^n -> P -> 0 with m \leq n.  This
-- uses the results described in Proposition 3.1 of Fabianska-Quadrat
-- "Applications of the Quillen-Suslin Theorem" (pg. 37)

trimResolution = method()
trimResolution(Module,Complex) := (mp,F) -> (
     local dd1; local dd2; local dd3; local dd3t; local ident;
     local mp; local p; local proj; local R; local T;
     
     p = length F;
     R = ring mp;
     
     if p == 1 then return (F.dd_1,map(mp,R^(numgens mp),1)); -- If the resolution already has length 1, just return the differential.
     
     dd3 = F.dd_p; -- Get the last map in the resolution.
     dd2 = F.dd_(p-1); -- Get the next-to-last map in the resolution.
     	  
     while p > 2 do (
	  dd1 = F.dd_(p-2);
	  dd3t = transpose dd3;
	  ident = map(target dd3t,target dd3t,1);
	  T = transpose(ident // dd3t); -- T is a left inverse to dd3.
	  dd3 = dd2 || T; -- Make the last map in the shorter resolution.
	  dd2 = dd1 | map(target dd1,R^(numrows T),0); -- Make the next-to-last map in the shorter resolution.
    	  p = p-1;
     );

     -- Now the resolution should have length two, so it has the form
     -- 0 -> R^r -(dd3)-> R^p -(dd2)-> R^q.  We will trim it one last time and
     -- also construct the augmentation map.
     
     dd3t = transpose dd3;
     ident = map(target dd3t,target dd3t,1);
     T = transpose(ident // dd3t);
     dd1 = dd2 || T;
     proj = map(mp,R^(numgens mp),1)|map(mp,R^((numgens target dd1)-(numgens mp)),0);	  
     return(dd1,proj);
);


------------------------------------------------------------
------------------------------------------------------------

------------------------------------------------------------
-- Core methods in the QuillenSuslin package ---------------
------------------------------------------------------------

-- Method: applyRowShortcut
-- Input: Matrix (unimodular row over a polynomial ring)
-- Output: Matrix or null (solves the unimodular row problem if a shortcut is applicable)
-- Description:
-- Implements shortcuts from Fabianska's PhD thesis for solving the
-- unimodular row problem.

applyRowShortcut = method(Options => {Verbose => 0})
applyRowShortcut(Matrix) := opts -> g -> (
     local f; local g1; local gSwap; local h; local l; local lSwap; local M1;
     local M2; local M3; local M4; local n; local p1; local p2;
     local R; local s; local S; local verbosity;
	 local Lconst; local Pconst; local m; 

     
     R = ring g;	
	 m = ideal vars R;
     f = flatten entries g;
     n = #f;
     verbosity = opts.Verbose;
     
     if verbosity >= 1 then print("applyRowShortcut: Checking whether any shortcut methods apply to "|toString g|".");
     
     -- Fabianska shortcut 2.2.1(1).
     -- Test if any element of f is a unit in R.
     if verbosity >= 2 then print "applyRowShortcut: Checking whether any element of the row is a unit.";
     s = scan(n, i -> if ideal(f_i) == ideal(1_R) then break i);
     
     if s =!= null then (
	  if verbosity >= 1 then print "applyRowShortcut: An element of the row was a unit.  Using shortcut 2.2.1(1) from Fabianska's thesis.";
	  -- Swap g1 and gs.
	  gSwap = matrix columnSwap(mutableMatrix g,0,s); -- Now g1 is a unit.
	  S = map(target matrix{{gSwap_(0,0)}}) // matrix{{gSwap_(0,0)}};
	  M1 = mutableIdentity(R,n);
	  M1_(0,0) = S_(0,0);
	  scan(1..(n-1), i -> M1_(0,i) = -S_(0,0)*gSwap_(0,i));
	  return matrix rowSwap(M1,0,s);
     );
     
     -- Fabianska shortcut 2.2.1(2).
     -- Check if any pair of elements already generate the unit ideal.
     if verbosity >= 2 then print "applyRowShortcut: Checking whether any pair of elements in the row generate the unit ideal.";
          
     -- This process only checks pairs of elements if at least one pair has a constant term
     	 Lconst = apply(f, j -> j % m ); -- determines if a constant term exists                                                                                                                                       
	 Pconst = apply(positions(Lconst, i ->  i != 0 ), i -> apply(n, j ->  i ) );  -- creates lists of length #f of positions with constant term                                                                    
	 S = select( pack(2, flatten apply( Pconst, i -> mingle(apply(#Lconst, j -> j ), i) ) ), i -> i_0 != i_1 ); -- creates ordered pairs of positions. To be used in creating desired ideals                               
	 s = scan( S, i -> if ideal(f#(i_0), f#(i_1)) == ideal(1_R) then break i ); -- Checks if two elements generate the unit ideal                                                                                  

     if s =!= null then (
	  if verbosity >= 1 then print "applyRowShortcut: A pair of elements in the row generated the unit ideal.  Using shortcut 2.2.1(2) from Fabianska's thesis.";
	  s = {f#(s_0), f#(s_1)};
	  p1 = position(f, i -> i == s_0);
	  p2 = position(f, i -> i == s_1);
	  M1 = map(target matrix{s}) // matrix{s};
	  M2 = mutableIdentity(R,n);
     -- Swap so that the first two elements of g generate R.
     	  M2 = columnSwap(M2,0,p1);
	  if p2 == 0 then M2 = columnSwap(M2,1,p1) else M2 = columnSwap(M2,1,p2);
	  M3 = mutableIdentity(R,n);
	  gSwap = g*matrix(M2);
	  M3_(0,0) = M1_(0,0);
	  M3_(1,0) = M1_(1,0);
	  M3_(0,1) = -gSwap_(0,1);
	  M3_(1,1) = gSwap_(0,0);
	  M4 = mutableIdentity(R,n);
	  scan(2..(n-1), i -> M4_(0,i) = -gSwap_(0,i));
	  return matrix(M2)*matrix(M3)*matrix(M4);
     );
    
     -- Fabianska shortcut 2.2.1(3).
     -- Check if any element of the row is redundant.  ie. if any
     -- smaller subset of the row already generates the unit ideal.
     if verbosity >= 2 then print "applyRowShortcut: Checking whether any subset of the row generates the unit ideal.";
     s = scan(n, i -> if ideal submatrix'(g,,{i}) == ideal(1_R) then break i);
     if s =!= null then (
	  if verbosity >= 1 then print "A smaller subset of the row was unimodular.  Using shortcut 2.2.1(3) from Fabianska's thesis.";
	  gSwap = matrix columnSwap(mutableMatrix g,0,s);
	  h = map(target gSwap) // submatrix'(gSwap,,{0});
	  if verbosity >= 4 then print("Coefficients of the Bezout relation: "|toString(h));
	  g1 = 1-gSwap_(0,0);
	  h = g1*h;
	  M1 = mutableIdentity(R,n);
	  scan(1..(n-1), i -> scan(1..(n-1), j -> M1_(i,j) = M1_(i,j)-(h_(i-1,0)*gSwap_(0,j))));
	  scan(1..(n-1), i -> M1_(0,i) = -gSwap_(0,i));
	  scan(1..(n-1), i -> M1_(i,0) = h_(i-1,0));
	  M1 = rowSwap(M1,0,s);
	  if verbosity >= 1 then print("Exiting applyRowShortcut.");
	  return matrix(M1);
     );

     -- Fabianska shortcut 2.2.2(1).
     -- Check if any of the coefficients that give 1 as a linear combination
     -- of the elements of the unimodular row are units.
     if verbosity >= 2 then print "applyRowShortcut: Checking whether any of the entries in the right inverse are units.";
     l = flatten entries (map(target g) // g);
     if verbosity >= 2 then print("applyRowShortcut: Right inverse: "|toString l|".");
     s = scan(n, i -> if ideal l_i == ideal(1_R) then break i);
     if s =!= null then (
	  if verbosity >= 1 then print "applyRowShortcut: The right inverse to the row contained a unit.  Using shortcut 2.2.2(1) from Fabianska's thesis.";
	  M1 = mutableIdentity(R,n);
	  M1 = columnSwap(M1,0,s);
	  M2 = mutableIdentity(R,n);
	  scan(n, i -> M2_(i,0) = (matrix{l}*matrix(M1))_(0,i));
	  M3 = mutableIdentity(R,n);
	  apply(1..(n-1), i -> M3_(0,i) = -(g*matrix(M1))_(0,i));
	  return matrix(M1)*matrix(M2)*matrix(M3);
     );
     
     -- Fabianska shortcut 2.2.2(2).
     -- Check if any pair of coefficients expressing 1 as a linear
     -- combination of the elements of the row generate the unit ideal.
     if verbosity >= 2 then print "applyRowShortcut: Checking if any pair of elements of the right inverse generate the unit ideal.";
     S = subsets(l,2);
     s = scan(S, i -> if ideal i == ideal(1_R) then break i);
     if s =!= null then (
	  if verbosity >= 1 then print "applyRowShortcut: Two elements of the right inverse to the row generated the unit ideal.  Using shortcut 2.2.2(2) from Fabianska's thesis.";
	  p1 = position(l, i -> i == s_0);
	  p2 = position(l, i -> i == s_1);
	  M1 = map(target matrix{s}) // matrix{s};
	  M2 = mutableIdentity(R,n);
     -- Swap so that the first two elements of l generate R.
          M2 = columnSwap(M2,0,p1);
	  if p2 == 0 then M2 = columnSwap(M2,1,p1) else M2 = columnSwap(M2,1,p2);
	  M3 = mutableIdentity(R,n);
	  lSwap = matrix{l}*matrix(M2);
	  gSwap = g*matrix(M2);
	  scan(n, i -> M3_(i,0) = lSwap_(0,i));
	  M3_(0,1) = -M1_(1,0);
	  M3_(1,1) = M1_(0,0);
	  M4 = mutableIdentity(R,n);
	  M4_(0,1) = -(-M1_(1,0)*gSwap_(0,0)+M1_(0,0)*gSwap_(0,1));
	  scan(2..(n-1), i -> M4_(0,i) = -(gSwap)_(0,i));
	  return matrix(M2)*matrix(M3)*matrix(M4);  	  
     );

     if verbosity >= 1 then print "applyRowShortcut: No shortcut method applied.";
     return null;
)


-- Method: changeVar
-- Input: (Matrix,List) - (unimodular row, list of variables)
-- Output: (Matrix, Matrix, Matrix) - (unimodular matrix that when
--         multiplied by the row will make the first entry monic
--         after the change of variables, change of variables,
--         inverse change of variables)
-- Description:
-- Given a unimodular row, this method produces a unimodular matrix U
-- and a change of variables so that the first entry in f*U becomes
-- monic in last(varList) after applying the change of variables.
-- The underlying ideas for proof of existence of this matrix and
-- change of variables can be found in a 1976 paper of Suslin-Vaserstein.

changeVar = method(Options => {Verbose => 0})
changeVar(Matrix, List) := opts -> (f,varList) -> (
     local A; local almostMonic; local coefficientMap; local commonFactors;
     local currDeg; local currVar; local degList; local degMatrix;
     local degToBeat; local f1; local factorMap; local factors; local fSwap;
     local g; local h; local invSubs; local leastFactors; local m;
     local M; local M1; local M2; local M3; local minCol; local minDeg;
     local minDegPolyList; local minEntry; local minTerms;
     local mostCommonFactors; local n; local neededFactors;
     local notCommonFactors; local r; local R; local s; local S;
     local subs; local tempCoeff; local transVar; local u; local v; local verbosity;
     local subMap; local invSubMap;
     local phi; local phiL; 
     
     R = ring f;
     currVar = last varList;
     n = numcols f; -- n = number of columns in f.
     m = #varList; -- m = number of variables currently being considered.
     verbosity = opts.Verbose;
     
     if verbosity >= 1 then print("changeVar: Computing a unimodular matrix and change of variables to make the first entry of the new row monic in "|toString(last varList)|".");
     
     -- If n = 2, then we can easily transform f to [1,0].
     
     if n == 2 then (
	  if verbosity >= 2 then print "changeVar: The row had two columns.  Using n = 2 shortcut for normalization.";
	  g = map(R^1) // f;
	  M = mutableIdentity(R,2);
	  M_(0,0) = g_(0,0);
	  M_(1,0) = g_(1,0);
	  M_(0,1) = -f_(0,1);
	  M_(1,1) = f_(0,0);
	  return (matrix M,vars R,vars R);
     );

     -- If a component already equals 1, then move it to the front.
     -- This is just to make the degMatrix in the next step
     -- work out nicely.  i.e. This removes the possibility that
     -- a component of f is monic of degree zero.
     
     s = scan(n, i -> if f_(0,i) == 1 then break i);
     
     if s =!= null then (
	  if verbosity >= 2 then print "changeVar: One of the elements was already 1.  Moving it to the front.";
	  M = mutableIdentity(R,n);
	  M = columnSwap(M,0,s);
	  return (matrix M,vars R,vars R);
     );
     
     -- If none of the components are the constant 1, we create
     -- a matrix (degMatrix) whose (i,j)th entry is zero if
     -- f_(0,j) is not monic in varList#i (currVar counts as i = m-1)
     -- and if degMatrix_(i,j) != 0, then degMatrix_(i,j) is the
     -- degree of f_(0,j) viewed as a polynomial in varList#i.
     -- The goal is to move the smallest degree monic component
     -- to the front of f.
     
     degMatrix = mutableMatrix(R,m,n);
     scan(m, i -> 
	  scan(n, j -> 	       
     	       if leadCoeffVar(f_(0,j),varList#i) == 1 then degMatrix_(i,j) = degVar(f_(0,j),varList#i)
	  )
     );

     -- Now that degMatrix has been constructed, go through
     -- and check if any nonzero entries exist (a nonzero
     -- entry represents a row element which is monic in
     -- one of the variables.)
     
     minEntry = (null,null,null);
     scan(m, i -> 
	  scan(n, j -> 
	       if degMatrix_(i,j) > 0 then ( minEntry = (i,j,degMatrix_(i,j)); break;)
	  )
     );
          
     if minEntry =!= (null,null,null) then (
	  if verbosity >= 2 then print "changeVar: One of the entries of the row was monic in one of the variables. Permuting the variables.";
     	  scan(minEntry#0..(m-1), i -> 
	       scan(n, j -> 
		    if degMatrix_(i,j) > 0 and degMatrix_(i,j) < minEntry#2 then minEntry = (i,j,degMatrix_(i,j))
	       )
	  );
     	  M = mutableIdentity(R,n);
	  M = columnSwap(M,0,minEntry#1);
	  subs = mutableMatrix vars R;
	  subs = columnSwap(subs,minEntry#0,m-1); -- This map just transposes the two variables.  It is its own inverse.
	  return (matrix M,matrix subs,matrix subs);
     );

     if verbosity >= 2 then print "No component of the row was already monic in any of the variables.";
     
     -- The last normalization shortcut is to check whether
     -- a smaller subset of the row elements generate the
     -- entire ring.  If so, then we can use a unimodular
     -- transformation to get 1 in the first position of f.
     -- This is the same as shortcut 2.2.1(3) in applyRowShortcut.
     
     s = scan(n, i -> if ideal submatrix'(f,,{i}) == ideal(1_R) then break i);
     if s =!= null then (
	  if verbosity >= 2 then print "changeVar: A smaller subset of the row was unimodular.  Using a shortcut for normalization.";
	  fSwap = matrix columnSwap(mutableMatrix f,0,s);
	  h = map(target fSwap) // submatrix'(fSwap,,{0});
	  f1 = 1-fSwap_(0,0);
	  h = f1*h;
	  M1 = mutableIdentity(R,n);
	  scan(1..(n-1), i -> scan(1..(n-1), j -> M1_(i,j) = M1_(i,j)-(h_(i-1,0)*fSwap_(0,j))));
	  scan(1..(n-1), i -> M1_(0,i) = -fSwap_(0,i));
	  scan(1..(n-1), i -> M1_(i,0) = h_(i-1,0));
	  M1 = rowSwap(M1,0,s);
	  return (matrix(M1),vars R,vars R);
     );
     
     -- We will split into two cases, based on whether the
     -- coefficient ring is a field or ZZ.
     
     if isField(coefficientRing R) then (
	  if verbosity >= 2 then print "changeVar: Normalizing over a field.";
	  -- Find the component of smallest degree.
	  minCol = minPosition apply(n, i -> degVar(f_(0,i),currVar));
	  if verbosity >= 3 then print("changeVar: Found polynomial of minimal degree in "|toString currVar|": "|toString f_(0,minCol)|".");
	  tempCoeff = leadCoeffVar(f_(0,minCol),currVar);
	  scan(reverse (0..m-1), i -> tempCoeff = leadCoeffVar(tempCoeff,varList#i)); -- After this loop, tempCoeff is the coefficient of the leading term of f_(0,minCol) with respect to the block order induced by varList.
	  M1 = mutableIdentity(R,n);
	  M1 = columnSwap(M1,0,minCol);
	  M1_(minCol,0) = tempCoeff^-1; -- When f is multiplied by M1, f_(0,0) is now almost monic with respect to varList.
	  (subs,invSubs) = monicPolySubs(tempCoeff^-1 * f_(0,minCol),varList,Verbose => verbosity);
	  return (matrix M1,subs,invSubs);
     );

     if coefficientRing(R) === ZZ then (
	  if verbosity >= 2 then print "changeVar: Normalizing over ZZ.";
	  S = subsets(flatten entries f,2);
	  s = scan(S, i -> if gcd(i#0,i#1) == 1_R then break i);
	  -- If s == null here, we need to multiply by a unimodular matrix to make 2 elements be relatively prime.
	  A = mutableIdentity(R,n);
	  if s === null then (
	       
	       -- This uses a constructive prime avoidance technique
	       -- to guarantee that two elements of the row generate
	       -- a height 2 ideal.
	       
	       if verbosity >= 2 then print "changeVar: No pair of elements in the row are relatively prime.  Using constructive prime avoidance algorithm.";
	       factors = apply(n, i-> factorList f_(0,i));
	       -- Finds entry with least number of factors.
	       leastFactors = minPosition apply(factors, i -> #i);
	       -- Finds entry with most number of common factors with position leastFactors.
	       commonFactors = new MutableList from apply(apply(factors, i -> set i), j -> (set factors#leastFactors)*j);
	       commonFactors#leastFactors = set{}; -- Don't compare the one with the least number of factors to itself.
	       mostCommonFactors = maxPosition apply(commonFactors, i -> #i);
	       notCommonFactors = toList(set factors#leastFactors - set factors#mostCommonFactors); -- Create a list of irreducible factors which are not common between the two.
	       coefficientMap = map(R^1) // f;
	       neededFactors = apply(factors, i -> toList(set notCommonFactors - set i));
	       scan(n,i -> A_(i,mostCommonFactors) = coefficientMap_(i,0)*product(neededFactors#i));
	       A_(mostCommonFactors,mostCommonFactors) = 1;
	       A_(leastFactors,mostCommonFactors) = 0;
	       u = leastFactors;
	       v = mostCommonFactors;
	  );
	       
	  if s =!= null then (
	       u = position(flatten entries f,i -> i == s#0); -- u is the position of the first polynomial.
	       v = position(flatten entries f,i -> i == s#1); -- v is the position of the second polynomial.
	  );
     
	  g = mutableMatrix (f*matrix A);
	  M = mutableIdentity(R,n);
	  g = columnSwap(g,1,u); -- Put the first polynomial in column 2.
	  M = columnSwap(M,1,u);
	  if v == 1 then (
	       g = columnSwap(g,u,2); -- Put the second polynomial in column 3.
	       M = columnSwap(M,u,2);
	       ) else (
	       g = columnSwap(g,2,v); -- Put the second polynomial in column 3.
	       M = columnSwap(M,2,v);
	  );
     	  almostMonic = findAlmostMonicPoly(ideal(g_(0,1),g_(0,2)),varList,Verbose => verbosity);
	  if verbosity >= 3 then print("changeVar: Output from findAlmostMonicPoly: "|toString almostMonic|".");
	  (subs,invSubs) = monicPolySubs(almostMonic,varList,Verbose => verbosity);
	  subMap = map(R,R,subs);
	  invSubMap = map(R,R,invSubs);
	  if verbosity >= 3 then print("changeVar: Change of variable and inverse change of variable from monicPolySubs: "|toString(subs,invSubs)|".");
	  degList = drop(apply(n, i -> {i,degVar(subMap g_(0,i),last varList),#(terms subMap g_(0,i))}),{1,2}); -- Make a list of the degrees of the polynomials other than g2 and g3.
	  minDeg = min apply(degList, i -> i#1); -- Find the minimal degree.
	  minDegPolyList = {};
	  scan(degList, i -> if i#1 == minDeg then minDegPolyList = append(minDegPolyList,{i#0,i#2}));
	  -- Now find the position of the polynomial with minimal degree and the least number of terms after the substitution.
	  minTerms = min apply(minDegPolyList, i -> i#1); -- Find the minimal number of terms among these polynomials.
	  r = (minDegPolyList#(position(minDegPolyList, i -> i#1 == minTerms)))#0; -- g_(0,r) is the 'best' polynomial to put at the front.
	  g = columnSwap(g,0,r); -- Move g_(0,r) to the g_(0,0) position.
	  M = columnSwap(M,0,r);
	  degToBeat = degVar(subMap g_(0,0),last varList); -- We need to multiply the monic polynomial in (f2,f3) by a sufficient power of the last variable to beat this power.
	  currDeg = degVar(subMap almostMonic,last varList);
	  factorMap = matrix{{almostMonic}} // matrix{{g_(0,1),g_(0,2)}};
	  phi = map(R, ring (flatten entries subs)#0 ); 
	  phiL = map(R, ring last varList ); 
	  transVar = (vars R)_(0,position(flatten entries subs, i -> phi(i) == phiL(last varList) )); 
	  factorMap = sub(matrix{{(transVar)^(max(0,degToBeat-currDeg+1)),0},{0,(transVar)^(max(0,degToBeat-currDeg+1))}},R)*sub(factorMap,R);
	  M2 = mutableIdentity(R,n);
	  M2_(1,0) = factorMap_(0,0);
	  M2_(2,0) = factorMap_(1,0);
	  return (matrix(A)*matrix(M)*matrix(M2),subs,invSubs); 
     );

     error "Unsupported coefficient ring.  Try QQ, ZZ/p, or ZZ.";
)


-- Method: completeMatrix
-- Input: Matrix - a unimodular m x n matrix over a polynomial ring.
-- Output: Matrix - a completion of the matrix to a square invertible matrix over the polynomial ring.
-- Description:
-- This method just computes the inverse of the matrix returned by qsAlgorithm, which is an extension of
-- the given matrix to a square invertible matrix.

completeMatrix = method(Options => {Verbose => 0})
completeMatrix(Matrix) := opts -> M -> (
     local R;
     R = ring M;
     if R.monoid.Options.Inverses == true then return sub(inverse sub(qsAlgorithm(M,Verbose => opts.Verbose),frac((coefficientRing R)(monoid [gens R]))),R) else return inverse qsAlgorithm(M,Verbose => opts.Verbose);
)


-- Method: computeFreeBasis
-- Input: Module -- projective module over a supported polynomial ring.
-- Output: Matrix -- free generating set for the given module.
-- Description:
-- This method computes a free basis for a projective module over a
-- polynomial ring with coefficients in QQ, ZZ, or ZZ/p.
-- In other words, we should have syz computeFreeBasis P == 0 and
-- image map(P,source computeFreeBasis P,computeFreeBasis P) == P.

computeFreeBasis = method(Options => {Verbose => 0,CheckProjective => false})
computeFreeBasis(Module) := opts -> M -> (
     if opts.Verbose >= 3 then print("computeFreeBasis: Computing a basis for "|toString M);
     return matrix mingens image qsIsomorphism(M, Verbose => opts.Verbose, CheckProjective => opts.CheckProjective); -- Compute an isomorphism from a free module to M.
)

computeFreeBasis(Ideal) := I -> (
     return computeFreeBasis module I;
)


-- Method: getLocalSolutions
-- Input: (Matrix,List,RingElement) -- (matrix, list of variables, current variable)
-- Output: List -- List of local solutions to the unimodular row problem
-- over various localizations at maximal ideals.
-- Description:
-- This computes a set of local solutions for a given unimodular row f
-- as part of the "local loop" of the Logar-Sturmfels algorithm.  The
-- first element of the unimodular row must be monic in the given
-- variable.  This can be achieved by using the changeVar method.

getLocalSolutions = method(Options => {Verbose => 0})
getLocalSolutions(Matrix,List,RingElement) := opts -> (f,ringVars,currVar) -> (
     local I; local localSolutions; local maxIdeal; local R; local S;
     local U; local verbosity; local subR; local subS;
     
     verbosity = opts.Verbose;
     if verbosity >= 1 then print("getLocalSolutions: Computing local solutions for "|toString f|".");
     R = coefficientRing(ring f)(monoid [ringVars]);
     S = ring f;
     I = ideal(0_R);
     subR = map(R,S);
     subS = map(S,R);
     maxIdeal = subS getMaxIdeal(I,ringVars,Verbose => verbosity);
     localSolutions = {};
     U = horrocks(f,currVar,maxIdeal,Verbose => verbosity); -- Find the first local solution.
     I = ideal(subR commonDenom U); -- Let I be the ideal generated by the common denominator of elements of U.
     localSolutions = append(localSolutions,U); -- Add the local solution to localSolutions.
     
     -- Repeat the following "local loop" until the ideal of denominators generates the entire ring.
     -- This is guaranteed to terminate after finitely many steps by the
     -- Hilbert Basis Theorem.
     
     while I != ideal(1_R) do (
	  maxIdeal = subS getMaxIdeal(I,ringVars,Verbose => verbosity);
	  if verbosity >= 2 then print "getLocalSolutions: Denominators did not generate the unit ideal.  Repeating horrocks.";
	  U = horrocks(f,currVar,maxIdeal,Verbose => verbosity);
	  I = I+ideal(subR commonDenom U);
	  localSolutions = append(localSolutions,U);
     );

     return localSolutions;
)


-- Method: getMaxIdeal
-- Input: (Ideal,List) -- (ideal, list of variables)
-- Output: Ideal -- maximal ideal containing the given ideal.
-- Description:
-- Given an ideal I and a list of variables varList,
-- this method computes a maximal ideal in
-- coefficientRing(ring I)[varList] containing I.

getMaxIdeal = method(Options => {Verbose => 0})
getMaxIdeal(Ideal,List) := opts -> (I,varList) -> (
     local c; local genList; local k; local indepVar; local m;
     local maxIdeal; local n; local p; local pp; local primeElement;
     local R; local s; local S; local verbosity;
     
     verbosity = opts.Verbose;
     if verbosity >= 1 then print("getMaxIdeal: Computing a maximal ideal containing "|toString I|".");
     
     R = ring I;
     
     if I == ideal(1_R) then error "Error: Expected a proper ideal.";
     
     -- If the ring is just the integers, then just return any prime factor of the generator.
     if R === ZZ then (
	  return sub(ideal(((factor((gens gb I)_(0,0)))#0)#0),R);
     );
     
     p = char coefficientRing R;
     S = (coefficientRing R)(monoid [varList]);
     
     n = #varList;
     m = sub(I,S);
     
     -- The algorithm over finite fields ZZ/p.

     if (isField(coefficientRing R) and p > 0) then (
     	  if verbosity >= 2 then print "getMaxIdeal: Using algorithm over a finite field.";
	  
	  if m == 0 then return sub(ideal(varList),R);
	  
	  -- Keep adding elements to cut down the dimension of the ideal.
	  
	  while dim m > 0 do (
	       if verbosity >= 3 then print("getMaxIdeal: dim(m) = "|toString(dim m)|".");
	       c = 0;
	       k = 1;
	       indepVar = (support (independentSets(m, Limit => 1))#0)#0;
       	       while m+(indepVar^k+c) == ideal(1_S) do ( 
		    c = c+1;
	       	    if c == p then (k=k+1;c=0;);
	       );
	       m = m+ideal(indepVar^k+c);   
	       if verbosity >= 3 then print("getMaxIdeal: Added the element "|toString(indepVar^k+c)|" to cut down the dimension of m.");
	  );
     
     	  -- Once the ideal is zero-dimensional, the associated primes are the maximal ideals containing it, so return one of them.
     	  
	  if verbosity >= 2 then print "getMaxIdeal: Ideal is now zero-dimensional, computing associated primes.";
	  
	  return sub((minimalPrimes m)_0,R);
	  );
      
     -- The algorithm over QQ.
       
     if coefficientRing(R) === QQ then(
     	  if verbosity >= 2 then print "getMaxIdeal: Using algorithm over QQ.";
	  
	  if m == 0 then return sub(ideal(varList),R);
	  
	  -- Keep adding elements to cut down the dimension of the ideal.
	  
	  while dim m > 0 do (
	       if verbosity >= 3 then print("getMaxIdeal: dim(m) = "|toString(dim m)|".");
	       c = 0;
	       indepVar = (support (independentSets(m, Limit => 1))#0)#0;
	       while m+(indepVar+c) == ideal(1_S) do ( 
		    c = c+1;
	       );
	       m = m+ideal(indepVar+c); 
	       if verbosity >= 3 then print("getMaxIdeal: Added the element "|toString(indepVar+c)|" to cut down the dimension of m.");
	  );
     
     	  -- Once the ideal is zero-dimensional, the associated primes are the maximal ideals containing it, so return one of them.
	  
	  if verbosity >= 2 then print "getMaxIdeal: Ideal is now zero-dimensional, computing associated primes.";
	  
     	  return sub(ideal gens gb (minimalPrimes m)#0,R);
     );

     -- The algorithm over ZZ.

     if coefficientRing(R) === ZZ then (

	  if verbosity >= 2 then print "getMaxIdeal: Using algorithm over ZZ.";

     	  if m == 0 then return (sub(ideal(2),R) + sub(ideal(varList),R));
	  
	  primeElement = 2;
	  
	  -- The following code determines whether or not the ideal already
	  -- contains an integer.  If so, it takes the smallest prime factor.
	  -- If not, it finds an integer to add to the ideal that doesn't
	  -- make it become the entire polynomial ring.
	  
	  genList = flatten entries gens gb m;
	  s = scan(genList,i -> if isConstant i then break i);
	  if s =!= null then (
	       if verbosity >= 3 then print "getMaxIdeal: The ideal already contained a nonzero integer.  Taking a prime factor.";
	       primeElement = ((factor(sub(s,ZZ)))#0)#0;
	  ) else (
	       if verbosity >= 3 then print "getMaxIdeal: The ideal did not contain a nonzero integer.  Finding an integer which when added does not make the ideal become the unit ideal.";
	       while m+(primeElement_S) == ideal(1_S) do (
		    primeElement = primeElement + 1;
	       );
	  );

	  -- Once we have a prime number pp in the ideal, we work in ZZ/pp
	  -- and find a maximal ideal over the finite field containing the
	  -- projection of the given ideal.  Then we lift this ideal back
	  -- to ZZ and add pp back in to create a maximal ideal over ZZ 
	  -- containing the given ideal.
	  
	  pp = sub(primeElement,ZZ);
	  if verbosity >= 3 then print("getMaxIdeal: Now computing a maximal ideal over ZZ/"|toString pp|", which we can pull back to ZZ.");
	  maxIdeal = sub(sub(getMaxIdeal(sub(m,(ZZ/pp)(monoid [varList])),varList,Verbose => verbosity),S) + sub(ideal(pp),S),R);
	  return maxIdeal;
     );
     
     -- Throw an error if the user tries to use an unsupported ring.
     
     error "Unsupported coefficient ring.  Try QQ, ZZ, or a finite field ZZ/p.";
)

getMaxIdeal(Ideal) := opts -> I -> (
     return getMaxIdeal(I,gens ring I,Verbose => opts.Verbose);
)


-- Method: horrocks
-- Input: (Matrix,RingElement,Ideal) -- (unimodular row, variable that
-- 	  the first element of f is monic in, maximal ideal to localize at)
-- Output: Matrix -- unimodular matrix over the localization at the
-- 	   given maximal ideal which solves the unimodular row problem
-- 	   for f.
-- Description: 
-- Effective version of Horrock's Theorem which takes
-- a unimodular row f over R[x1,...,xn-1][currVar] where the
-- first component of f is monic in currVar and computes
-- a local solution to the unimodular row problem when we
-- localize at the given maximal ideal.
-- Note: Could potentially get stuck in an infinite loop if the given
-- ideal is not maximal.

horrocks = method(Options => {Verbose => 0 , CheckUnimodular => false})
horrocks(Matrix,RingElement,Ideal) := opts -> (f,currVar,I) -> (
     local d; local degList; local g; local L; local M; local M1;
     local M2; local minDeg; local minPolyList; local minUnitCoeffDist;
     local n; local N; local r; local R; local s; local tempCoeff;
     local tempPoly; local U; local verbosity;
     
     R = ring f;
     currVar = sub(currVar,R);
     I = sub(I,R);
     n = numcols f; -- n = number of elements in the row f.
     verbosity = opts.Verbose;
     
     if verbosity >= 1 then print("horrocks: Computing a local solution to the unimodular row problem for "|toString f|" when localized at "|toString I|".");
     
     -- Throw errors if f does not meet requirements.
     
     if leadCoeffVar(f_(0,0),currVar) != 1_R then error "Error: The first element of the row is not monic in the given variable.";
     if opts#CheckUnimodular == true then (
     	  if not isUnimodular f then error "Error: The given row is not unimodular.";
     );
     if numrows f != 1 then error "Error: Expected a unimodular row vector.";
     
     U = map(R^n);
          
     -- Take care of a few special cases first.
     
     -- These should already be covered in applyRowShortcut
     -- but should be available here if horrocks can be
     -- used as a standalone method in the package.
     
     -- If deg(f1,currVar) == 0 and f1 is monic in currVar, then f1 = 1.
     
     if degVar(f_(0,0),currVar) == 0 then (
	  if verbosity >= 2 then print "horrocks: First element of the row was already 1.  Using elementary column operations to clear out the rest of the row.";
	  M = mutableIdentity(R,n);
	  scan(1..(n-1), i -> M_(0,i) = -f_(0,i));
	  return matrix M;
     );
     
     -- If n == 2, then (f1,f2) == R.
     
     if n == 2 then (
	  if verbosity >= 2 then print "horrocks: The row had 2 columns.  Using shortcut.";
	  M = map(R^1) // f;
	  return inverse(f || matrix{{-M_(1,0),M_(0,0)}});
     );

     -- Check if any element is already degree zero in currVar and
     -- is a unit in the localization.  If so, then we can finish.
     
     L = scan(1..(n-1), i -> (
          if (degVar(f_(0,i),currVar) == 0 and leadCoeffVar(f_(0,i),currVar) % I != 0) then (
	       if verbosity >= 3 then print("horrocks: f"|i+1|" has degree zero and is a unit in the localization.");
	       M = mutableIdentity(R,n);
	       M = columnSwap(M,0,i);
	       f = f*matrix(M); -- The first element of f is now a unit in the localization.
               U = U*matrix(M); -- Keep track of the swap in the matrix U.
	       break {f,U}; -- Break out of the scan.
	  );
     ));

     if L =!= null then (
	  if verbosity >= 3 then print "horrocks: The first element of the row had degree zero and was a unit in the localization, so we can finish.";
	  f = L#0;
	  U = L#1;
	  M1 = matrix{{1/f_(0,0)}}++map((frac R)^(n-1));
     	  M2 = mutableIdentity(R,n);
     	  scan(1..(n-1), i -> M2_(0,i) = -f_(0,i));
     	  U = U*M1*matrix(M2);
     	  return U; 
     );

     -- Use the general algorithm if n > 2 and deg(f1,currVar) > 0.

     f = sub(f,frac R);
     
     while degVar(f_(0,0),currVar) > 0 do (
	  
	  if verbosity >= 2 then print("horrocks: Iteratively reducing the degree of f1.  Deg(f1) = "|toString(degVar(f_(0,0),currVar))|".");
	  if verbosity >= 3 then print("horrocks: Current row: "|toString f);
	  
	  -- First check if any elements of the row already have
     	  -- degree less than f1 and leading coefficient a unit
     	  -- in the localization.  If so, swap f1 and that element.
     	  -- This scan puts such an element of least degree and so
     	  -- that the leading coefficient has the least number of
     	  -- terms in the f_(0,0) spot.
	       
     	  scan(1..(n-1), i -> (
	       if (degVar(f_(0,i),currVar) < degVar(f_(0,0),currVar) and isLocalUnit(leadCoeffVarFF(f_(0,i),currVar),I)) or (degVar(f_(0,i),currVar) == degVar(f_(0,0),currVar) and isLocalUnit(leadCoeffVarFF(f_(0,i),currVar),I) and #(terms numerator leadCoeffVarFF(f_(0,i),currVar)) < #(terms numerator leadCoeffVarFF(f_(0,0),currVar))) then (
	       	    if verbosity >= 3 then print("horrocks: Another element had smaller degree than f1 or had a unit leading coefficient with less terms: "|toString(f_(0,i))|".");
	       	    M = mutableIdentity(frac R,n);
	       	    M = columnSwap(M,0,i);
	       	    f = f*matrix(M);
	       	    U = U*matrix(M);
	       );
     	  ));

     	  -- Once this polynomial is put in the f_(0,0) spot, go ahead
	  -- and make it monic.
	  
	  M = mutableIdentity(frac R,n);
	  M_(0,0) = 1/leadCoeffVarFF(f_(0,0),currVar);
	  f = f*matrix(M);
	  U = U*matrix(M);
	    
	  -- The following code is implemented just to improve the
	  -- output of the method.  (To try to get polynomials of
	  -- lower degree, less terms, etc.)
	  
	  degList = {};
	  scan(1..(n-1), i -> (
	       tempPoly = f_(0,i);
	       while degVar(tempPoly,currVar) >= degVar(f_(0,0),currVar) do (
		    tempPoly = tempPoly-leadCoeffVarFF(tempPoly,currVar)*f_(0,0)*currVar^(degVar(tempPoly,currVar)-degVar(f_(0,0),currVar));
	       );
	       
	       -- When the while loop ends, tempPoly will have degree
	       -- less than the degree of f_(0,0).
	       
	       tempCoeff = coeffVarFF(tempPoly,currVar);
	       s = scan(#tempCoeff, j -> if isLocalUnit((tempCoeff#j)#0,I) then break j);
	       if s =!= null then (
		    degList = append(degList,{i,degVar(f_(0,i),currVar),degVar(tempPoly,currVar)-(tempCoeff#s)#1});
	       );
	  
	       -- If f_(0,i) was completely reduced to zero by f_(0,0),
	       -- then actually perform this.
	       
	       if tempPoly == 0 then (
		    if verbosity >= 3 then print("horrocks: f"|toString(i+1)|" can be reduced to zero by f1.  Reducing.");
		    while degVar(f_(0,i),currVar) >= degVar(f_(0,0),currVar) do (
			 M = mutableIdentity(frac R,n);
	       	    	 M_(0,i) = -leadCoeffVarFF(f_(0,i),currVar)*currVar^(degVar(f_(0,i),currVar)-degVar(f_(0,0),currVar));
	       	    	 f = f*matrix(M); -- The matrix M will reduce the degree of f_(0,i).
			 U = U*matrix(M); -- Keep track of this in the matrix U.
	       	    );
	       );
	  ));

     	  -- Now degList contains triples {i,deg(f_(0,i)),distance of
	  -- unit coefficient from top degree} for any i so that when
	  -- f_(0,i) is reduced by f_(0,0) it has a unit coefficient.
	  -- Now only consider triples with minimal deg(f_(0,i)).
	  
	  minDeg = min apply(degList, i -> i#1);
	  minPolyList = {};
	  scan(degList, i -> if i#1 == minDeg then minPolyList = append(minPolyList,{i#0,i#2}));
	  minUnitCoeffDist = min apply(minPolyList, i -> i#1);
	  r = (minPolyList#(position(minPolyList, i -> i#1 == minUnitCoeffDist)))#0;
	  
	  if verbosity >= 3 then print("horrocks: Found polynomial of least degree to put in f2 spot: "|toString(f_(0,r))|"."); 
	  
	  -- r gives the index so that f_(0,r) is a polynomial with minimal degree among those who have a unit coefficient after being reduced by f_(0,0) and also has the unit coefficient as close to the highest degree as possible.
	  
	  M = mutableIdentity(frac R,n);
	  M = columnSwap(M,1,r); -- Put f_(0,r) in the f2 spot.
	  f = f*matrix(M);
	  U = U*matrix(M);
	  
	  -- Now use f_(0,0) to reduce the degree of f_(0,1).
	  
	  while degVar(f_(0,1),currVar) >= degVar(f_(0,0),currVar) do (
	       if verbosity >= 3 then print("horrocks: Using f1 to reduce the degree of f2.");
	       M = mutableIdentity(frac R,n);
	       M_(0,1) = -leadCoeffVarFF(f_(0,1),currVar)*currVar^(degVar(f_(0,1),currVar)-degVar(f_(0,0),currVar));
	       f = f*matrix(M); -- The matrix M will reduce the degree of f_(0,1).
	       U = U*matrix(M); -- Keep track of this in the matrix U.
	  );
     	  
	  -- If the leading coefficient of f_(0,1) is a unit in the
	  -- localization, then we can swap f_(0,0) and f_(0,1) and
	  -- start the while loop over again.
	  
     	  if isLocalUnit(leadCoeffVarFF(f_(0,1),currVar),I) then (
	       if verbosity >= 3 then print "horrocks: The leading coefficient of the reduction of f2 was a unit in the localization.  Swapping f1 and f2.";
	       M = mutableIdentity(frac R,n);
	       M = columnSwap(M,0,1);
	       f = f*matrix(M);
	       U = U*matrix(M);
	  ) else (
	       
	  -- If the leading coefficient wasn't a unit in the
	  -- localization, then we need to use Suslin's Lemma to find
	  -- a polynomial g in (f_(0,0),f_(0,1)) with the same degree
	  -- as f_(0,1) whose leading coefficient is a unit in the
	  -- localization.
	       
	       (g,N) = suslinLemma(f_(0,0),f_(0,1),currVar,I);
	       if verbosity >= 3 then print("horrocks: The leading coefficient of the reduction of f2 was not a unit in the localization.  Output from suslinLemma: "|toString g|".");
	     
	       -- The next command just finds the index minDeg so that
	       -- f_(0,minDeg) has minimal degree among f_(0,2),...,f_(0,n-1).
	       
	       minDeg = 2 + minPosition apply(2..(n-1), i -> degVar(f_(0,i),currVar));
	         
	       -- Move f_(0,minDeg) to the f_(0,2) spot.
	       
	       M = mutableIdentity(frac R,n);
	       M = columnSwap(M,2,minDeg);
	       f = f*matrix(M);
	       U = U*matrix(M);
	         
	       while degVar(f_(0,2),currVar) >= degVar(f_(0,0),currVar) do (
		    if verbosity >= 3 then print "horrocks: Using f1 to reduce the degree of f3.";
	       	    M = mutableIdentity(frac R,n);
	       	    M_(0,2) = -leadCoeffVarFF(f_(0,2),currVar)*currVar^(degVar(f_(0,2),currVar)-degVar(f_(0,0),currVar));
	       	    f = f*matrix(M); -- The matrix M will reduce the degree of f_(0,2).
	       	    U = U*matrix(M); -- Keep track of this in the matrix U.
	       );
	  
	       -- At this point we can guarantee that f_(0,2) has degree
	       -- strictly less than f_(0,0) in currVar.
	       
	       if degVar(f_(0,2),currVar) >= degVar(g,currVar) then (
		    
	       -- If the degree of f_(0,2) is at least the degree of
	       -- g, then we will use g to reduce the degree of f_(0,2)
	       -- so that they have the same degree, then we will make
	       -- f_(0,2) have the same leading coefficient as g.
	       
	       	    while degVar(f_(0,2),currVar) > degVar(g,currVar) do (
		    	 if verbosity >= 3 then print "horrocks: Using the polynomial from suslinLemma to reduce the degree of f3.";
	       	    	 M = mutableIdentity(frac R,n);
			 d = degVar(f_(0,2),currVar)-degVar(g,currVar);
	       	    	 M_(0,2) = -leadCoeffVarFF(f_(0,2),currVar)*N_(0,0)*currVar^d;
			 M_(1,2) = -leadCoeffVarFF(f_(0,2),currVar)*N_(1,0)*currVar^d;
			 f = f*matrix(M);
			 U = U*matrix(M);
		    );
	       
	       -- Now that the degree of f_(0,2) and g are the same,
	       -- first check if the sum of their leading coefficients
	       -- is a unit in the localization.  If so, just add them
	       -- together.  If not, then use a unimodular matrix to
	       -- make f_(0,2) monic and then move f_(0,2) to the
	       -- f_(0,0) spot and start over.
	       
	       	    M = mutableIdentity(frac R,n);
		    if isLocalUnit(leadCoeffVarFF(f_(0,2),currVar) + leadCoeffVarFF(g,currVar),I) then (
			 M_(0,2) = N_(0,0);
			 M_(1,2) = N_(1,0);
		    ) else (
	      	         M_(0,2) = (1_R-leadCoeffVarFF(f_(0,2),currVar))*N_(0,0);
		    	 M_(1,2) = (1_R-leadCoeffVarFF(f_(0,2),currVar))*N_(1,0);
		    );
		    
		    M2 = mutableIdentity(frac R,n);
		    M2 = columnSwap(M2,0,2);
		    f = f*matrix(M)*matrix(M2);
		    U = U*matrix(M)*matrix(M2);
		    
	       ) else (
	       	    
	       -- If this code is executed, then f_(0,2) has degree
	       -- strictly less than g in currVar.  In this case we can
	       -- just use a unimodular matrix to add g to f_(0,2),
	       -- then move f_(0,2) to the f_(0,0) spot and start over.
	       
	       	    M = mutableIdentity(frac R,n);
		    M_(0,2) = N_(0,0);
		    M_(1,2) = N_(1,0);
		    M2 = mutableIdentity(frac R,n);
		    M2 = columnSwap(M2,0,2);
		    f = f*matrix(M)*matrix(M2);
		    U = U*matrix(M)*matrix(M2);
	       );
	  );
     );

     -- When the main while loop terminates, f_(0,0) will be
     -- a unit in the localization.  Divide by it to make it 1, then
     -- use it to clear out the rest of the row.
     
     if verbosity >= 3 then print("horrocks: The first element of the row is now a unit in the localization: "|toString f_(0,0)|".");
     
     M = matrix{{1/f_(0,0)}}++map((frac R)^(n-1));
     M2 = mutableIdentity(frac R,n);
     scan(1..(n-1), i -> M2_(0,i) = -f_(0,i));
     U = U*M*matrix(M2);
     
     return U;
)


-- Method: monicPolySubs
-- Input: (RingElement,List) - (polynomial, list of variables)
-- Output: (Matrix,Matrix) - (change of variables, inverse change of variables)
-- Description:
-- Given a polynomial f which is "almost monic" with respect to the
-- order of the variables given by varList, this method determines the
-- change of variables necessary to make the polynomial become monic
-- in the last variable.  The method also computes and returns the
-- inverse change of variables.  Some efforts are made to make the 
-- exponents as small as possible.

monicPolySubs = method(Options => {Verbose => 0})
monicPolySubs(RingElement,List) := opts -> (f,varList) -> (
     
     local allVars; local degZeroSub; local expList; local lastVarPosition;
     local minSolList; local n; local r; local R; local s; local subs;
     local tempCoeff; local tempCoeffDeg; local tempDeg;
     local tempInvSub; local tempPoly; local tempSub;
     local usedVarPosition; local usedVars; local verbosity; local subR1;
     
     R = ring f;
     allVars = gens R;
     usedVars = support f;
     verbosity = opts.Verbose;
     
     if verbosity >= 1 then print("monicPolySubs: Computing an invertible change of variables so that "|toString f|" becomes monic in "|toString(last varList)|".");
     
     -- Throw an error if the polynomial has degree zero and is not 1.
     if #usedVars == 0 then (
	  if f == 1_R then return (vars R,vars R) else error "Error: The given polynomial had degree zero and was not 1."
     );

     subR1 = map(R,ring first varList);
     usedVarPosition = apply(usedVars, i -> position(allVars, j -> j == i));
     lastVarPosition = position(allVars, i -> i == subR1 last varList);
     
     -- First take care of the case when there is only one variable.
     -- If the polynomial is monic, then return the trivial change of
     -- variables.  If it is not monic, then there is no possible
     -- way to make it monic.
     if #usedVars == 1 then (
	  if isMonic(f,usedVars#0) then return (matrix columnSwap(mutableMatrix vars R,usedVarPosition#0,lastVarPosition),matrix columnSwap(mutableMatrix vars R,usedVarPosition#0,lastVarPosition)) else error "Error: The given polynomial is univariate and not monic.  It is not possible to obtain a monic polynomial."
     );

     -- Check here if the polynomial is already monic in a different
     -- variable.  If so, then just permute the variables.
     s = scan(#varList, i -> (
	  if isMonic(f,varList#i) then (
	       if verbosity >= 2 then print "monicPolySubs: The given polynomial was already monic in one of the variables.  Permuting the variables.";
	       subs = mutableMatrix vars R;
	       subs = columnSwap(subs,i,#varList - 1);
	       break (matrix subs, matrix subs); -- The change of variables is just a transposition, so it is its own inverse.
	  );
     ));
     if s =!= null then return s;

     if degVar(f,last varList) == 0 then (
	  print "The element had degree zero in the last variable.";
	  degZeroSub = mutableMatrix vars R;
	  degZeroSub = columnSwap(degZeroSub,last usedVarPosition,lastVarPosition);
	  f = sub(f,matrix degZeroSub); -- Interchange variables so that last varList is involved in f.  Now f has positive degree in last varList.
     );
     
     -- Now we enter the general algorithm.
     expList = {};
     scan(#usedVars-1, i -> (
          -- Here we apply an inductive process to create a change of variables
          -- that makes f become monic in last(varList).
          tempPoly = f;
	  -- Notice that if #varList <= 2 then the following scan is over an empty list, which is what we want.
    	  scan(1..(#usedVars-i-2), j -> tempPoly = leadCoeffVar(tempPoly,usedVars#-j));
	  r = degVar(leadCoeffVar(tempPoly,usedVars#(i+1)),first usedVars);
	  n = degVar(tempPoly,usedVars#(i+1));
	  tempDeg = flatten degrees(target (coefficients(tempPoly,Variables=>{usedVars#(i+1)}))#1);
	  tempDeg = drop(tempDeg,1);
	  tempCoeff = flatten entries (coefficients(tempPoly,Variables=>{usedVars#(i+1)}))#1;
	  tempCoeff = drop(tempCoeff,1);
	  tempCoeffDeg = apply(tempCoeff, j -> degVar(j,first usedVars));
	  minSolList = apply(#tempDeg, j -> if (tempCoeffDeg#j-r) % (n-tempDeg#j) == 0 then max(sub((tempCoeffDeg#j-r)/(n-tempDeg#j),ZZ)+1,1) else max(ceiling((tempCoeffDeg#j-r)/(n-tempDeg#j)),1));
	  expList = append(expList,max(append(minSolList,1)));
     	  tempSub = mutableMatrix vars R;
	  tempSub_(0,usedVarPosition#(i+1)) = usedVars#(i+1) + sub((first usedVars)^(expList#i),R);
	  f = sub(f,matrix tempSub);
     ));
     tempSub = mutableMatrix vars R;
     tempInvSub = mutableMatrix vars R;
     tempSub_(0,usedVarPosition#0) = subR1 last varList;
     tempSub_(0,lastVarPosition) = sub(first usedVars,R) + subR1 (last varList)^(last expList);
     tempInvSub_(0,usedVarPosition#0) = subR1 last varList - sub((first usedVars)^(last expList),R);
     tempInvSub_(0,lastVarPosition) = sub(first usedVars,R);
     scan(1..(#usedVars-2), i -> (
	  tempSub_(0,usedVarPosition#i) = sub(usedVars#i,R) + subR1 (last varList)^(expList#(i-1));
     	  tempInvSub_(0,usedVarPosition#i) = sub(usedVars#i,R) - sub((first usedVars)^(expList#(i-1)),R);
     ));

     if degZeroSub =!= null then (
	  print("degZeroSub: "|toString(degZeroSub));
	  tempSub = columnSwap(tempSub,last usedVarPosition,lastVarPosition);
	  tempInvSub = sub(matrix tempInvSub,matrix degZeroSub);
     );
     
     return (matrix tempSub,matrix tempInvSub);
)


-- Method: parkAlgorithm
-- Input: Matrix
-- Output: Matrix - Solves the unimodular matrix problem for the given
--         matrix if it is unimodular, returns an error otherwise.
-- Description:
-- This algorithm iteratively uses Park's causal conversion algorithm
-- and the methods of QuillenSuslin for polynomial rings to solve the
-- unimodular matrix problem for a unimodular matrix of Laurent polynomials.

parkAlgorithm = method()
parkAlgorithm(Matrix) := f -> (
     local cols; local E; local NList; local invSubList; local R;
     local rows; local S; local T; local temp; local tempInvSub;
     local tempN; local tempSub; local tempU; local U; local UList;
     local V; local varList;
     
     R = ring f;
     T = (coefficientRing R)(monoid [gens R]);
     S = frac T;
     varList = gens S;
     f = sub(f,S);
     rows = numrows f;
     cols = numcols f;
     -- First handle the case where f has only 1 column (and hence only 1 row since we are assuming rows <= cols).
     -- The matrix is unimodular over the Laurent polynomial ring if and only if the entry is a unit (Laurent monomial).
     if cols == 1 then (
	  if #(terms numerator f) == 1 then return matrix{{(f_(0,0))^-1}} else error "Error: The given matrix is not unimodular.";
     );
     NList = {};
     UList = {};
     invSubList = {};
     scan(rows, i -> (
	  temp = submatrix(f,{i},toList(i..(cols-1)));
          (tempN,tempSub,tempInvSub) = laurentNormalize(temp,first varList);
     	  
     -- TODO: Possible optimization.  May want to choose the 'best' variable
     -- to normalize with respect to at each inductive step of the algorithm.
     
          NList = NList|{map(R^i,R^i,1_R)++sub(tempN,R)};
     	  invSubList = invSubList|{sub(tempInvSub,S)};
     	  temp = sub(sub(temp*tempN,tempSub),T); -- Now temp is a polynomial row vector which is unimodular if and only if the first row of f is.
     	  if not isUnimodular temp then error "The given matrix was not unimodular over the Laurent polynomial ring.";
     	  tempU = map(R^i,R^i,1_R)++sub(qsAlgorithm temp,R);
     	  UList = UList|{tempU};
     	  f = f*sub(NList#i,S)*sub(UList#i,tempInvSub); -- Now the i-th row of f has 1 on the diagonal and 0's to the right of the diagonal.
     ));
     U = map(R^cols,R^cols,1_R);
     scan(#NList, i -> U = U*(sub(NList#i,R))*sub(sub(UList#i,invSubList#i),R)); 
     if rows == 1 then return U; -- If f only has 1 row then f*U = [1 0 ... 0].
     V = prune image f;
     liftedF := (gens V) // f; -- over S
     --E = (sub(gens V,R) // map(R^rows,R^cols,sub(f,R)))|(map(R^rows,R^(cols-rows),0_R)||map(R^(cols-rows)));
     E = sub(liftedF, R)|(map(R^rows,R^(cols-rows),0_R)||map(R^(cols-rows)));
     return U*E;
)


-- Method: patch
-- Input: (List,RingElement) -- (list of local solutions, current variable)
-- Output: Matrix -- unimodular matrix U so that f*U is equivalent to
--         evaluating f at currVar = 0.
-- Description:
-- Method to patch together the local solutions obtained
-- by getLocalSolutions as in Logar-Sturmfels.

patch = method(Options => {Verbose => 0})
patch(List,RingElement) := opts -> (matrixList,currVar) -> (
     local deltaDenom; local deltaList; local g; local inverseList;
     local k; local localVar; local m; local R; local S; local U;
     local verbosity; local flattenData; local phi; local phiInv;
     local currVarS; local localVarS; local subS; local subR;
     local matrixListMap; local subS2;
          
     R = ring numerator (first matrixList)_(0,0); -- Getting our hands on the underlying ring.
     flattenData = flattenRing(R[symbol localVar]); -- Adjoin the dummy variable 'localVar' and flatten to a polynomial ring.
     S = frac first flattenData; -- Make the fraction field of the polynomial ring including our dummy variable.
     phi = flattenData#1; -- Name the natural ring homomorphism R[localVar] ---> R[x,y,localVar].
     phiInv = phi^-1; -- Construct the natural ring homomorphism R[x,y,localVar] ---> R[localVar].
     
     currVar = sub(currVar,frac R);
     m = numcols matrixList#0; -- m = length of unimodular row.
     k = #matrixList; -- k = number of local solutions.
     verbosity = opts.Verbose;
     
     if verbosity >= 1 then print("patch: Patching together local solutions: "|toString matrixList);

     matrixListMap = map(frac R, ring matrixList#0);
     matrixList = apply(matrixList, i -> matrixListMap(i));
     inverseList = apply(matrixList, i -> matrixListMap(inverse i)); -- Compute inverse for each local solution in frac R.

     currVarS = sub(currVar,S);
     localVarS = sub(localVar,S);
     subS = map(S,frac R);
     subS2 = map(S,S,{currVarS => currVarS+localVarS});
     deltaList = apply(k, i -> (subS matrixList#i)*subS2(subS inverseList#i)); -- Make a list of all of the 'shifted' product matrices.
     subR = map(R,S);
     deltaDenom = apply(deltaList, i -> subR commonDenom i); -- Make a list of the common denominators of these matrices and substitute them back into R.
     
     g = map(R^1) // matrix{deltaDenom};
     U = matrixList#0*sub(sub(inverseList#0,{currVar => (currVar - currVar*(g_(0,0))*(deltaDenom#0))}),frac R);
     scan(1..(k-1), i -> U = U*sub(matrixList#i,{currVar => (currVar - (sum(0..(i-1), j -> currVar*g_(j,0)*deltaDenom#j)))})*sub(inverseList#i,{currVar => (currVar - (sum(0..i, j -> currVar*g_(j,0)*deltaDenom#j)))}));
     return sub(U,R);  -- U is a unimodular matrix over R such that f*U does not involve currVar (it is the same as f evaluated when currVar = 0).
)


-- Method: qsAlgorithm
-- Input: Matrix
-- Output: Matrix - Solves the unimodular matrix problem for M.
-- Description:
-- For a given unimodular matrix M of size m x n with m \leq n, 
-- qsAlgorithm computes a unimodular matrix U such that M*U is
-- of the form [I | 0].
-- If m > n, then qsAlgorithm computes a unimodular matrix U such
-- that U*M is of the form [ I ]
--                         [ 0 ].

qsAlgorithm = method(Options => {Verbose => 0 , CheckUnimodular => false})
qsAlgorithm(Matrix) := opts -> M -> (
     local B; local bottomRow; local inv; local ncol; local nrow;
     local r; local R; local U; local U'; local U''; local V;
     local verbosity; local W; local transposed;
     
     if opts#CheckUnimodular == true then (
	  if not isUnimodular M then error "Error: Expected a unimodular matrix.";
     );
 
     R = ring M;
     verbosity = opts.Verbose;
     
     transposed = false; -- Initialize this to false unless the following if statement is satisfied.
     if numcols M < numrows M then (
	  M = transpose M; -- If there are more rows than columns, then work with the transpose.
          transposed = true; -- Also set this variable so that we will know to transpose U at the end.
          if verbosity >= 3 then print "qsAlgorithm: Transposed the original matrix so that there are more columns than rows.";
     );
     ncol = numcols M;
     nrow = numrows M;
     r = ncol-nrow;
     
     if verbosity >= 2 then print("qsAlgorithm: Solving the unimodular row problem for: "|toString(M)|".");
          
     -- If the ring is a Laurent polynomial ring, then use Park's
     -- algorithm to solve the unimodular matrix problem.
     
     if numcols vars R > 0 and R.monoid.Options.Inverses == true then (
	  if transposed == true then return transpose parkAlgorithm M else return parkAlgorithm M;
     );
          
     -- If the ring is a PID, then use the Smith normal form algorithm.
     
     if isField R or R === ZZ or (isField coefficientRing R and numcols vars R == 1) then (
	  if transposed == true then return transpose qsAlgorithmPID(M,Verbose => verbosity) else return qsAlgorithmPID(M,Verbose => verbosity);
     );

     -- If there is only one row, then just return the
     -- output of qsAlgorithmRow.
     
     if nrow == 1 then (
	  if transposed == true then return transpose qsAlgorithmRow(M,Verbose => verbosity) else return qsAlgorithmRow(M,Verbose => verbosity);
     );

     -- Implements the shortcut for (p-1) x p unimodular
     -- matrices from Fabianska section 2.2.1.
     
     -- Invert and calculate row minors. (Use Cauchy-Binet formula.)
     if r == 1 then (
	  if verbosity >= 2 then print "qsAlgorithm: Matrix had exactly one more column than the number of rows.  Using shortcut 2.2.1 (2) from Fabianska's thesis.";
	  inv = inverse M;
	  bottomRow = mutableMatrix(R,1,ncol);
	  scan(ncol, i -> bottomRow_(0,i) = (-1)^(ncol+1+i)*det(submatrix'(inv,{i},)));
	  bottomRow = matrix bottomRow;
	  if transposed == true then return transpose inverse(M || bottomRow) else return inverse(M || bottomRow);
     );
     
     -- Since the given matrix is unimodular and has more columns than rows
     -- (and hence is right invertible), the first row of the matrix is also
     -- unimodular.  This allows us to implement an inductive approach
     -- to reduce the general unimodular matrix problem to the case of
     -- unimodular rows.  The idea for this section can be found in the
     -- Logar-Sturmfels paper.
     
     U = qsAlgorithmRow(M^{nrow-1},Verbose => verbosity);
     M = M*U;
     if verbosity >= 2 then print "qsAlgorithm: Using qsAlgorithmRow iteratively to clear out elements to the right of the main diagonal.";
     if verbosity >= 3 then print("qsAlgorithm: Need to clear out elements to the right of the main diagonal on "|toString(nrow)|" rows.");
     scan(1..(nrow-1), i -> (
	  B = submatrix(M,0..(nrow-1-i),i..(ncol-1));
	  if verbosity >= 3 then print("qsAlgorithm: Applying qsAlgorithmRow to truncated row "|toString(i+1)|" to clear out the elements to the right of the main diagonal.");
	  U' = qsAlgorithmRow(B^{numrows(B)-1},Verbose => verbosity);
	  if verbosity >= 4 then print U';
	  U'' = map(R^i)++U';
	  if verbosity >= 4 then print U'';
	  U = U*U'';
	  M = M*U'';
	  if verbosity >= 4 then print M;
     ));
     
     -- Now only the elements under the main diagonal in M are nonzero.
     -- Use elementary column operations to clear out these nonzero entries.
     
     V = prune image M;
     W = (gens V // map(R^nrow,R^ncol,M))|(map(R^nrow,R^r,0_R)||map(R^r));
     if verbosity >= 2 then print "qsAlgorithm: Using elementary column operations to clear out the elements below the main diagonal.";
     U = sub(U*W,R);
     if transposed == true then return transpose U else return U;
)


-- Method: qsAlgorithmPID
-- Input: Matrix
-- Output: Matrix (solves the unimodular matrix problem for M)
-- Description:
-- This method takes a unimodular matrix M over a PID and returns a unimodular matrix U
-- such that M*U = (I | 0).  This algorithm is described in Fabianska
-- in the proof of Lemma 2.1.5.
-- Warning: Will return unexpected results if R is not actually a PID!

qsAlgorithmPID = method(Options => {Verbose => 0})
qsAlgorithmPID(Matrix) := Matrix => opts -> M -> (
     local B; local C; local D; local F; local G; local H1;
     local H2; local ncols; local nrows; local U; local U'; local V;
     
     if opts.Verbose >= 1 then print "qsAlgorithmPID: Using Smith normal form algorithm to compute a solution to the unimodular matrix problem over a PID.";
     
     nrows = numrows M;
     ncols = numcols M;
     (D,F,G) = smithNormalForm M;
     H1 = submatrix(G,0..(ncols-1),0..(nrows-1));
     H2 = submatrix(G,0..(ncols-1),nrows..(ncols-1));
     U' = H1*F | H2;
     V = submatrix(M*U',0..nrows-1,0..nrows-1);
     B = prune image V;
     C = gens B // V;
     return U'*(C ++ map((ring M)^(ncols-nrows))); 
)


-- Method: qsAlgorithmRow
-- Input: Matrix
-- Output: Matrix -- Solution to unimodular row problem for M.
-- Description:
-- General algorithm to compute solution to the unimodular
-- row problem using the inductive procedure from Logar-Sturmfels.

qsAlgorithmRow = method(Options => {Verbose => 0 , CheckUnimodular => false})
qsAlgorithmRow(Matrix) := opts -> f -> (
     
     if numrows f != 1 then error "Error: Expected a row vector.";
     if opts#CheckUnimodular == true then (
     	  if not isUnimodular f then error "Error: Expected a unimodular matrix.";
     );       
     local currVar; local invSubs; local invSubsList; local localSolutions;
     local m; local M; local matrixList; local n; local R; local s;
     local s2; local shortcut; local subs; local U; local U1;
     local varList; local verbosity; local W;
     
     verbosity = opts.Verbose;
     
     if verbosity >= 1 then print("qsAlgorithmRow: Computing a solution to the unimodular row problem for: "|toString(f));
     
     -- If a shortcut applies, return it.
     
     s = applyRowShortcut(f,Verbose => verbosity);
     if s =!= null then return s;
     
     -- If the ring is a PID, then use the Smith normal form algorithm.
     
     R = ring f;
     n = numcols vars R;
     if isField R or R === ZZ or (isField coefficientRing R and n == 1) then return qsAlgorithmPID(M,Verbose => verbosity); 
     
     -- If not, enter the general algorithm.
     
     if verbosity >= 2 then print "qsAlgorithmRow: No shortcut method applied.  Entering the general algorithm.";
     
     m = numcols f; -- m = length of the row.
     U = map(R^m);
     varList = gens R;
     currVar = last varList; -- Set the last variable to be the current variable to eliminate.
     varList = drop(varList,-1); -- Delete the last variable from the list of variables.
     matrixList = {}; -- Initialize matrixList, which will hold each of the matrices applied at each step of the algorithm.
     invSubsList = {}; -- Initialize invSubsList, which will hold all of the inverse substitutions produced each time changeVar is used.
     
     if verbosity >= 2 then print("qsAlgorithmRow: Current variable to eliminate: "|toString currVar);
     
     -- Iteratively reduce the number of variables in f.
	  
     shortcut = scan(n, i -> (
	  
	  s = scan(m, j -> if degVar(f_(0,j),currVar) > 0 then break j);
	  
	  -- If f doesn't involve currVar, then move to the next variable.
	  
	  if s =!= null then (
	       (W,subs,invSubs) = changeVar(f,append(varList,currVar),Verbose => verbosity); -- Normalize the row so that the first component is monic with respect to currVar.
	       if verbosity >= 3 then print("qsAlgorithmRow: Output of changeVar: "|toString(W,subs,invSubs));
	       f = sub(f*W,subs);
	       invSubsList = append(invSubsList,invSubs);
	       if verbosity >= 3 then print("qsAlgorithmRow: The first element of the row is now monic in "|toString(currVar)|": "|toString(f));
	       -- Enter the local loop.
	       if verbosity >= 2 then print "qsAlgorithmRow: Now entering the local loop.";
	       localSolutions = getLocalSolutions(f,varList,currVar,Verbose => verbosity); -- Collect a list of unimodular matrices over frac(R) which solve the unimodular row problem for f over various localizations at maximal ideals.
	       if verbosity >= 3 then print("qsAlgorithmRow: Local solutions computed: "|toString localSolutions);
	       if verbosity >= 2 then print "qsAlgorithmRow: Patching local solutions.";
	       U1 = patch(localSolutions,currVar,Verbose => verbosity); -- U1 is a unimodular matrix such that f*U is equivalent to evaluating f at currVar = 0.
	       if verbosity >= 3 then print ("qsAlgorithmRow: Global solution from patching method: "|toString U1);
	       matrixList = append(matrixList,W*U1); -- Add W*U1 to the list of matrices used.
	       f = f*U1; -- Update f.
	       if verbosity >= 3 then print("qsAlgorithmRow: Row now has one less variable: "|toString(f));
	       
	       -- See if any shortcut methods apply to this new row.
	       
	       if verbosity >= 2 then print "qsAlgorithmRow: Checking if any shortcut methods apply to the new row.";
	       
	       s2 = applyRowShortcut(f,Verbose => verbosity);
	       if s2 =!= null then (
	       	    matrixList = append(matrixList,s2);
     	       	    invSubsList = append(invSubsList,vars R);
	       	    scan(#matrixList,i -> U = sub(matrixList#(-i-1)*U,invSubsList#(-i-1)));
     	       	    break U;
	       );
	  );
	  currVar = last varList; -- Set currVar to the next variable.
	  varList = drop(varList,-1); -- Shorten the list of variables by one.
	  
	  if verbosity >= 2 then print("qsAlgorithmRow: Current variable to eliminate: "|toString currVar);
	  
	  -- Now repeat the loop until no variables are left.
     ));

     -- If a shortcut method applied sometime during the inductive
     -- process, then it will break the above scan and the matrix
     -- solving the unimodular row problem will be stored in shortcut.
     
     if shortcut =!= null then (
	  if verbosity >= 2 then print "qsAlgorithmRow: A shortcut method applied, returning the result.";
	  return shortcut;
     );	  
     -- When the above scan has finished, we will have eliminated all
     -- variables from f, and so it will be a unimodular row over the
     -- coefficient ring, which is a PID, so we can use qsAlgorithmPID.
     
     matrixList = append(matrixList,qsAlgorithmPID(f,Verbose => verbosity));
     invSubsList = append(invSubsList,vars R);
     
     -- Now apply all of the inverse substitutions and multiply in the correct order
     -- to undo the changes of variables created by changeVar.
     scan(#matrixList,i -> U = sub(matrixList#(-i-1)*U,invSubsList#(-i-1)));
     return U;
)


-- Method: qsIsomorphism
-- Input: Module -- projective module over a polynomial ring.
-- Output: Matrix -- an isomorphism from a free module to the given projective module.
-- Description:
-- Produces an isomorphism from a free module of the appropriate rank
-- over a polynomial ring to the given projective module.

qsIsomorphism = method(Options => {Verbose => 0,CheckProjective => false})
qsIsomorphism(Module) := opts -> M -> (
     local mp; local pruneMap; local d1; local proj; local C; local F;
     local verbosity;
     verbosity = opts.Verbose;
     
     if verbosity >= 2 then print("qsIsomorphism: Computing an isomorphism between a free module and "|toString(M)|".");
     
     if opts#CheckProjective == true then (
	  if verbosity >= 2 then print "qsIsomorphism: Checking whether the given module is projective.";
     	  if not isProjective M then error "Error: The given module is not projective.";
     );
     
     if verbosity >= 3 then print("qsIsomorphism: Computing a minimal presentation for "|toString(M)|".");
     mp = minimalPresentation M;
     pruneMap = mp.cache.pruningMap;
     if verbosity >= 3 then print("qsIsomorphism: Constructing a free resolution of the minimal presentation.");
     
     F = freeResolution mp;
     -- If Macaulay2 already knows that the module is free, then just return the pruning map.
     if length F == 0 then (
	  if verbosity >= 2 then print "qsIsomorphism: Macaulay2 already knows that this module is free.  Returning the pruning map.";
	  return pruneMap;
     );

     if verbosity >= 2 then print "qsIsomorphism: Trimming the resolution of the projective module.";
     (d1,proj) = trimResolution(mp,F); -- Trim the resolution to length 1 and also obtain the augmentation map.
     if verbosity >= 3 then print("qsIsomorphism: Map between free modules: "|toString d1);
     if verbosity >= 3 then print("qsIsomorphism: Augmentation map: "|toString proj);
     -- d1 must have numcols d1 <= numrows d1.
     C = submatrix'(completeMatrix(d1,Verbose => verbosity),,toList(0..((numcols d1)-1))); -- C consists of the columns which were added to d1 to complete it to a square invertible matrix.
     -- The images of these columns will form a free generating set for the projective module M.
     return pruneMap*proj*C;
)


-- Method: suslinLemma
-- Input: (RingElement,RingElement,RingElement,Ideal)
-- (monic polynomial, polynomial with a unit coefficient, variable, ideal)
-- Output: (RingElement,Matrix) -- (monic polynomial in (f,g), matrix M so
--     	   that matrix{{f,g}}*M = matrix{{monic polynomial}}.
-- Description:
-- Effective version of Suslin's Lemma which takes two
-- polynomials with deg(f) = s and deg(g) <= s-1 in B[var], f monic,
-- and one of the coefficients of g a unit, and produces
-- a monic polynomial h in (f,g) with deg(h) = s-1.
-- Note: See Rotman Theorem 4.55.

suslinLemma = method()
suslinLemma(RingElement,RingElement,RingElement,Ideal) := (f,g,var,I) -> (
     local F; local lcg; local M; local R; local tempM;
     
     if degVar(f,var) <= degVar(g,var) then error "Error: Expected the first entry to have degree strictly larger than the second entry.";
     if not isLocalUnit(leadCoeffVarFF(f,var),I) then error "Error: Expected the leading coefficient of the first entry to be a unit when localized at the given ideal.";
     
     R = ring I;
     F = matrix{{sub(f,frac R),sub(g,frac R)}};
     lcg = leadCoeffVarFF(F_(0,1),var);
     -- Make F_(0,0) monic if it wasn't already.
     M = matrix{{1/leadCoeffVarFF(F_(0,0),var),0},{0,1}};
     F = F*M;
     
     -- Keep creating syzygies on the leading terms of f and g.
     -- Eventually, since g contains a unit coefficient, this process
     -- will terminate with the leading coefficient being a unit in the
     -- localization.
     while not isLocalUnit(lcg,I) do (
	  tempM = matrix{{1,-lcg},{0,var^(degVar(F_(0,0),var)-degVar(F_(0,1),var))}};
	  F = F*tempM;
	  M = M*tempM;
	  lcg = leadCoeffVarFF(F_(0,1),var);
     );
     -- Now that the leading coefficient of F_(0,1) is a unit in the
     -- localization, make F_(0,1) monic and return it.
     
     tempM = matrix{{1,0},{0,1/lcg}};
     return ((1/lcg)*F_(0,1),(M*tempM)_{1});
)


--------------------------------------------------
-- DOCUMENTATION ---------------------------------
--------------------------------------------------

beginDocumentation()

document {
     Key => QuillenSuslin,
     Headline => "computes a free basis of a projective module over a polynomial ring",
     
     "Using the algorithms in Logar-Sturmfels and Fabianska-Quadrat, this package computes a free basis of 
     a projective module over a polynomial ring with coefficients in the rationals, integers, or a finite field.  It
     also provides methods to solve related problems involving completing a unimodular matrix to a square invertible
     matrix over a polynomial ring with coefficients in the rationals, integers, or a finite field, or a Laurent
     polynomial ring with coefficients in the rationals or a finite field.",
     
     PARA{}, "For mathematical background and applications, see ",

     UL {
	  {"A. Fabianska.", EM " Algorithmic analysis of presentations of groups and modules. ", HREF{"http://darwin.bth.rwth-aachen.de/opus/volltexte/2009/2950/","http://darwin.bth.rwth-aachen.de/opus/volltexte/2009/2950/"}, ", Jan 2009."},
	  {"T. Y. Lam.", EM " Serre's problem on projective modules.", " Springer Monographs in Mathematics.", " Springer-Verlag, Berlin, 2006."},
	  {"A. Logar and B. Sturmfels.", EM " Algorithms for the Quillen-Suslin theorem.", " J. Algebra, 145(1):231-239, 1992."},
	  {"A. Fabianska and A. Quadrat." , EM " Applications of the Quillen-Suslin theorem to multidimensional systems theory.", " Grobner bases in control theory and signal processing.", " Radon Series Comp. Appl. Math (3):23-106, 2007."}
	},
     
     }

document {
     Key => {changeVar,(changeVar,Matrix,List)},
     Headline => "computes a change of variables which make the first entry of a unimodular row monic in a specified variable",
     Usage => " (A,B,C) = changeVar(U,L)",
     Inputs => {
	  "U" => Matrix => {" a unimodular row"},
	  "L" => List => {" the variables appearing in the matrix, ordered so that the first entry of the new row will become monic in the last variable in the list"},
     	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)"},
     },
     Outputs => {
	  "A" => Matrix => {" a unimodular matrix that when multiplied by the row will make the 
	       first entry monic after the change of variables"}, 
	  "B" => Matrix => {" change of variables"}, 	       
	  "C" => Matrix => {" inverse change of variables"}, 	       
     },

     PARA{}, "The purpose of changeVar is to compute a unimodular matrix ", TT "A", " and an invertible change of variables (given as a matrix ", TT "B", " with the
     inverse change of variables given by ", TT "C", ") so that after multiplying the given unimodular row ", TT "U", " by ", TT "A", " and applying the substitution ",
     TT "B", " (with ", TT "sub(U*A,B)", "), the first entry of the new row becomes monic in the last variable in the list ", TT "L", ".",
     PARA{}, "This method is primarily used before applying ", TO "horrocks",", since horrocks requires the first entry of the unimodular row to be monic in the given variable.",
     
     EXAMPLE {
	  "R = ZZ[x]",
	  "U = matrix{{12*x^2+20*x+7,4*x^2+12*x+5,12*x^2+44*x+35}}",
	  "isUnimodular U",
	  "(A,B,C) = changeVar(U,{x})",
	  "U' = sub(U*A,B)",
	  "isUnimodular U'",
     },
     
     PARA{}, "Notice that after multiplying ", TT "U", " by the unimodular matrix ", TT "A", " and applying the
     change of variables ", TT "B", " (using the ", TO2(substitute, "sub"), " command), the first entry in ", TT "U'", " above is now monic in ", TT "x", ".",
     
     PARA{}, "The order of the variables given in the list matter, as changeVar 
     will construct a change of variable so that the new unimodular row is monic in the ", EM "last",
     " variable of the list.",
     
     PARA{}, "In the next example, since we are using the command ", TT "changeVar(U,{x,y})", " the first
     entry in the row ", TT "sub(U*A,B)", " will be monic in ", TT "y",".",
     
     EXAMPLE {
	  "R = ZZ/7[x,y]",
	  "U = matrix{{2*x^2*y+x*y+1,3*x^2*y^2+x*y,5*x^3*y^2+x*y}}",
	  "isUnimodular U",
	  "(A,B,C) = changeVar(U,{x,y})",
	  "U' = sub(U*A,B)",
	  "isUnimodular U'",
     },
     
     PARA{}, "One can also check that the inverse change of variables, ", TT "C", ", will give
     the matrix ", TT "U*A", ", undoing the change of variables given by ", TT "B", ".",
     
     EXAMPLE {	  
	  "U'' = sub(U',C)",
	  "U'' == U*A",
     },
     
     SeeAlso => { "horrocks", "sub" },

}


document {
     Key => {completeMatrix,(completeMatrix,Matrix)},
     Headline => "completes a unimodular matrix over a polynomial ring or Laurent polynomial ring to a square invertible matrix",
     Usage => " M = completeMatrix U",
     Inputs => {
	  "U" => Matrix => {" a unimodular matrix over a polynomial ring with coefficients in ", TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " for ", 
	       TT "p", " a prime integer, or a Laurent polynomial ring over ", TT "QQ", " or ", TT "ZZ/p"},
	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)"},
	  },
     Outputs => {
	  "M" => Matrix => {" which completes ", TT "U", " to a square invertible matrix"}, 
	  },
     
     PARA{}, "Given a unimodular ", TEX/// m \times \ n ///, "matrix over a polynomial ring with coefficients in ",
     TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " with ", TT "p", " a prime integer, this method returns the inverse of the matrix returned by ", TO "qsAlgorithm", ".  The
     first ", TEX ///m///, " rows or columns (depending on whether ", TEX/// m < n ///, " or ", TEX /// m > n///, ") of this matrix are equal to ", TT "U", " and the determinant of the matrix is a unit in the polynomial ring.",
     
     EXAMPLE {
	  "R = ZZ/101[x,y]",
	  "U = matrix{{x^2*y+1,x+y-2,2*x*y}}",
	  "isUnimodular U",
	  "M = completeMatrix U",
	  "det M",
     },

     PARA{}, "The method can also be used over a Laurent polynomial ring with coefficients in ", TT "QQ", " or ", TT "ZZ/p", " for ", TT "p", " a prime
     integer.  The following example demonstrates how to construct a Laurent polynomial ring and also how to use the method on a unimodular matrix over
     the ring.",
     
     EXAMPLE {
	  "R = QQ[x,Inverses => true,MonomialOrder => Lex]",
	  "U = matrix{{3*x^-1-2-2*x+2*x^2, 3*x^-1-2*x,2*x},{6*x^-1+25-23*x-16*x^2+20*x^3, 6*x^-1+29-4*x-20*x^2,2+4*x+20*x^2}}",
	  "M = completeMatrix U",
	  "det M",	  
     },

     SeeAlso => { "qsAlgorithm" },
}


document {
     Key => {computeFreeBasis,(computeFreeBasis,Module),(computeFreeBasis,Ideal),[computeFreeBasis,CheckProjective]},
     Headline => "computes a free basis of a projective module",
     Usage => " computeFreeBasis M",
     Inputs => {
	  "M" => Module => {" a projective module over a polynomial ring"},
	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)"},
	  CheckProjective => Boolean => {" which gives the user the option to check whether the given module is projective"},
	  },
     Outputs => {
	  Matrix => {" a free generating set for ", TT "M"},
	  },
     
     PARA{}, "Using the fact that every finitely generated projective module over a polynomial ring R is isomorphic to the kernel of some surjection between 
     free modules, we define a surjective R-linear map from ", TEX///R^3 \to \ R///," to get a projective module.",
     
     EXAMPLE {
	  "R = QQ[x,y]",
	  "M = matrix{{x^2*y+1,x+y-2,2*x*y}}", 
	  "isUnimodular M",
	  },
     
     PARA{}, "Let P be the stably-free (and hence projective) kernel with rank 2.  Notice that the first generator is a linear combination of the other two.",
     
     EXAMPLE {
	  "P = ker M",
	  "isProjective P",
	  "rank P",
	  "mingens P",
	  "syz mingens P", 
	  },
     
     PARA{}, "Notice that the native command ", TO "mingens", " does not return a free generating set.  We can use computeFreeBasis to construct
     a free generating set for P.",

     EXAMPLE {
	  "B = computeFreeBasis(P)",
	  "image B == P",
	  "syz B",
	  },
     }


document {
     Key => {getMaxIdeal,(getMaxIdeal,Ideal),(getMaxIdeal,Ideal,List),[horrocks,Verbose]},
     Headline => "computes a maximal ideal containing a given ideal in a polynomial ring",
     Usage => " M = getMaxIdeal I \n M = getMaxIdeal(I,L)",
     Inputs => {
	  "I" => Ideal => {" an ideal of a polynomial ring over ", TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " for ", 
	       TT "p", " a prime integer."},
	  "L" => List => {" a subset of the variables of the ring.  This list must contain the variables 
	       that appear in the ideal ", TT "I", ". By default, ", TT "L", " is assumed 
	       to be the list of variables in the ring."},
	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)."},
     },
     Outputs => {
	  "M" => Ideal => {" maximal with respect to the variables in ", TT "L","."}, 
	  },

     PARA{}, "In absence of an input list, getMaxIdeal yields a maximal ideal containing the input ideal ", TT "I", ".",

     EXAMPLE {
	  " R = ZZ/3[x,y]",
	  " I = ideal(x*(x-1)*(x-2)*y*(y-1)*(y-2)+1)",
	  " J = getMaxIdeal I",
	  " isSubset(I,J)",
	  },
     
     PARA{}, "The function ", TT "isSubset", " shows that ", TT "I", " is contained in our new ideal.  To see that ", TT "J", " is indeed maximal,
      consider the codimension and the minimal primes.",
     
     EXAMPLE {
	  " codim J", 
	  " P = minimalPrimes J",
	  " J == P_0",
	  },
     
     PARA{}, "The optional list argument allows us to restrict our maximal ideal to a polynomial ring defined by a subset
     of the variables of the ambient ring.  Note that the list must contain the variables that appear in the generators of ",
     TT "I", ".",
     
     EXAMPLE {
	  "R = ZZ[x,y,z,a,b,c]",
	  "I = ideal(27,x^2+1)",
	  "J = getMaxIdeal(I,{x,y,z})",
	  "isSubset(I,J)",
	  },
     }


document {
     Key => {horrocks,(horrocks,Matrix,RingElement,Ideal),[horrocks,CheckUnimodular]},
     Headline => "computes a local solution to the unimodular row problem over a localization
     at a maximal ideal",
     Usage => " U = horrocks(f,var,M)",
     Inputs => {
	  "f" => Matrix => {" a unimodular matrix over a polynomial ring with coefficients in ", TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " for ", 
	       TT "p", " a prime integer"},
	  "var" => RingElement => {" a variable in the polynomial ring"},
	  "M" => Ideal => {" a maximal ideal in the polynomial ring excluding the given variable"},
	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)"},
	  CheckUnimodular => Boolean => {" which gives the user the option to test whether the given matrix is unimodular"},
	  },
     Outputs => {
	  "U" => Matrix => {" a local solution to the unimodular row problem"},
	  },

     PARA{}, "Given a unimodular row ", TT "f", " over a polynomial ring ", TEX/// R = S[x_1,\ldots,x_{n-1}][x_n]///,
     " (where S is either ", TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p",") and a maximal ideal ", TT "M", " in ",TEX/// S[x_1,\ldots,x_{n-1}] ///,
     ", horrocks computes a unimodular matrix ", TT "U", " over ", TEX/// (S[x_1,\ldots,x_{n-1}]_M)[x_n] ///, " which
     solves the unimodular row problem for ", TT "f", ".  That is, ", TT "f*U", " is of the form ", TT "|1 0 ... 0|",".",
     
     EXAMPLE {
	  "S = ZZ[x]",
  	  "M = ideal(2,x)",	  	  
	  "R = ZZ[x,y]",
	  "f = matrix{{y^2+x^2*y+x,3*y+1,x^2}}",
	  "U = horrocks(f,y,M)",
     },

     PARA{}, "It is worth noting that ", TT "M", " can be chosen to be an extension of a maximal ideal from ", TEX/// S[x_1,\ldots,x_{n-1}]///,
     " to the ring ", TEX///R///, ".  We repeat the above example with this convention.",
     
     EXAMPLE {
	  "R = ZZ[x,y]",
	  "f = matrix{{y^2+x^2*y+x,3*y+1,x^2}}",
  	  "M = ideal(2,x)",	  	  	  
	  "U = horrocks(f,y,M)",
     },

     PARA{}, "One can check that ", TT "U", " is unimodular over ", TEX/// (\mathbb{Z}[x]_{(2,x)})[y] ///, " (i.e. the determinant is a unit in
     this ring), and that ", TT "U", " solves the unimodular row problem for ", TT "f", " (i.e. ", TT "f*U = |1 0 0|", ").",
     
     EXAMPLE {
	  "det U",
	  "f*U",
     },

     PARA{}, "Before applying horrocks one must guarantee that the first entry in the unimodular row
     is monic in the given variable.  This can be accomplished, for example, by using the method ", TO "changeVar", ".  One can
     also use the method ", TO "getMaxIdeal", " to construct a maximal ideal to localize at.  We demonstrate the use of these
     methods in the next example.",
     
     EXAMPLE {
	  "R = ZZ/11[x,y]",
	  "f = matrix{{4*x^2-4*x*y+2*y^2+3*x-2*y+3,-2*x^2+2*x*y-4*y^2-2*x-2,-5*x^2-4*x*y-5*y^2+4*x+3*y+5}}",
	  "isUnimodular f",
	  "M = getMaxIdeal(ideal(0_R),{x})",
	  "(N,subs,invSubs) = changeVar(f,{x,y})",
	  "g = sub(f*N,subs)",
	  "U = horrocks(g,y,M)",
     },
     
     PARA{}, "Again we can check that ", TT "U", " is unimodular over ", TEX/// ((\mathbb{Z}/11\mathbb{Z})[x]_{(x)})[y] ///,
     " and that ", TT "U", " solves the unimodular row problem for ", TT "g", ".",
     
     EXAMPLE {
	  "det U",
	  "g*U",
     },

     Caveat => { "horrocks may give undesired results if the given ideal is not actually maximal."},

     SeeAlso => { "changeVar" , "getMaxIdeal" },
     
}


document {
     Key => {isProjective,(isProjective,Module)},
     Headline => "determines if a given module is projective with constant rank over a Noetherian ring",
     Usage => " isProjective M",
     Inputs => {
	  "M" => Module => {" a finitely generated module over a Noetherian ring."},
	  },
     Outputs => {
	  Boolean 
	  },

     PARA{}, "This method determines if the given ", TEX///R///, "-module is projective with constant rank by considering 
     the ideal of minors of its presentation matrix.  In particular, if ", TEX///\phi///, " is the presentation matrix
     of the module ", TT "M", ", let ", TEX///I_t(\phi)///, " be the ideal in ", TEX///R///, " generated by the ", TEX///t \times\ t///, 
     " minors of ", TEX///\phi///, ".  If there exists an ", TEX///r///, " such that ", TEX///I_r(\phi) = R///, " and ", 
     TEX///I_{r+1}(\phi) = 0///, ", then we know that ", TT "M", " is necessarily projective of constant rank (see Proposition 1.4.10 
     of Bruns-Herzog below).  The method ", TT "isProjective", " calls on ", TO "maxMinors", " to compute the ideal of minors ", 
     TEX///I_r(\phi)///, " such that ", TEX///I_r(\phi) \neq 0///, " and ", TEX///I_{r+1}(\phi) = 0///, ". If ", TEX///I_r(\phi)///, 
     " is the whole ring, then the module ", TT "M", " is projective with constant rank.",
     
     PARA{}, "Reference:",
     
     UL {
	  {"W. Bruns and J. Herzog.", EM " Cohen-Macaulay Rings.", " Cambridge Studies in Advanced Mathematics, 39. Cambridge University Press, Cambridge, 1993. xii+403 pp. ISBN: 0-521-41068-1."}
	},
   
     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "P = matrix{{x^2*y+1,x+y-2,2*x*y}}", 
	  "isProjective ker P",
	  "M = matrix{{-y,-z^2,0},{x,0,-z^2},{0,x^2,x*y}}",
	  "isProjective cokernel M",
	  "I = ideal(x^2,x*y,z^2)",
	  "isProjective module I",
	  "isProjective R^3",
	  "isProjective module ideal x",
     },

     Caveat => { "If the method outputs ", TT "false", ", this only implies the module in question is not projective with constant 
	  rank.  However, if the ring is a domain, then all finitely generated projective modules have constant rank.  In this 
	  scenario, ", TT "isProjective", " outputs ", TT "true", " if and only if the module is projective." },
     
     SeeAlso => { "maxMinors" },
     
     }
   
document {
     Key => {isUnimodular,(isUnimodular,Matrix)},
     Headline => "determines if a given matrix is unimodular",
     Usage => " isUnimodular M",
     Inputs => {
	  "M" => Matrix => {" a matrix over a polynomial ring"},
	  },
     Outputs => {
	  Boolean 
	  },
     
     PARA{}, "An ", TEX/// m \times \ n ///," matrix over a polynomial ring is unimodular if its maximal minors
     generate the entire ring.  If ", TEX/// m \leq \ n ///," then this property is equivalent to the matrix being
     right-invertible and if ", TEX/// m \geq \ n ///," then this property is equivalent to the matrix being
     left-invertible.",

     EXAMPLE {
	  "R = QQ[x,y,z]",
	  "A = matrix{{x^2*y+1,x+y-2,2*x*y}}", 
	  "isUnimodular A",
	  "B = matrix{{x*y+x*z+y*z-1},{x^2+y^2}, {y^2+z^2}, {z^2}}",
	  "isUnimodular B",
	  "I = ideal(x^2,x*y,z^2)",
	  "isUnimodular presentation module I",
     },
     
     SeeAlso => { "minors" },     
     }

document {
     Key => {maxMinors, (maxMinors,Matrix)},
     Headline => "computes the ideal generated by the maximal non-vanishing minors of a given matrix",
     Usage => "maxMinors M",
     Inputs => {
	  "M" => Matrix => {" with entries in a ring ", TEX///R///, "."},
	  },
     Outputs => {
	  Ideal => {"the ideal generated by maximal non-vanishing minors of the matrix ", TT "M", "."},
	  },
     
     PARA{}, "Let ", TEX///I_t(M)///, " be the ideal in ", TEX///R///, " generated by the ", TEX///t \times\ t///, 
     " minors of ", TEX///M///, ".  If there exists an ", TEX///r///, " such that ", TEX///I_r(M)///, " is non-zero and ", 
     TEX///I_{r+1}(\phi) = 0///, ", then ", TT "maxMinors M", " gives ", TEX///I_r(M)///, ".",

     EXAMPLE {
	  "R = QQ[x,y]",
	  "M = matrix{{x,0},{-y,x},{0,-y}}",
	  "maxMinors M",
     },
     
     PARA{}, "This method returns the unit ideal as the ideal of maximal minors of the zero matrix.",
     
     EXAMPLE {
	  "N = matrix{{0_R}}",
	  "maxMinors N",
     },

     SeeAlso => { "minors" },
}

document {
     Key => {patch, (patch,List,RingElement)},
     Headline => "patch together local solutions to eliminate a variable",
     Usage => "patch(L,var)",
     Inputs => {
	  "L" => List => {" a list of local solutions to the unimodular row problem for some unimodular row ", TT "f"},
	  "var" => RingElement => {" the current variable to eliminate"},
	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)"},
	  },
     Outputs => {
	  Matrix => {" a unimodular matrix ",TT "U", " so that ", TT "f*U", " is the same as evaluating ", TT "f", " when ", TT "var", " = 0"},
	  },
     
     PARA{}, "Given a unimodular row ", TT "f", " over a polynomial ring ", TEX/// R[x_1,\ldots,x_n]///, " (where ", TEX /// R ///, " is either ", TT "QQ", ", ", TT "ZZ",", or ", TT "ZZ/p",") one can compute a collection of matrices ",
     TEX /// \{U_i\}^r_{i=1}///, " and maximal ideals ", TEX /// m_1,\ldots,m_r///, " so that:",
     PARA{}, "1. Each ", TEX /// U_i ///, " is unimodular over ", TEX /// (R[x_1,\ldots,x_{n-1}]_{m_i})[x_n] ///, ",",
     PARA{}, "2. Each ", TEX /// U_i///, " solves the unimodular row problem for ", TT "f in ", TEX /// (R[x_1,\ldots,x_{n-1}]_{m_i})[x_n]///, ", and",
     PARA{}, "3. Letting ", TEX /// d_i ///, " denote the common denominator of the entries of", TEX /// U_i ///, " in ", TEX /// R[x_1,\ldots,x_{n-1}] ///, ", we have ",
     TEX /// (d_1,\ldots,d_r) = R[x_1,\ldots,x_{n-1}]///, ".",
     PARA{}, "One can accomplish this, for example, by repeatedly using ", TO "getMaxIdeal", " and ", TO "horrocks", " as in the following example.",
     
     EXAMPLE {
	  "R = ZZ[x,y]",
	  "f = matrix{{y^3+2*x*y^2,3*y+1,x^3}}",
	  "isUnimodular f",
	  "m1 = getMaxIdeal(ideal(0_R),{x})",
	  "L1 = horrocks(f,y,m1)",
	  "m2 = getMaxIdeal(sub(ideal(6*x-1),R),{x})",
	  "L2 = horrocks(f,y,m2)",
	  "sub(ideal(6*x-1,x^3),R) == ideal(1_R)",
	  "L = {L1,L2}",	  
     },

     PARA{}, "Once we have constructed a list ", TT "L", " of local solutions of the unimodular row problem for ", TT "f", ", we can use patch to create a matrix ", TT "U", " so that the product ", TT "fU" , 
     " is the same as ", TT "f", " evaluated at ", TT "y", "=0.",
     
     EXAMPLE {
	  "U = patch(L,y)",
	  "isUnimodular U",
	  "f*U",
	  "sub(f,{y => 0})",
     },

     SeeAlso => { "getMaxIdeal","horrocks" },
}


document {
     Key => {qsAlgorithm,(qsAlgorithm,Matrix),[qsAlgorithm,CheckUnimodular]},
     Headline => "computes a solution to the unimodular matrix problem",
     Usage => " M = qsAlgorithm U",
     Inputs => {
	  "U" => Matrix => {" a unimodular matrix over a polynomial ring with coefficients in ", TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " for ", 
	       TT "p", " a prime integer, or a Laurent polynomial ring over ", TT "QQ", " or ", TT "ZZ/p"},
	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)"},
	  CheckUnimodular => Boolean => {" which gives the user the option to check whether the matrix is unimodular"},
	  },
     Outputs => {
	  "M" => Matrix => {" such that ", TT "U*M", " is of the form ", TEX ///[I \ 0]///, " or ", TEX ///[I \ 0]^T///, ", where I is an identity matrix"}, 
	  },
     
     PARA{}, "Given a unimodular ", TEX/// m \times \ n ///, "matrix over a polynomial ring with coefficients in ",
     TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " with ", TT "p", " a prime integer, this method uses the algorithm of Logar-Sturmfels to compute a solution of the
     unimodular matrix problem for ", TT "U", ".  In other words, this method computes a square unimodular matrix ", TT "M", " such that if ", TEX ///m \leq \ n///,
     " then ", TT "U*M", " is of the form ", TEX ///[I \ 0]///, " where I is an ", TEX ///m \times \ m///, " identity matrix, and if ",
     TEX ///m \geq \ n///, " then ", TT "M", " is of the form ", TEX ///[I \ 0]^T///, ", where I is an ", TEX ///n \times \ n///, " identity matrix.",
     
     EXAMPLE {
	  "R = ZZ/101[x,y]",
	  "U = matrix{{x^2*y+1,x+y-2,2*x*y}}",
	  "isUnimodular U",
	  "M = qsAlgorithm U",
	  "isUnimodular M",
	  "U*M",
     },

     PARA{}, "The inverse of the matrix obtained by qsAlgorithm gives a completion of the original unimodular matrix ", TT "U",
     " to a square invertible matrix over the polynomial ring.  This completion can also be obtained directly by using the method ",
     TO "completeMatrix", ".",
     
     EXAMPLE {
	  "I = inverse M",
	  "det I",
     },

     PARA{}, "The method can also be used over a Laurent polynomial ring with coefficients in ", TT "QQ", " or ", TT "ZZ/p", " for ", TT "p", " a prime
     integer.  The following example demonstrates how to construct a Laurent polynomial ring and also how to use the method on a unimodular matrix over
     the ring.",
     
     EXAMPLE {
	  "R = QQ[x,Inverses => true,MonomialOrder => Lex]",
	  "U = matrix{{3*x^-1-2-2*x+2*x^2, 3*x^-1-2*x,2*x},{6*x^-1+25-23*x-16*x^2+20*x^3, 6*x^-1+29-4*x-20*x^2,2+4*x+20*x^2}}",
	  "M = qsAlgorithm U",
	  "det M",
	  "U*M",	  
     },


     SeeAlso => { "completeMatrix" },
}

document {
     Key => {qsIsomorphism,(qsIsomorphism,Module),[qsIsomorphism,CheckProjective]},
     Headline => "computes an isomorphism between a free module and a given projective module",
     Usage => " qsIsomorphism M",
     Inputs => {
	  "M" => Module => {" a projective module over a polynomial ring with coefficients in ", TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " for ", 
	       TT "p", " a prime integer, or a Laurent polynomial ring over ", TT "QQ", " or ", TT "ZZ/p"},
	  Verbose => ZZ => {" which controls the level of output of the method (0, 1, 2, 3, or 4)"},
	  CheckProjective => Boolean => {" which gives the user the option to check whether the module is projective"},
	  },
     Outputs => {
	  "M" => Matrix => {" which is an isomorphism from a free module to the given projective module M"}, 
	  },
     
     PARA{}, "Given a projective module ", TT "M", " over a polynomial ring with coefficients in ",
     TT "QQ", ", ", TT "ZZ", ", or ", TT "ZZ/p", " with ", TT "p", " a prime integer, this method uses algorithms of Logar-Sturmfels and Fabianska-Quadrat to compute an isomorphism
     from a free module ", TT "F", " to the projective module ", TT "M", ".  The following gives examples of constructing such isomorphisms in the cases where the module is a cokernel, kernel, image, or coimage of a unimodular matrix.",
     
     EXAMPLE {
	  "R = ZZ/101[x,y,z]",
	  -- "f = matrix{{x*y+x*z+y*z-1,x^2+y^2,y^2+z^2,z^2}}",
	     -- This is the preferred example for the documentation, 
	     -- but M2 1.6 has a bug preventing it from compiling with it.  
	     -- This is not the case with M2 1.5.
	  "f = matrix{{x^2*y+1,x+y-2,2*x*y}}", -- Less robust example but works in M2 1.6.
	  "isUnimodular f",
	  "P1 = coker transpose f -- Construct the cokernel of the transpose of f.",
	  "isProjective P1",
	  "rank P1",
	  "phi1 = qsIsomorphism P1",
	  "isIsomorphism phi1",
	  "image phi1 == P1",
	  "P2 = ker f -- Construct the kernel of f.",
	  "isProjective P2",
	  "rank P2",
	  "phi2 = qsIsomorphism P2",
	  "isIsomorphism phi2",
	  "image phi2 == P2",
	  "P3 = image f -- Construct the image of f.",
	  "isProjective P3",
	  "rank P3",
	  "phi3 = qsIsomorphism P3",
	  "isIsomorphism phi3",
	  "image phi3 == P3",
	  "P4 = coimage f -- Construct the coimage of f.",
	  "isProjective P4",
	  "rank P4",
	  "phi4 = qsIsomorphism P4",
	  "isIsomorphism phi4",
	  "image phi4 == P4",
     },

     SeeAlso => { "computeFreeBasis", "qsAlgorithm" },
}


-- Documentation for Optional Inputs

document {
     Key => {CheckProjective},
     Headline => "optional input which gives the user the option to check whether the given module is projective",
}

document {
     Key => {CheckUnimodular},
     Headline => "optional input which gives the user the option to check whether the given matrix is unimodular",
}


--------------------------------------------------
-- TESTS -----------------------------------------
--------------------------------------------------


-- Test 0
-- isProjective
TEST ///
     R = ZZ/101[x,y,z];
     assert(isProjective cokernel matrix{{x^2*y+1,x+y-2,2*x*y}}  == true);
     assert(isProjective ker matrix{{x^2*y+1,x+y-2,2*x*y}}  == true);     
     assert(isProjective(R^3) == true);
     assert(isProjective cokernel matrix{{ -y,-z^2,0 },{ x,0,-z^2}, { 0 ,x^2, x*y } } == false );
     assert(isProjective ker matrix{{ -y,-z^2,0 },{ x,0,-z^2}, { 0 ,x^2, x*y } } == true );
--   assert(isProjective ker matrix{{x*y+x*z+y*z-1, x^2+y^2, y^2+z^2, z^2}} == true ); -- works with M2 1.5
     
     R = ZZ[x,y];
     assert(isProjective(R^7) == true);
     assert(isProjective ker matrix{{2*x^3+2*x^2+1,2*(x^4+x^2),2*(x^5+x^2)}} == true);
     assert(isProjective ker matrix{{12*x^2+20*x+7,4*x^2+12*x+5,12*x^2+44*x+35}} == true);
     
     R = QQ[x,y];
     assert(isProjective(R^20) == true);
     assert(isProjective ker matrix{{5*x^2+8*x*y+8*y^2+8*x+6,7*x^2+4*y^2+8*x+6*y+3,7*x^2+4*y^2+8*x+6*y+1}} == true);
     assert(isProjective module ideal(0_R) == true);
///

-- Test 1
--isUnimodular
TEST ///
     R = ZZ/101[x,y,z];
     assert(isUnimodular matrix{{x^2*y+1,x+y-2,2*x*y}} == true);
     assert(isUnimodular matrix{{ -y,-z^2,0 },{ x,0,-z^2}, { 0 ,x^2, x*y } } == false );
     assert(isUnimodular id_(R^3) == true );
///

-- Test 2
-- maxMinors
TEST ///
     R = ZZ/101[x,y,z];
     assert(maxMinors matrix{{x^2*y+1,x+y-2,2*x*y}}  == ideal(1_R));
     assert(maxMinors matrix{{ -y,-z^2,0 },{ x,0,-z^2}, { 0 ,x^2, x*y } } == ideal(y*z^2, x*z^2, x*y^2, x^2*y, x^3, z^4 ) );
///

-- Test 3
-- changeVar
TEST ///
     R = ZZ[x,y];
     f = matrix{{3*x+3*y+17,6*x+9*y+24,4*x+7*y+11}};
     (U,A,B) = changeVar(f,{x,y});
     assert((coefficients((sub(f*U,A))_(0,0),Variables =>{y}))#1_(0,0) == 1);
     
     f = matrix{{12*x^2+20*x+7,4*x^2+12*x+5,12*x^2+44*x+35}};
     (U,A,B) = changeVar(f,{x});
     assert((coefficients((sub(f*U,A))_(0,0),Variables =>{x}))#1_(0,0) == 1);
     
     R = QQ[x,y];
     f = matrix{{5*x^2+8*x*y+8*y^2+8*x+6,7*x^2+4*y^2+8*x+6*y+3,7*x^2+4*y^2+8*x+6*y+1}}
     (U,A,B) = changeVar(f,{x,y});
     assert((coefficients((sub(f*U,A))_(0,0),Variables =>{y}))#1_(0,0) == 1);
          
     R = ZZ/7[x,y];
     f = matrix{{2*x^2*y+x*y+1,3*x^2*y^2+x*y,5*x^3*y^2+x*y}}
     (U,A,B) = changeVar(f,{x,y});
     assert((coefficients((sub(f*U,A))_(0,0),Variables =>{y}))#1_(0,0) == 1);
///

-- Test 4
-- computeFreeBasis
TEST ///
     R = ZZ/101[x,y,z];
     assert(syz computeFreeBasis R^5 == 0);
     assert(image computeFreeBasis R^5 == R^5); 
     P = ker matrix{{x^2*y+1,x+y-2,2*x*y}}; -- less robust check 
--   P = ker matrix{{x*y+x*z+y*z-1, x^2+y^2, y^2+z^2, z^2}}; -- Works with M2 1.5    
     F = computeFreeBasis P;
     assert(syz gens image F == 0);
     assert(image F == P);
     
     R = ZZ[x];
     P = ker matrix{{2*x^3+2*x^2+1,2*(x^4+x^2),2*(x^5+x^2)}};
     F = computeFreeBasis P;
     assert(syz gens image F == 0);
     assert(image F == P);
     
     R = QQ[x,y];
     P = ker matrix{{x^2*y+1,x+y-2,2*x*y}};
     F = computeFreeBasis P;
     assert(syz gens image F == 0);
     assert(image F == P);
     
     R = ZZ/101[x,y,z];
     P = coker transpose matrix{{x*y+x*z+y*z-1, x^2+y^2, y^2+z^2, z^2}};
     F = computeFreeBasis P;
     assert(syz gens image F == 0);
     assert(image map(P,R^3,F) == P);     
///

-- Test 5
-- completeMatrix
TEST ///
     R = ZZ[x,y,z];
     f = matrix{{x^2+y^2+z^2-1, x^2+y^2, y^2+z^2, z^2}};
     U = completeMatrix f;
     assert( rank source U == rank target U );
     assert( ideal det U == ideal(1_R) );
     
     R = ZZ/101[x,y,z];
     f = matrix{{x*y+x*z+y*z-1, x^2+y^2, y^2+z^2, z^2}};
     U = completeMatrix f;
     assert( rank source U == rank target U );
     assert( ideal det U == ideal(1_R) );
  
-- fails under M2 version 1.9.3, see git issue #XX.
--     R = ZZ[x];
--     f = matrix{{12*x^2+20*x+7,4*x^2+12*x+5,12*x^2+44*x+35}};
--     U = completeMatrix f;
--     assert( rank source U == rank target U );
--     assert( ideal det U == ideal(1_R) );
     
     R = QQ[x,y];
     f = matrix{{x^2*y+1,x+y-2,2*x*y}};
     U = completeMatrix f;
     assert( rank source U == rank target U );
     assert( ideal det U == ideal(1_R) );          

     -- Tests for the Laurent polynomial ring methods.
     R = QQ[x,Inverses => true,MonomialOrder => Lex]
     f = matrix{{x^-1+1+x,2*x^-2+1,1-x}}
     C = completeMatrix f
     assert( isUnit det C )
     
     R = QQ[x,Inverses => true,MonomialOrder => Lex]
     f = matrix{{3*x^-1-2-2*x+2*x^2, 3*x^-1-2*x,2*x},{6*x^-1+25-23*x-16*x^2+20*x^3, 6*x^-1+29-4*x-20*x^2,2+4*x+20*x^2}}
     C = completeMatrix f
     assert( isUnit det C )
///

-- Test 6
-- getMaxIdeal
TEST ///
     R = ZZ/3[x,y];
     I = ideal(x*(x-1)*(x-2)*y*(y-1)*(y-2)+1);
     M = getMaxIdeal I;
     assert( dim M == 0 );
     assert( M:I == ideal(1_R) );
     assert( #(primaryDecomposition M) == 1 );
     assert( (primaryDecomposition M)_0 == M );

     R = ZZ[x,y];
     I = ideal(27, x^3-1, 2*y-8);
     M = getMaxIdeal I;
     assert( M:I == ideal(1_R) );

     R = ZZ[x,y,z];
     I = ideal(45*y^2 , y^2*x^2 + 27*y^2*x + 37*y^2);
     M = getMaxIdeal I;
     assert( M:I == ideal(1_R) );
    
     R = QQ[x,y,z];
     I = ideal(x^2+y^2+z^2,2*x+5);
     M = getMaxIdeal I;
     assert( dim M == 0 );
     assert( M:I == ideal(1_R) );
     assert( #(primaryDecomposition M) == 1 );
     assert( (primaryDecomposition M)_0 == M );
///

-- Test 7
-- horrocks
TEST ///
     R = ZZ/101[x,y,z];
     f = matrix{{x*y+x*z+y*z-1, x^2+y^2, y^2+z^2, z^2}};
     V = gens R;
     M = getMaxIdeal(ideal(0_R),{first V});
     A = changeVar( f, V );
     g = sub( f*A_0, A_1 );
     U = horrocks( g, last V, M );
     assert( flatten entries(g*U) == prepend(1,apply(rank source f - 1, i -> 0) ) );
     
     R = QQ[x,y];
     f = matrix{{x^2*y+1,x+y-2,2*x*y}};
     V = gens R;
     M = getMaxIdeal(ideal(0_R),{first V});
     A = changeVar( f, V );
     g = sub( f*A_0, A_1 );
     U = horrocks( g, last V, M );
     assert( flatten entries(g*U) == prepend(1,apply(rank source f - 1, i -> 0) ) );
     
     R = ZZ[x];
     f = matrix{{12*x^2+20*x+7,4*x^2+12*x+5,12*x^2+44*x+35}};
     V = gens R;
     M = getMaxIdeal(ideal(0_R),{first V});
     A = changeVar( f, V );
     g = sub( f*A_0, A_1 );
     U = horrocks( g, last V, M );
     assert( flatten entries(g*U) == prepend(1,apply(rank source f - 1, i -> 0) ) );  
///

-- Test 8
-- patch
TEST ///
     R = ZZ[x,y];
     f = matrix{{y^3+2*x*y^2,3*y+1,x^3}};
     assert( isUnimodular f );
     m1 = getMaxIdeal(ideal(0_R),{x});
     L1 = horrocks(f,y,m1);
     m2 = getMaxIdeal(sub(ideal(6*x-1),R),{x});
     L2 = horrocks(f,y,m2);
     assert( sub(ideal(6*x-1,x^3),R) == ideal(1_R) );
     L = {L1,L2};
     U = patch(L,y);
     assert( isUnimodular U );
     assert( flatten entries (f*U) == flatten entries sub( f, R/ideal(y) ) );
///

-- Test 9
-- qsAlgorithm
TEST ///
     R = ZZ/101[x,y,z];
     f = matrix{{x*y+x*z+y*z-1, x^2+y^2, y^2+z^2, z^2}};
     assert( isUnimodular f);
     U = qsAlgorithm f;
     assert( isUnimodular U );
     assert( flatten entries(f*U) == prepend(1,apply(rank source f - 1, i -> 0) ) );
     
     R = ZZ[x,y,z];
     f = matrix{{1,0,0,0,0},{0,1,0,0,0},{0,0,1,0,0},{0,0,0,1,0}}
     assert( isUnimodular f);
     U = qsAlgorithm f;
     assert( isUnimodular U );
     assert( f == f*U );

     R = ZZ[x];
     f = matrix{{12*x^2+20*x+7,4*x^2+12*x+5,12*x^2+44*x+35}};
     assert( isUnimodular f);
     U = qsAlgorithm f;
     assert( isUnimodular U );
     assert( flatten entries(f*U) == prepend(1,apply(rank source f - 1, i -> 0) ) );
     
     R = QQ[x,y,z];
     f = matrix{{x^2*y+1,x+y-2,2*x*y}};
     assert( isUnimodular f);
     U = qsAlgorithm f;
     assert( isUnimodular U );
     assert( flatten entries(f*U) == prepend(1,apply(rank source f - 1, i -> 0) ) );

     -- Tests for the Laurent polynomial ring methods.
     R = QQ[x,Inverses => true,MonomialOrder => Lex]
     f = matrix{{x^-1+1+x,2*x^-2+1,1-x}}
     U = qsAlgorithm f
     assert( isUnit det U )
     assert(f*U == sub(matrix{{1,0,0}},R) )

     R = QQ[x,Inverses => true,MonomialOrder => Lex]
     f = matrix{{3*x^-1-2-2*x+2*x^2, 3*x^-1-2*x,2*x},{6*x^-1+25-23*x-16*x^2+20*x^3, 6*x^-1+29-4*x-20*x^2,2+4*x+20*x^2}}
     U = qsAlgorithm f
     assert( isUnit det U )
     assert( f*U == sub(matrix{{1,0,0},{0,1,0}},R) )
///

-- Test 10
-- qsIsomorphism
TEST ///
     R = ZZ/101[x,y,z];
     P = coker transpose matrix{{x^2*y+1,x+y-2,2*x*y}}; -- Less robust Check
--   P = coker transpose matrix{{x*y+x*z+y*z-1, x^2+y^2, y^2+z^2, z^2}}; -- Works with M2 1.5
     phi = qsIsomorphism P;
     assert(isIsomorphism phi);
     assert(image phi == P);
     
     R = ZZ[x,y];
     P = ker matrix{{x^2,2*y+1,y+x^5*y^2}};
     phi = qsIsomorphism P;
     assert(isIsomorphism phi);
     assert(image phi == P);
     
     R = QQ[x,y];
     P = ker matrix{{x^2*y+1,x+y-2,2*x*y}};
     phi = qsIsomorphism P;
     assert(isIsomorphism phi);
     assert(image phi == P);
     
     R = ZZ[x,y];
     P = ker matrix{{x^2,3*y+1,x+x^2*y+y^2}};
     phi = qsIsomorphism P;
     assert(isIsomorphism phi);
     assert(image phi == P);
///     
     
end

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------


-- Install
restart
uninstallPackage "QuillenSuslin"
restart
installPackage "QuillenSuslin"
check "QuillenSuslin"

-- Documentation
viewHelp QuillenSuslin

-- Checks for the tests above.
check QuillenSuslin -- Checks all tests
check(0, QuillenSuslin) -- isProjective
check(1, QuillenSuslin) -- isUnimodular
check(2, QuillenSuslin) -- maxMinors
check(3, QuillenSuslin) -- changeVar
check(4, QuillenSuslin) -- computeFreeBasis
check(5, QuillenSuslin) -- completeMatrix
check(6, QuillenSuslin) -- getMaxIdeal
check(7, QuillenSuslin) -- horrocks
check(8, QuillenSuslin) -- patch
check(9, QuillenSuslin) -- qsAlgorithm
check(10, QuillenSuslin) -- qsIsomorphism



restart
needsPackage "QuillenSuslin";
S = ZZ[x,y];
f = matrix {{x^2,2*y+1,x^5*y^2+y}};
isUnimodular f
(U1,subs,invSubs) = changeVar(f,{x,y})
f = sub(f*U1,subs)
A = ZZ[x];
m1 = getMaxIdeal(ideal(0_A),{x}) 
L1 = horrocks(f,y,m1)
m2 = getMaxIdeal(sub(ideal(2*x+1),A),{x})
L2 = horrocks(f,y,m2) 
sub(ideal(2*x+1,x),S) == ideal(1_S)
U = patch({L1,L2},y)
f*U
U = qsAlgorithm f
det U == -1
f*U
C = completeMatrix f
det C == -1
K = ker f
isProjective K
mingens K
syz mingens K
B = computeFreeBasis K
syz B == 0
image B == K
phi = qsIsomorphism K
source phi
target phi
isIsomorphism phi
