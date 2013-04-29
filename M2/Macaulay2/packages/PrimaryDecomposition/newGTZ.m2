-- THIS IS BEING DEVELOPED NOW!! --
-- This is not meant for general use yet.

newPackage(
	"newGTZ",
    	Version => "0.5", 
    	Date => "June 25, 2009",
    	Authors => {{Name => "Frank Moore", 
		  Email => "frankmoore@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~frankmoore/"},
	            {Name => "Mike Stillman",
		  Email => "mike@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/~mike/"},
	            {Name => "Ben Lundell",
		  Email => "blundell@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/People/Grads/lundell.html"},
	            {Name => "Jen Biermann",
		  Email => "jbiermann@math.cornell.edu", 
		  HomePage => "http://www.math.cornell.edu/People/Grads/biermann.html"}},
    	Headline => "A (new) Primary Decomposition Package following loosely the GTZ algorithm",
    	DebuggingMode => false
    	)

export {
     getSaturation,
     newPD,GeneralPosition,BasicPD,getSeparator}

needs "newGTZGenPos.m2"

-- Comments:
-- bug in "help RingElement"
-- radical does not work for ideals over ZZ with nonunit coefficients
-- cannot factor polynomials with coefficients not in QQ or ZZ/p

getHighestDegree = method()
getHighestDegree(List, RingElement) := (gs, x) ->
(
   --- the gs are a list of ring elements, and should be polynomials involving (among other things)
   --- the variable x
   --- gets the highest degree element with respect to x.
   first select(gs, g -> degree(x,g) == max apply(gs, h -> degree(x,h)))
)
TEST ///
restart
load "newGTZ.m2"
debug newGTZ
R = QQ[x,y,z]
myList = {x^2*y+2*x*y, z^3+x^2*y+x*y*z}
assert(getHighestDegree(myList, x) == x^2*y+2*x*y)
///

getPCoeff = method()
getPCoeff(RingElement, RingElement) := (g,p) ->
(
   --- returns the pair (n,g/p^n) where p^n is the largest power of p dividing g,
   if (p == 0_(ring g)) then (0_(ring g),g)
   else
   (
      pPower := p;
      f := g % pPower;
      n := 1;
      while (f == 0) do
      {
         n = n+1;
	 pPower = p*pPower;
         f = g % pPower;
      };
      (n-1, g // (pPower // p))
   )
)
TEST ///
restart
load "newGTZ.m2"
debug newGTZ
R = ZZ[x,y]
f = 125*x*y+25*x^2
assert(getPCoeff(f,5_R) == (2,x^2+5*x*y))
/// 

--- also return a smarter element for saturation?
getSaturation = method()
getSaturation(Ideal, RingElement) := (I,s) ->
(
   -- This function takes an ideal and a ring element s as input, and
   -- computes the saturation of I with respect to s, as well as the integer n
   -- such that (I:s^\infty) = (I:s^n) [which exists by Noetherianness]
   satI := saturate(I,s);
   -- add *option* to iteravely compute colons later?
   n := 0;
   J := compress((gens satI) % I);
   while J != 0 do (
      J = compress((s**J) % I);
      n = n+1;
   );
   (satI,n)
)
TEST ///
restart
load "newGTZ.m2"
R = ZZ/32003[a..d]
I = (ideal vars R)^3
assert (getSaturation(I,a) == (ideal 1_R,3))
assert (getSaturation(I,a*b) == (ideal 1_R,2))
J = (ideal (a,b,c-d))*I
assert (getSaturation(J,a) == (ideal 1_R, 4))
assert (getSaturation(J,d) == (ideal (c - d, b, a), 3))
///

getProductOrderLeadTerms = method()
getProductOrderLeadTerms(Ideal, List) := (I, myVars) ->
(
   -- This function takes an ideal I and a list of variables myVars as input
   -- and returns a pair of matrices (mons, cs) where mons are the monomials in the ideal
   -- of lead terms of a gb of I, and cs are the coefficients, but with respect to
   -- a product order kk[fiberVars][myVars].  See example below for behavior
   R := ring I;
   allVars := set gens R;
   fiberVars := toList(allVars - set myVars);
   RU := (coefficientRing R) monoid([fiberVars,myVars,
	     MonomialOrder=>{#fiberVars,#myVars}]);
   J := substitute(I,RU);
   leadTermsJ := leadTerm(1, gens gb J);
   (mons,cs) := coefficients(leadTermsJ, Variables => toList(0..#fiberVars-1));
   mons = substitute(mons, R);
   cs = substitute(cs, R);
   (mons, cs)
)
TEST ///
restart
load "newGTZ.m2"
debug newGTZ
R = ZZ/32003[x,y,z]
I = ideal (y^2*z-15*x*z^2,5*x^2*y^3-25*x*y*z^3)
independentSets(I)
getProductOrderLeadTerms(I,{x})
///

getProductOrderHashTable = method()
getProductOrderHashTable(Ideal, List) := (I, myVars) -> (
   -- This function takes an ideal and a list of variables as input.  The
   -- list of variables forms part of an independent set for the ideal I.
   -- Its output is just a HashTable version of the data output in the
   -- getProductOrderLeadTerms function.
   (mons,cs) := getProductOrderLeadTerms(I, myVars);
   monsList := flatten entries mons;
   monHash := hashTable for i from 0 to #monsList-1 list (
	monsList#i => flatten entries compress cs^{i});
   monHash
)

factorPHashTable = method()
factorPHashTable(HashTable,RingElement) := (monHash,p) ->
(
   --- monHash is a hash table of the form (monomial, list of polynomials) and p is a prime number or zero.
   --- The function moves the powers of p from the values to the keys of the HashTable monHash
   --- the output of this function has key, value pairs of the form (monomial, ideal).  Therefore, in the p = 0
   --- case we need only convert the value to the ideal it generates.
   if (p =!= 0_(ring p)) then (hashTable apply(pairs monHash, (m, val) -> (
	       pParts := apply(val, i -> getPCoeff(i,p));
	       minPPower := min (pParts / first);
	       newGs := select(pParts, (pow, g) -> pow === minPPower);
	       (p^minPPower*m, trim ideal (newGs / (x -> x#1))))))
   else (hashTable apply(pairs monHash, (m, val) -> (m, trim ideal val)))
)

minimalizeHashTable = method()
minimalizeHashTable(HashTable) := (monHash) ->
(
   --- the input has key, value pairs of the form (Monomial, ideal)
   --- use monomialIdeal instead of ideal for field coefficientRings?
   local minGens;
   kk := coefficientRing ring first keys monHash;
   if (isField kk) then minGens = set flatten entries gens monomialIdeal keys monHash else minGens = set flatten entries mingens ideal keys monHash;
   --if (p =!= 0_(ring p)) then minGens = set flatten entries mingens ideal keys monHash else minGens = set flatten entries gens monomialIdeal keys monHash;
   hashTable select(pairs monHash, (m,val) -> member(m,minGens))
)

getSeparatorZZ = method()
getSeparatorZZ(Ideal, List, RingElement) := (I, myVars, p) ->
(
   --- This function takes as input an ideal, a prime number p, and a list of variables so that
   --- p, together with the list of variables are an independent set for I after localizing at p.
   --- It returns a pair (s, monHash) where monHash is the internally used hashTable to construct
   --- s, and s is an element so that I = (I:s^\infty) \cap (I,s) [a separator].  This is the
   --- crucial decomposition in the GTZ algorithm.
   --- Step 1:  Make the hash table that contains the monomials and coeffs in the grobner basis of I
   ---          with respect to the product order given by the variables input
   monHash := getProductOrderHashTable(I,myVars);
   --- Step 2:  If necessary, move the powers of p from the values to the keys of the table, since
   ---          coefficients does not do this work for us
   monHash = factorPHashTable(monHash,p);
   --- Step 3:  Attempt to be a little clever coming up with a small separator by only using those values corresponding to minimal
   ---          generators of the ideal generated by the keys
   monHash = minimalizeHashTable(monHash);
   --- Step 4:  The s we desire is now the intersection (of the radicals of, but we can't do radical over ZZ) of the values in the HashTable we have built
   --- s := ((values monHash) / radical // intersect)_0;
   s := ((values monHash) // intersect)_0;
   (s, monHash)
)

getSeparator = method()
getSeparator(Ideal, List) := (I, myVars) ->
(
   p := 0_(ring I);
   monHash := getProductOrderHashTable(I,myVars);
   monHash = factorPHashTable(monHash,p);
   monHash = minimalizeHashTable(monHash);
   s := ((values monHash) / (i -> ideal gens radical i) // intersect)_0;
   (s, monHash)
)
TEST ///
restart
load "newGTZ.m2"
debug newGTZ
path = prepend("~/Mac2SVN/M2/Macaulay2/packages", path)
loadPackage "ExampleIdeals"
R = ZZ[vars(0..8)];
I = permanents(2, genericMatrix(R,3,3))
getSeparatorZZ(I,{a,b,c},2_(ring I))
R' = QQ[vars(0..8)];
use R'
I' = substitute(I,R');
getSeparator(I',{a,b,c})
newPD(I',Verbosity => 2)
///

--- not finished
--- do we instead need to carry around an ideal M, not p?
ZPD = method()
ZPD(Ideal, List, RingElement) := (I, myVars, p) ->
(
   --- This function takes an ideal, a set of variables, and a prime number or 0 as input.
   --- We assume that in the ring with the variables not in myVars inverted, I is zero dimensional.
   --- The return value is a primary decomposition of I (a list of primary ideals).
   if (myVars == {}) then I
   else
   (
      allVars := gens ring I;
      myNewVars := take(myVars, #myVars-1);
      lastVar := last myVars;
      gbJ := flatten entries gens gb eliminate (myNewVars, I);
      g := getHighestDegree(gbJ, lastVar);
      --- now need to factor g mod a maximal ideal M in R/M[lastVar] (where M = (p,variables not in myVars?))
      --- factorList will be a list of pairs (a,b) where a is an irred factor of g, and b is its order.
      factorList := {};
      --- Find s such that g^s is in I
      --- still use orderOfElement to do this?
      append apply(factorList, (a,b) -> set ZPD(I + ideal a^b, myNewVars, p))
   )
)

--- not finished
PPD0 = method()
PPD0(Ideal, List, RingElement) := (I, myVars, p) ->
(
   --- This function takes an ideal, a list of variables and a prime number or zero p as input
   --- the coefficient ring of the ring of I is a PID (currently only ZZ or a field will work)
   --- Also, we assume here that I \cap R is p-primary
   --- The output of PPD0 is the primary decomposition of I
   local retVal;
   --- WARNING: need to get an independent set that contains myVars...
   indSetsI := independentSets (I, Limit => 1);
   if (indSetsI == {1_(ring I)}) then (retVal = ZPD(I,myVars,p);)
   else
   (
      variable := first support first independentSets I;
      myNewVars := toList (set myVars - set {variable});
      mySep := first getSeparator(I, myNewVars, p);
      --- WARNING: Make sure PPD0 returns ideals in the original ring
      retVal = PPD0(I, myNewVars, p);
      --- REMARK:  Be more clever with this computation?  I.e., check if it really must be done?
      if ((ideal mySep + I) =!= ideal (1_(ring I))) then retVal = retVal | PPD0(I + ideal mySep,myVars,p);
      retVal = firstList;
   );
   retVal
)

myIntersect = ideals -> (
   result := ideals_0;
   for i from 1 to #ideals-1 do result = intersect(result,ideals_i);
   result
)

makeIrredundant = method();
makeIrredundant(List) := (idealList) ->
(
   if (idealList == {}) then error "Expected nonempty list.";
   runningIntersection := idealList_0;
   newIdealList := {idealList_0};
   for i from 1 to #idealList-1 do
   (
      tempIntersection := intersect(runningIntersection, idealList_i);
      if (runningIntersection != tempIntersection) then
      (
         newIdealList = append(newIdealList,idealList_i);
	 runningIntersection = tempIntersection;
      );
      i = i + 1;
   );   
   newIdealList
)

debug PrimaryDecomposition
-- uses findNonMember and removeScalarMultipleColumns, both of which call engine functions.
-- so must just import the PrimaryDecomposition package symbols with debug to use them
-- so I don't have to import the core symbols
getComponentSep = method()
getComponentSep(List,ZZ) := (compList,i) -> (
   R = ring first compList;
   ss := apply(toList(0..#compList-1), j -> if i == j then 0_R else findNonMember(compList#j, compList#i));
   -- the following throws out zeros, and elements which are                                                                                                                                                                   
   -- not constant multiples of others.                                                                                                                                                                                        
   seps := flatten entries removeScalarMultipleColumns matrix {ss};
   -- be even more clever here?
   product seps
)

debug PrimaryDecomposition
primDecZeroDim = method(Options => {Verbosity => 0, Strategy=> {}})
primDecZeroDim(Ideal, List, Ideal) := opts -> (I, variables, resultSoFar) ->
(
   pdList := time if member(BasicPD,set opts.Strategy) then primaryDecomposition I
             else if member(GeneralPosition, set opts.Strategy) then primDecZeroDimField(I,variables,resultSoFar,Verbosity => opts.Verbosity)
	     else 
             (
                compList := decompose I;
		if (#compList == 1) then {I}
		else for i from 0 to #compList-1 list (
			sep := getComponentSep(compList,i);
			trim saturate(I,sep)
                     )
             );
   answer := {};
   for i from 0 to #pdList-1 do
   (
      if (not isSubset(resultSoFar, pdList_i)) then
      (
         answer = append(answer, pdList_i);
	 resultSoFar = intersect(resultSoFar, pdList_i);
      );
   );
   (answer, resultSoFar)
)

newPD = method(Options => {Verbosity => 0, Strategy => {}})
newPD(Ideal) := opts -> (I) -> first PDWorker(I, ideal 1_(ring I), 0, opts)

PDWorker = method(Options => {Verbosity => 0, Strategy => {}})
PDWorker(Ideal, Ideal, ZZ) := opts -> (I, resultSoFar, callDepth) ->
(
   local retVal;
   local otherComps;
   comps := {};
   newResultSoFar := resultSoFar;
   --printSkip := concatenate (callDepth : ".");
   indSetI := independentSets(I, Limit => 1);
   if (indSetI == {1_(ring I)}) then (
	retVal = primDecZeroDim(I,{},resultSoFar,opts);
      if opts.Verbosity >= 2 then (
         << "ZD Components Found : " << netList retVal#0 << endl
      );
   )
   else
   (
      variables := support first indSetI;
      t1 := timing (mySep := first getSeparator(I, variables));
      --t2 := timing ((Isat,satIndex) := getSaturation(I,mySep));
      local Isat;
      local satIndex;
      local t2;
      if (mySep == 1) then (t2 = timing Isat = I; satIndex = 0;) else
      (
	 -- this section includes code from the PrimaryDecomposition package  
         t2 = timing (ret := minSatPPD(I,factors mySep));
         satIndex = 1;
         mySep = ret#2;
         Isat = ret#0;
      );
      if opts.Verbosity >= 2 then (
	 << "Sat Info : " << satIndex << ", " << mySep << endl;
         << "Sep Time : " << t1#0 << endl;
         << "Sat Time : " << t2#0 << endl;
      );
      if (not isSubset(resultSoFar,Isat)) then
      (
	 (comps,newResultSoFar) = primDecZeroDim(Isat, variables, resultSoFar,opts);
	 if opts.Verbosity >= 2 then (
            << "Components Found : " << netList comps << endl;
	 );
      )
      else
      (
         if (opts.Verbosity >= 2) then (
	    << "Skipping PD of saturation." << endl;
	 )
      );
      newI := I + ideal (mySep^satIndex);
      if (not isSubset(newResultSoFar,newI)) then
      (
         (otherComps,newResultSoFar) = PDWorker(newI, newResultSoFar, callDepth + 1,opts);
	 comps = join (comps, otherComps)
      );
      retVal = (comps, newResultSoFar);  
   );
   retVal
)

beginDocumentation()

doc ///
  Key
    newGTZ
  Headline
    An implementation of the GTZ primary decomposition algorithm
  Description
   Text
     A small example is below.
   Example
     R = ZZ/32003[a,b,c,h]
     I = ideal(a+b+c,a*b+b*c+a*c,a*b*c-h^3)
     newPD(I, Strategy=>{GeneralPosition})
///

doc ///
  Key
    newPD
  Headline
    Computes a primary decomposition of the input ideal I
  Usage
    newPD(I)
  Inputs
    I : Ideal
  Outputs
    pdList : List 
  Description
   Text
     Below is a brief example.
   Example
     R = ZZ/32003[a,b,c,h]
     I = ideal(a+b+c,a*b+b*c+a*c,a*b*c-h^3)
     newPD(I, Strategy=>{GeneralPosition})
     I = ideal I_*
     newPD(I, Strategy=>{BasicPD})
     I = ideal I_*
     newPD(I)
     I = ideal I_*
     newPD(I, Verbosity=>2)
///

doc ///
  Key
    BasicPD
  Headline
    newPD primary decomposition option.
  Usage
    newPD(I, Strategy=>{BasicPD})
  Description
   Text
     Uses the basic primaryDecomposition code (modulo some small reductions) to find the PD.
///

doc ///
  Key
    GeneralPosition
  Headline
    Option for using a general change of coordinates in newPD
  Usage
    newPD(I, Strategy=>{GeneralPosition})
  Description
   Text
     Uses the general position algorithm to perform the bulk of the primary decomposition work.
///

doc ///
  Key
    getSeparator
  Headline
    Blah
  Usage
    getSeparator(I,variables)
  Inputs
    I : Ideal
    variables : List
  Outputs
    f : RingElement
  Description
   Text
     Returns an element $f$ such that $I = (I,\ f)\ \cap\ (I\ :\ f^\infty)$.  Some effort is made to find an $\ f\ $ that is
     better for the subsequent primary decomposition computations (i.e. low degree).  
///

TEST ///
-- CORRECT
restart
load "newGTZ.m2"
R = QQ[a,b,c]
I = ideal apply(1 .. 3, i -> random(3,R))
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten gens I
time ourPD = newPD(I,Verbosity=>2); -- CRASHES M2
I = ideal flatten gens I
time m2PD = primaryDecomposition I -- CRASHES M2, but only after running the first newPD above!
///

TEST ///
-- CORRECT
restart
load "newGTZ.m2"
R = ZZ/32003[a,b,c,d]
I = ideal apply(1 .. 3, i -> random(3,R))
-- takes > 10 minutes, hangs in decompose routine
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
-- return 3 components, all primary according to existing m2 code, and intersect to I
-- also only takes 1.4 seconds on my MBP
time m2PD = primaryDecomposition I
///

TEST ///
-- CORRECT
restart
load "newGTZ.m2"
R = ZZ/32003[a,b,c,h]
I = ideal(a+b+c,a*b+b*c+a*c,a*b*c-h^3)
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten gens I
time m2PD = primaryDecomposition I;

-- CORRECT
restart
load "newGTZ.m2"
R = ZZ/32003[a,b,c,d,h]
I = ideal(a+b+c+d,a*b+b*c+c*d+d*a,a*b*c+b*c*d+c*d*a+d*a*b,a*b*c*d-h^4)
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time m2PD = primaryDecomposition I;

-- CORRECT
-- very slow here, must be the QQ coefficients
restart
load "newGTZ.m2"
R = QQ[a,b,c,d,h]
I = ideal(a+b+c+d,a*b+b*c+c*d+d*a,a*b*c+b*c*d+c*d*a+d*a*b,a*b*c*d-h^4)
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time m2PD = primaryDecomposition I;

-- CORRECT
-- *much* speedier on other examples with trim in the zeroDimGenPos code, but putting it in causes a bug -- namely
-- the independent sets change after a random change of coordinates (in this example)
restart
load "newGTZ.m2"
debug newGTZ
R = ZZ/32003[a,b,c,d,e,h]
I = ideal(
         a+b+c+d+e,
	 d*e+c*d+b*c+a*e+a*b,
	 c*d*e+b*c*d+a*d*e+a*b*e+a*b*c,
	 b*c*d*e+a*c*d*e+a*b*d*e+a*b*c*e+a*b*c*d,
	 a*b*c*d*e-h^5)
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time m2PD = primaryDecomposition I;

-- CORRECT
-- BIG time difference...
restart
load "newGTZ.m2"
debug newGTZ
R = QQ[a,b,c,d,e,h]
I = ideal(
         a+b+c+d+e,
	 d*e+c*d+b*c+a*e+a*b,
	 c*d*e+b*c*d+a*d*e+a*b*e+a*b*c,
	 b*c*d*e+a*c*d*e+a*b*d*e+a*b*c*e+a*b*c*d,
	 a*b*c*d*e-h^5)
-- 78 seconds
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition}); -- CRASH on Ubuntu
I = ideal flatten gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time m2PD = primaryDecomposition I;

--- UNKNOWN - Takes a very long time.
restart
load "newGTZ.m2"
R = ZZ/32003[a,b,c,d,e,f,h]
I = ideal(
         a+b+c+d+e+f,
	 a*b+b*c+c*d+d*e+e*f+a*f,
	 a*b*c+b*c*d+c*d*e+d*e*f+e*f*a+f*a*b,
	 a*b*c*d+b*c*d*e+c*d*e*f+d*e*f*a+e*f*a*b+f*a*b*c,
	 a*b*c*d*e+b*c*d*e*f+c*d*e*f*a+d*e*f*a*b+e*f*a*b*c+f*a*b*c*d,
	 a*b*c*d*e*f-h^6)
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time ourPD = newPD(I);
I = ideal flatten gens I
time ourPD2 = newPD(I,Verbosity=>2,Strategy=>{BasicPD});
I = ideal flatten gens I
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten gens I
time m2PD = primaryDecomposition I;

-- CORRECT
-- in this example, it seems we could be more careful with choosing the separator itself.  It goes quite far down the recursion
-- before it at last reaches the zero dimensional primary component.  Maybe nothing can be done about it.
restart
load "newGTZ.m2"
debug newGTZ
path = prepend("/Mac2SVN/M2/Macaulay2/packages", path)
load "/Mac2SVN/M2/Macaulay2/packages/ExampleIdeals.m2"
loadPackage "ExampleIdeals"
R = QQ[vars(0..8)];
I = permanents(2, genericMatrix(R,3,3))
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten entries gens I
time m2PD = primaryDecomposition I;

-- CORRECT
-- in this example, it seems we could be more careful with choosing the separator itself.  It goes quite far down the recursion
-- before it at last reaches the zero dimensional primary component.  Maybe nothing can be done about it.
restart
load "newGTZ.m2"
debug newGTZ
path = prepend("/Mac2SVN/M2/Macaulay2/packages", path)
load "/Mac2SVN/M2/Macaulay2/packages/ExampleIdeals.m2"
loadPackage "ExampleIdeals"
R = ZZ/32003[vars(0..8)];
I = permanents(2, genericMatrix(R,3,3))
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten entries gens I
time m2PD = primaryDecomposition I;

-- CORRECT
restart
load "newGTZ.m2"
R = QQ[b,s,t,u,v,w,x,y,z];
I = ideal"su - bv, tv - sw, vx - uy, wy - vz"
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten entries gens I
time m2PD = primaryDecomposition I;

-- CORRECT
restart
load "newGTZ.m2"
R = ZZ/32003[x,y,z];
I = ideal"
  x2yz + xy2z + xyz2 + xyz + xy + xz + yz,
  x2y2z + xy2z2 + x2yz + xyz + yz + x + z,
  x2y2z2 + x2y2z + xy2z + xyz + xz + z + 1"
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten entries gens I
time m2PD = primaryDecomposition I;

-- INCORRECT
-- Must the GB really be calculated after going to the fraction field?  This example seems to say yes.
restart
load "newGTZ.m2"
R = ZZ/32003[x,y,z,t]
I = ideal(
    t^10-x,
    t^31-t^6-t-y,
    t^8-z)
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten entries gens I
time m2PD = primaryDecomposition I;

-- UNKNOWN - Runs for a very long time on built in version, as well as the 'decompose' version.
-- The GeneralPosition one does indeed run a lot faster though
restart
load "newGTZ.m2"
R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l]
I = ideal "-2hjk + 4ef + bj + ak,
           -2hjl + 4eg + cj + al,
           -4fhj - 4ehk - djk + 2be + 2af,
           -4ghj - 4ehl - djl + 2ce + 2ag,
           -2dfj - 2dek + ab,
           -2dgj - 2del + ac"
-- should try on laptop and let run for a while
-- gets much closer with GP algorithm
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity => 2);
I = ideal flatten entries gens I
time m2PD = primaryDecomposition I;
///

-- CORRECT
-- same error as in the cyclic permutation example in 5 variables 
-- ideals are going from zero dimensional to non-zero dimensional after a change of coordinates.
-- I shouldn't have to compute independent sets again after changing coordinates, should I?
TEST ///
restart
load "newGTZ.m2"
R = ZZ/32003[x,y,z,t]
I = ideal(
   y^2*z+2*x*y*t-2*x-z,
   -x^3*z+4*x*y^2*z+4*x^2*y*t+2*y^3*t+4*x^2-10*y^2+4*x*z-10*y*t+2,
   2*y*z*t+x*t^2-x-2*z,
   -x*z^3+4*y*z^2*t+4*x*z*t^2+2*y*t^3+4*x*z+4*z^2-10*y*t-10*t^2+2)
time ourPD = newPD(I,Verbosity=>2);
I = ideal flatten gens I
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
///

end

restart
loadPackage "newGTZ"
installPackage "newGTZ"
viewHelp newGTZ
path = prepend("/Mac2SVN/M2/Macaulay2/packages", path)
load "/Mac2SVN/M2/Macaulay2/packages/ExampleIdeals.m2"
R = ZZ/32003[vars(0..8)];
I = permanents(2, genericMatrix(R,3,3))
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity=>2);

restart
path = prepend("/Mac2SVN/M2/Macaulay2/packages", path)
loadPackage "ExampleIdeals"
loadPackage "newGTZ"
I = bayes({{},{1},{1},{2},{3,4}}, (2,2,2,2,2))
I = bayes({{},{},{1},{1,2},{3}}, (2,2,2,2,2))
I = bayes({{}, {1}, {2}, {2}, {2}},(2,2,2,2,2))
I = bayes({{},{1},{2},{3}}, (2,2,2,2))
I = bayes({{},{1},{1},{2,3}}, (2,2,2,2))
I = bayes({{},{1},{1,2},{2,3}}, (2,2,2,2))
I = bayes({{},{1},{2},{2,3}}, (2,2,2,2))
time ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
I = ideal flatten entries gens I
time ourPD = newPD(I,Verbosity=>2);

time primaryDecomposition(ideal I_*)
