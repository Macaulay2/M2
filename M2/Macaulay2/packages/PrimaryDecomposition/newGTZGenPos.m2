getCoordChange = method()
getCoordChange(Ring) := (R) ->
(
   randList := toList apply(1..(numgens R-1),i -> random(ultimate(coefficientRing,R)));
   coeffs := substitute(matrix{randList},R);
   lastVar := last gens R;
   otherVars := take(gens R, numgens R - 1);
   coordChange := matrix{append(otherVars, lastVar + coeffs*(transpose matrix {otherVars}))};
   coordChangeInverse := matrix{append(otherVars, lastVar - coeffs*(transpose matrix {otherVars}))};
   (map(R,R,coordChange),map(R,R,coordChangeInverse))
)
TEST ///
restart
--load "newGTZGenPos.m2"
R = ZZ/32003[a,b,c,x,MonomialOrder => Lex]
(phi,phiInverse) = getCoordChange(R)
phi*phiInverse
phiInverse*phi
///

saturateFiberVars = method()
saturateFiberVars(Ideal, List) := (I, myVars) ->
(
   mySep := first getSeparator(I,myVars);
   saturate(I,mySep)
)

invertVariables = method(Options => {MonomialOrder => null})
invertVariables(List,List,Ring) := opts -> (fiberVars, baseVars, R) ->
(
   local newR;
   if (fiberVars == {}) then
   (
      if (opts.MonomialOrder === null)
         then newR = R
         else newR = newRing(R, MonomialOrder => opts.MonomialOrder);
   )
   else
   (
      if (opts.MonomialOrder === null)
         then newR = frac((coefficientRing R)[fiberVars])[baseVars]
         else newR = frac((coefficientRing R)[fiberVars])[baseVars,MonomialOrder => opts.MonomialOrder];
   );
   mapRtoNewR := map(newR,R);
   mapRtoNewR
)

clearDenominators = method()
clearDenominators(Ring, Ideal) := (R,I) ->
(
   fracR := frac R;
   gensI := flatten entries gens I;
   myMap := map(fracR, ring I);
   numerators := apply(gensI, i -> numerator myMap(i));
   ideal numerators
)

debtimes = 0;
zeroDimGenPos = method(Options => {Verbosity => 0,ChangeCoords => false})
zeroDimGenPos(Ideal, Ideal) := opts -> (I, resultSoFar) ->
(
   local compList;

   debtimes = debtimes + 1;
   if debtimes == 2 then gbTrace=3;   
{*
   I1 = I;
   I2 = trim I1;

   if debtimes == 2 then error "look at this!";
   << " about to test equality of ideals" << endl;
   if I1 != I2 then error "flummox me";
   I = I2;
*}

   << " about to eliminate" << endl << flush;
   prevI = I;
   I = ideal I_*;
   oldI = I;
   I = trim I;
--   if (oldI != I) then error "Trim changed the input ideal!";

   -- independentSets oldI: {1}
   -- independentSets I   : {d}
   -- doing the trim creates a nontrivial independent set!

   myNewVars := gens ring I;
   n := #myNewVars - 1;
   
   -- both these lines, and the lines following are the hold up in computations (usually)
   gbI := first entries gens gb I;
   f := first select(gbI, i -> support(i) == {myNewVars_n});
   --elimI := trim eliminate(I, take(myNewVars,n));
   --f := first flatten entries gens elimI;

   << " done with gb" << endl << flush;

   factorList := newFactor f;
   -- only look at those that matter to the decomposition
   factorList = select(factorList, (irr,pow) -> not isSubset(resultSoFar, I + ideal (irr^pow)));
   -- find out if they are in general position and if so, get the g's from the DGP paper
   genPosList := apply(factorList, (irr,pow) -> isInGeneralPosition(I,irr,pow));
   if (genPosList != {}) then
   (
      isInGenPos := fold(apply(genPosList, i -> first i), (i,j) -> i and j);
      if (not isInGenPos and not opts.ChangeCoords) then error "Not in general position";
      if (isInGenPos)
         then
	 (
	    compList = apply(genPosList, (isGenPos,hs,irr,pow) -> (((ideal (irr)^(pow)) + I), (ideal apply(hs, i -> first i))));
	    if (not fold(compList / (i -> isSubset(I,first i)), (j,k) -> j and k)) then error "Unknown error has occurred.";
	 )
         else
         (
	    -- make a random change of coordinates
            (phi,phiInverse) := getCoordChange(ring I);
	    compList = zeroDimGenPos(phi(I),phi(resultSoFar),opts);
	    -- bring it back with phiInverse
	    compList = apply(compList, (Q,P) -> (phiInverse(Q),phiInverse(P)));
         );
   )
   else compList = {};
   compList 
)
TEST ///
-- SERIOUS TRIM BUG (the below is a reproduction of the situation, though it does not reproduce the bug...)
restart
load "newGTZ.m2"
R = frac (ZZ/32003[d])[c,h,a,b]
I = ideal (c-14965*h-8141*a-2438*b-2438*d,h^2+679*h*a-7029*h*b-7029*d*h-4751*a^2-2122*a*b-2122*d*a+6553*b^2+13106*d*b+6553*d^2,h*a^2-d^2*h-15662*a^3+12487*a^2*b+12487*d*a^2+15662*d^2*a-12487*d^2*b-12487*d^3,h*a+10668*d*h+(11913/(d^2))*a^4+4428*a^2+12487*a*b-14069*d*a+14830*d*b+14830*d^2,h*a*b^2+2*d*h*a*b+3186*d^2*h*a+12958*h*b^3+6871*d*h*b^2+8082*d^2*h*b-1556*d^3*h-7768*a^4-3699*a^3*b-3699*d*a^3-7831*a^2*b^2-15662*d*a^2*b-7532*d^2*a^2+12487*a*b^3+5458*d*a*b^2+7921*d^2*a*b+4812*d^3*a+15535*b^4-1866*d*b^3+13542*d^2*b^2+11146*d^3*b+12206*d^4)
J = I + ideal(-14851*b+d)^2
K = trim J
J == K
isSubset(I,J)
isSubset(I,K)
///

PDZDF = method(Options => {Verbosity => 0, ChangeCoords => false})
PDZDF(Ideal, List, Ideal) := opts -> (I, variables, resultSoFar) ->
(
   local retVal;
   --- This function implements the zero dimensional primary decomposition in the case
   --- that the ideal is in general position.
   --- There is now an option that if the ideal is not in general position, the code
   --- tries to make a change, and continues.

   --put the ideal in the ring with the independent variables inverted.
   -- CHANGE: here, fiber -> base, but needs to be done elsewhere as well
   
   --baseVars := support first independentSets I; 
   baseVars := variables;
   
   myVars := toList(set gens ring I - set baseVars);   
   mapRtoNewR := invertVariables(baseVars, myVars, ring I, MonomialOrder => Lex);
   newI := mapRtoNewR(I);
   newResultSoFar := mapRtoNewR(resultSoFar);

   -- now call zeroDimGenPos on the ideal.  If necessary, further calls are made to this function.
   compList := zeroDimGenPos(newI, newResultSoFar, opts);
   
   -- now we need to put the decomposition back over the original ring, and saturate away the baseVars
   -- doing so is a bit tricky.  You cannot just use a substitute.  You have to clear the denomimators yourself.
   compList = apply(compList, (Q,P) -> (trim saturateFiberVars(clearDenominators(ring I,Q),baseVars),trim clearDenominators(ring I,P)));
   compList
)

newFactor = method()
newFactor(RingElement) := (g) -> (
   -- g is in a polynomial ring of the form frac(kk[fiberVars])[baseVars].
   -- this function is an attempt to factor the numerator of g.
   baseVars := gens ring g;
   fiberVars := gens coefficientRing ring g;
   newF := frac(ultimate(coefficientRing, ring g)[fiberVars | baseVars]);
   h := substitute(g, newF);
   factorList := apply(toList factor numerator h, toSequence);
   factorList = apply(factorList, (irr,pow) -> (substitute(irr,ring g),pow));
   select(factorList, (irr,pow) -> not isSubset(set support irr, set fiberVars))
)

isInGeneralPosition = method()
isInGeneralPosition(Ideal,RingElement,ZZ) := (I,f,pow) ->
(
   --- This function checks to see if the zero dimensional ideal I is in general position.
   g := f^pow;
   J := ideal g + I;
   S := flatten entries gens gb J;
   -- make g monic;
   g = g // (leadCoefficient g);
   gPos := position(S, s -> (s == g));
   loopVar := not(gPos === null);
   if (loopVar) then S = drop(S,{gPos,gPos});
   myVars := gens ring I;
   i := #myVars - 2;
   n := #myVars - 1;
   hs := {(f,pow)};
   while (i >= 0 and loopVar) do
   (
      local nextHInfo;
      nextH := position(S, s -> (nextHInfo = isLinearPower(s,hs,myVars_i,myVars_n); nextHInfo_0));
      if (nextH === null)
         then loopVar = false
         else (hs = append(hs, (nextHInfo_1,nextHInfo_2));
	       S = drop(S,{nextH,nextH});
	       i = i - 1;);
   );
   (loopVar, hs, f, pow)
)
TEST ///
-- This seems to be working as desired...
restart
load "newGTZ.m2"
R = frac (ZZ/32003[d])[c,h,a,b]
I = ideal (c-14965*h-8141*a-2438*b-2438*d,h^2+679*h*a-7029*h*b-7029*d*h-4751*a^2-2122*a*b-2122*d*a+6553*b^2+13106*d*b+6553*d^2,h*a^2-d^2*h-15662*a^3+12487*a^2*b+12487*d*a^2+15662*d^2*a-12487*d^2*b-12487*d^3,h*a+10668*d*h+(11913/(d^2))*a^4+4428*a^2+12487*a*b-14069*d*a+14830*d*b+14830*d^2,h*a*b^2+2*d*h*a*b+3186*d^2*h*a+12958*h*b^3+6871*d*h*b^2+8082*d^2*h*b-1556*d^3*h-7768*a^4-3699*a^3*b-3699*d*a^3-7831*a^2*b^2-15662*d*a^2*b-7532*d^2*a^2+12487*a*b^3+5458*d*a*b^2+7921*d^2*a*b+4812*d^3*a+15535*b^4-1866*d*b^3+13542*d^2*b^2+11146*d^3*b+12206*d^4)
isInGeneralPosition(I, -14581*b+d,2)
///

isLinearPower = method()
isLinearPower(RingElement,List,RingElement,RingElement) := (s,hs,xi,xn) ->
(
   R := ring s;
   tempR := R/(ideal apply(hs,(i,j) -> i));
   newR := newRing(tempR, MonomialOrder => Lex);
   retVal := (false,0_R,0);
   news := substitute(s, newR);
   myMap := map(newR, ring xi);
   newVars := {myMap(xi),myMap(xn)};
   newsSupport := support news;
   if ((newsSupport == {myMap(xi)} or newsSupport == newVars) and news != 0_(ring news)) then
   (
      (newH, linPow) := getLinearPower(news, myMap(xi));
      if (newH == 0_(ring news)) then retVal = (false, 0_R, 0) else retVal = (true, substitute(newH,R), linPow);
   );
   retVal
)

getLinearPower = method()
getLinearPower(RingElement,RingElement) := (s,xi) ->
(
   --- make s monic
   news := s // leadCoefficient s;
   n := degree(xi,leadTerm s);
   topTerms := select(terms news, f -> degree(xi,f) >= n);
   secondTerms := select(terms news, f -> degree(xi,f) == n-1);
   retVal := (0_(ring s),0);
   if (#topTerms == 1 and secondTerms != {}) then
   (
      insideTerm := sum apply(secondTerms, f -> f // (-n*xi^(n-1)));
      testElt := (xi - insideTerm)^n;
      if (testElt == s) then retVal = (xi - insideTerm, n);
   )
   else if (topTerms == terms news) then retVal = (xi,n);
   retVal
)
