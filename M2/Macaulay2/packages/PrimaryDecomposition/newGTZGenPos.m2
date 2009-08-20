-- Input  : A (polynomial) ring
-- Output : A pair of endomorphisms of the ring.  Say R = kk[x_1,...,x_n].  Then
--          each map sends x_i to x_i for i < n, but the first map sends
--          x_n ---> x_n + (random linear form in x_1,...,x_{n-1}).  The inverse
--          of course sends x_n ---> x_n - (random linear form above).
--          The map also fixes those elements of us, and the coordinate change does not involve the elements of us
--          us = baseVars
getCoordChange = method()
getCoordChange(Ring,List) := (R,us) ->
(
   randTerms := terms random(1,R);
   okVars := toList(set gens R - set us);
   lastVar := last okVars;
   myTerms := select(randTerms, i -> isSubset(support i, okVars));
   linForm := sum myTerms;
   coordChange := map(R, R, {lastVar => linForm});
   coordChangeInverse := invertLinearRingMap(coordChange);
   (coordChange,coordChangeInverse,lastVar)
)
TEST ///
restart
load "newGTZ.m2"
debug newGTZ
R = ZZ/32003[a,b,c,x]
(phi,phiInverse) = getCoordChange(R,{x})
phi*phiInverse
phiInverse*phi
///

-- Input  : An ideal I, and a list of variables myVars.  The list myVars is supposed to be
--          the set of variables that were inverted because they were an independent set
--          earlier in the algorithm (and hence of the input)
-- Output : 
contractBack = method()
contractBack(Ring,Ideal) := (R,I) ->
(
   fracR := frac R;
   gensI := flatten entries gens I;
   myMapFracR := map(fracR, ring I);
   myMapR := map(R, ring I);
   numerators := apply(gensI, i -> numerator myMapFracR(i));
   newI := ideal numerators;
   myVars := (flatten entries vars coefficientRing ring I) / myMapR;
   sepAndSat(newI,myVars)
)

sepAndSat = method()
sepAndSat(Ideal,List) := (I,myVars) ->
(
   mySep := first getSeparator(I, myVars);
   trim saturate(I,mySep)
)

-- Input  : 
-- Output : 
invertVariables = method(Options => {MonomialOrder => null})
invertVariables(List,List,Ring) := opts -> (baseVars, fiberVars, R) ->
(
   local newR;
   if (baseVars == {}) then
   (
      if (opts.MonomialOrder === null)
         then newR = R
         else newR = newRing(R, MonomialOrder => opts.MonomialOrder);
   )
   else
   (
      if (opts.MonomialOrder === null)
         then newR = frac((coefficientRing R)[baseVars])[fiberVars]
         else newR = frac((coefficientRing R)[baseVars])[fiberVars,MonomialOrder => opts.MonomialOrder];
   );
   mapRtoNewR := map(newR,R);
   mapRtoNewR
)

-- Input  : I, a zero dimensional ideal in kk[x_1,...,x_n] (note that kk could have variables too)
-- Output : The 'minimal polynomial' of x_n modulo I.
getMinimalPolynomial = method()
getMinimalPolynomial(Ideal) := (I) ->
(
   -- both these lines, and the lines following are the hold up in computations (usually)
   --newR = newRing(ring I, MonomialOrder => Lex)
   --gbI := first entries gens gb I;
   --f := first select(gbI, i -> support(i) == {last gens ring I});
   elimI := trim eliminate(I, take(gens ring I,(#gens ring I)-1));
   first flatten entries gens elimI
)

-- Input  : 
--          
-- Output : 
getMinimalPolynomial2 = method()
getMinimalPolynomial2(Ideal,List,RingElement,RingElement) := (I,us,x,y) ->
(
   J := substitute(I,{x => y});
   elimVars := toList(set gens ring I - set us - set {x});
   elimJ := eliminate(J, elimVars);
   --error "err";
   --if (numgens elimJ == 0 and isSubset(ideal elimVars,J)) then y
   if numgens elimJ != 1 then error "Uh oh."
   else elimJ_0
)
TEST ///
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
independentSets I
mySep = first getSeparator(I,{c})
Isat = saturate(I,mySep)
getMinimalPolynomial2(Isat,{c},d,d)
phi = (getCoordChange(R))_0
getMinimalPolynomial2(Isat,{c},h,phi(h))
///

-- Input  :
-- Output :
invertLinearRingMap = method()
invertLinearRingMap(RingMap) := (f) ->
(
   R := target f;
   map(R,R,(vars R)*(jacobian f.matrix)^(-1))
)
TEST ///
restart
load "newGTZ.m2"
debug newGTZ
R = ZZ/32003[a,b,c]
f = map(R,R,{c => c+a+b})
g = invertLinearRingMap(f)
f*g
g*f
///

-- Input  : An ideal I that is zero dimensional over k(us)[xs\us] and saturated with respect to us.  The change of coordinates x => y is used.
-- Output : A list of ideals of the form (I,phiInverse(h_i)) where the h_i are powers of irred polys
--          such that prod {h_i} is the minimal polynomial of y over I after a change of coords in last variable
splitZeroDimensionalIdeal = method()
splitZeroDimensionalIdeal(Ideal,List,RingElement,RingElement) := (I, us, x, y) ->
(
   mySep := first getSeparator(I,us);
   Isat := saturate(I,mySep);
   myMap := map(ring I, ring I, {x => y});
   -- using this since myMap^(-1) is not implemented yet
   myMapInverse := invertLinearRingMap(myMap);
   factorList := apply(toList factor getMinimalPolynomial2(Isat, us, x, y), toList);
   factorList = select(factorList, fac -> first degree fac#0 > 0);
   << netList factorList << endl;
   idealList := apply(factorList, fac -> ideal (myMapInverse fac#0)^(fac#1) + I);
   idealList = apply(idealList, J -> time sepAndSat(J,us));
   apply(idealList, J -> trim ideal gens gb J)
)
splitZeroDimensionalIdeal(Ideal,List) := (I,us) ->
(
   (phi,phiInverse,lastVar) := getCoordChange(ring I,us);
   splitZeroDimensionalIdeal(I, us, lastVar, phi(lastVar))
)
TEST ///
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
independentSets I
mySep = first getSeparator(I,{c})
Isat = saturate(I,mySep)
time splitZeroDimensionalIdeal(Isat,{c},h,a+7*b+13*d+5*e+9*h)
///

-- Input  : A list xs, and a function f on the elements of xs.  The return value of f should be a tuple,
--          the first coordinate of which is a boolean.
-- Output : The result of apply(xs,f) until a BooleanValue (in options) is achieved as output from f
applyUntil = method(Options => {BooleanValue => false})
applyUntil(List, Function) := opts -> (xs, f) ->
(
   retVal := for i from 0 to #xs-1 list
             (
	        temp := f(xs#i);
		if (temp#0 == opts.BooleanValue) then i = #xs;
		temp
             );
   retVal
)

-- Input  : A zero dimensional ideal I, and a resultSoFar.
-- Output : 
primDecZeroDimField = method(Options => {Verbosity => 0})
primDecZeroDimField(Ideal, List, Ideal) := opts -> (I, variables, resultSoFar) ->
(
   splitTime := timing (compList := splitZeroDimensionalIdeal(I, variables));

   if (opts.Verbosity > 0) then << "Splitting time : " << splitTime#0 << endl;

   -- problem: compList's ideals are not in general position at this point.  I think
   -- one needs to leave them with coords changed, and then change them back.
   --genPosList := applyUntil(compList, J -> isInGeneralPosition(J,variables));
   genPosList := applyUntil(compList, J -> isPrimaryZeroDim(J));
   if (genPosList != {}) then
   (
      isInGenPos := fold(genPosList / first, (i,j) -> i and j);
      if (not isInGenPos) then (
	 error "Uhoh.";
         -- try again
	 compList = primDecZeroDimField(I, variables, resultSoFar, opts);
      );
   )
   else compList = {};
   compList
)

-- Input  : A RingElement g in a polynomial ring of the form frac(kk[baseVars])[fiberVars].
-- Output : A factorization of the numerator of g.  WARNING: Units in frac(kk[baseVars]) are not returned as factors.
newFactor = method()
newFactor(RingElement) := (g) -> (
   -- g is in a polynomial ring of the form frac(kk[baseVars])[fiberVars].
   -- this function is an attempt to factor the numerator of g.
   fiberVars := gens ring g;
   baseVars := gens coefficientRing ring g;
   newF := frac(ultimate(coefficientRing, ring g)[baseVars | fiberVars]);
   h := substitute(g, newF);
   factorList := apply(toList factor numerator h, toSequence);
   factorList = apply(factorList, (irr,pow) -> (substitute(irr,ring g),pow));
   select(factorList, (irr,pow) -> not isSubset(set support irr, set baseVars))
)


TEST ///
restart
load "newGTZ.m2"
debug newGTZ
R = ZZ/32003[a,b,c,d,e,f,g,h,i]
I = ideal(i,h,g,f,c,b*d+a*e)
isPrimaryZeroDim(I)
///

TEST ///
restart
load "newGTZ.m2"
debug newGTZ
R = ZZ/32003[a,b,c,d,e,h]
idealList = {ideal(e-h,d-h,c-h,a+b+3*h,b^2+3*b*h+h^2),ideal(e-h,c+d+3*h,b-h,a-h,d^2+3*d*h+h^2),ideal(e-h,d-h,b+c+3*h,a-h,c^2+3*c*h+h^2),ideal(e-h,a+b+c+d+h,d^2-c*h,c*d-b*h,b*d+b*h+c*h+d*h+h^2,c^2+b*h+c*h+d*h+h^2,b*c-h^2,b^2-d*h)}
idealList / (I -> time isPrimaryZeroDim I)

restart
load "newGTZ.m2"
debug newGTZ
R = ZZ/32003[a,b,c,d,h]
I = ideal(c-d,a+b+2*d,d^2+h^2,b^2+2*b*d-h^2)
isPrimaryZeroDim I

restart
load "newGTZ.m2"
debug newGTZ
R = QQ[a,b,c]
I = ideal apply(1 .. 3, i -> random(3,R))
ourPD3 = newPD(I,Verbosity=>2,Strategy=>{GeneralPosition});
///

isPrimaryZeroDim = method()
isPrimaryZeroDim(Ideal) := (I) ->
(
   R := ring I;
   variables := support first independentSets I;
   (phi,phiInverse,myVar) := getCoordChange(ring I,variables);
   fiberVars := toList (set gens R - set variables);
   
   -- trying to do lex product order here to simulate the fraction field.
   --newR := (coefficientRing R) monoid([fiberVars,variables, MonomialOrder=>{Lex=>#fiberVars,Lex=>#variables}]);
   
   newR := newRing(R, MonomialOrder => Lex);
   psi := map(newR,R);
   J := psi(phi(I));
   fiberVars = fiberVars / psi;
   
   S := flatten entries gens gb J;
   fiberVars = toList (set gens ring J - set (variables / psi));

   (areLinearPowers(S,fiberVars),0)
)

areLinearPowers = method()
areLinearPowers(List,List) := (S, fiberVars) ->
(
   R := ring first S;
   baseVars := toList (set gens R - set fiberVars);

   -- first h should have only a single non-base-variable in its support (and should also be irreducible)
   -- we should pick the element that can be written as the highest degree irreducible.
   potentialHs := select(S, s -> #(set support s - set baseVars) == 1);
   firstHPos := maxPosition(potentialHs / irredPower);
   firstHPos = position(S, s -> s == potentialHs#firstHPos);
   firstH := first first apply(toList factor S#firstHPos, toList);

   phi := invertVariables(baseVars,fiberVars,R,MonomialOrder => Lex);
   newS := S / phi;
   hs := {};

   fiberVars = fiberVars / phi;
   
   if (firstHPos =!= null) then 
   (
      hs = {phi(firstH)};
      varPos = position(fiberVars, x -> (x === first support first hs));
      fiberVars = drop(fiberVars, {varPos,varPos});
      newS = drop(newS,{firstHPos,firstHPos});
      i := 0;
      loopVar := true;
      while (fiberVars =!= {} and loopVar) do
      (
         local newHInfo;
	 findNewH := position(newS, s -> (newHInfo = isLinearPower(s,fiberVars#i,hs); newHInfo#0));
	 if (findNewH === null) then i = i + 1
         else
         (
            hs = append(hs, newHInfo#1);
            newS = drop(newS,{findNewH,findNewH});
	    fiberVars = drop(fiberVars, {i,i});
	    i = 0;
         );
         if (i == #fiberVars and i > 0) then loopVar = false;
      );
   );
   loopVar
)

irredPower = method()
irredPower(RingElement) := (f) ->
(
   factorList := apply(toList factor f, toList);
   max(factorList / first / degree / first)
)

isLinearPower = method()
isLinearPower(RingElement,RingElement,List) := (s,xi,hs) ->
(
   R := ring s;
   usedVars := flatten (hs / support);
   n := degree(xi, s);
   newR := R/(substitute(ideal hs,R));
   myMap := map(R, newR);
   myMap2 := map(newR, R);
   usedVars = usedVars / myMap2;
   news := myMap2 s;
   xi = myMap2 xi;
   retVal := (false,0_R);
   sCoeff = coefficients news;
   coeffIndex := position((flatten entries first sCoeff) / support, xs -> xs === {xi});
   if (n > 0 and coeffIndex =!= null) then
   (
      --- make s monic in the xi variable
      news = news // ((flatten entries last sCoeff)#coeffIndex);
      -- is linear power?
      firstTerm := sum select(terms news, f -> degree(xi,f) == n);
      secondTerm := sum select(terms news, f -> degree(xi,f) == n-1);
      if (secondTerm != 0_(ring s)) then 
      (
         insideTerm := secondTerm // (n*xi^(n-1));
	 newH := xi + insideTerm;
         testElt := newH^n;
	 if (isSubset(support insideTerm, usedVars) and testElt == news) then retVal = (true,myMap newH);
      )
      else if (firstTerm == news) then retVal = (true,myMap xi);
   );
   retVal
)
