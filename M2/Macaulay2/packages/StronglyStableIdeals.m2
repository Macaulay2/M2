newPackage("StronglyStableIdeals",
           Version => "1.1",
	   Date => "June 2018",
           Authors => {
	               {Name => "Davide Alberelli", Email => "davide.alberelli@gmail.com"},
	               {Name => "Paolo Lella", Email => "paolo.lella@polimi.it", HomePage => "http://www.paololella.it/"}
    	              },
	   Headline => "studying strongly stable ideals related to Hilbert polynomials",
	   Keywords => {"Commutative Algebra"},
	   PackageImports => {"gfanInterface","Truncations"},
	   Certification => {
		"journal name" => "The Journal of Software for Algebra and Geometry",
		"journal URI" => "http://j-sag.org/",
		"article title" => "Strongly stable ideals and Hilbert polynomials",
		"acceptance date" => "4 November 2018",
		"published article URI" => "https://msp.org/jsag/2019/9-1/p01.xhtml",
         	"published article DOI" => "10.2140/jsag.2019.9.1",
		"published code URI" => "https://msp.org/jsag/2019/9-1/jsag-v9-n1-x01-StronglyStableIdeals.m2",
     	        "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/StronglyStableIdeals.m2",
		"release at publication" => "8c301dd0cdfb015d06f7967d12a7592a6c2e07b9",	    -- git commit number in hex
		"version at publication" => "1.1",
		"volume number" => "9",
		"volume URI" => "https://msp.org/jsag/2018/9-1/"
		}
	  )

-- For information see documentation key "StronglyStableIdeals" below.

export {
      -- methods
      "isHilbertPolynomial",
      "gotzmannDecomposition",
      "macaulayDecomposition",
      "gotzmannNumber",
      "lexIdeal",
      "stronglyStableIdeals",
      "isGenSegment",
      "isHilbSegment",
      "isRegSegment",
      -- Options
      "MaxRegularity",
      "OrderVariables"
      }
     
---------------------------------------------------------------------------------
--          PROJECTIVE HILBERT POLYNOMIALS AND GOTZMANN DECOMPOSITION          --
---------------------------------------------------------------------------------

--------
-- isHilbertPolynomial
--------
isHilbertPolynomial = method(TypicalValue => Boolean)

isHilbertPolynomial ProjectiveHilbertPolynomial := hp -> (
   currentHP := hp;
   k := 0;
   while degree currentHP > 0 do 
   (
      b := dim currentHP;
      if b < 0 then return false;
      currentHP = currentHP - projectiveHilbertPolynomial(b,-k);
      k = k+1;
   );
   if degree currentHP != 0 then false else true
) -- END isHilbertPolynomial ProjectiveHilbertPolynomial

isHilbertPolynomial RingElement := p -> (
   R := ring p;
   if not isPolynomialRing R then return false;
   if numgens R != 1 then return false;
   if coefficientRing R =!= ZZ and coefficientRing R =!= QQ then return false;
   
   currentP := p;
   k := 0;
   while leadCoefficient currentP > 0 do 
   (
      b := first degree currentP;
      currentP = currentP - polynomialBinom(b-k,b,R);
      k = k+1;
   );
   if currentP != 0_R then false else true
) -- END isHilbertPolynomial RingElement


--------
-- gotzmannDecomposition
--------
gotzmannDecomposition = method(TypicalValue => List)

gotzmannDecomposition ProjectiveHilbertPolynomial := hp -> (
   currentHP := hp;
   decomposition := {};
   k := 0;
   while degree currentHP > 0 do 
   (
      b := dim currentHP;
      if b < 0 then error "argument 1: expected a Hilbert polynomial";
      decomposition = decomposition | {projectiveHilbertPolynomial(b,-k)};
      currentHP = currentHP - projectiveHilbertPolynomial(b,-k);
      k = k+1;
   );
   if degree currentHP != 0 then 
      error "argument 1: expected a Hilbert polynomial"
   else 
      decomposition
) -- END gotzmannDecomposition ProjectiveHilbertPolynomial

gotzmannDecomposition RingElement := p -> (
   R := ring p;
   if not isPolynomialRing R then error "argument 1: expected a polynomial";
   if numgens R != 1 then error "argument 1: expected a univariate polynomial";
   if coefficientRing R =!= ZZ and coefficientRing R =!= QQ then error "argument 1: expected a numerical polynomial";
   
   currentP := p;
   decomposition := {};
   k := 0;
   while leadCoefficient currentP > 0 do 
   (
      b := first degree currentP;
      decomposition = decomposition | {projectiveHilbertPolynomial(b,-k)};
      currentP = currentP - polynomialBinom(b-k,b,R);
      k = k+1;
   );
   if currentP != 0_R then 
      error "argument 1: expected a Hilbert polynomial"
   else 
      decomposition
) -- END gotzmannDecomposition RingElement


--------
-- macaulayDecomposition
--------
macaulayDecomposition = method(TypicalValue => List)

macaulayDecomposition ProjectiveHilbertPolynomial := hp -> (
   lexExp := lexIdealExponents hp; 
   d := dim hp; 
   macaulayCoeff := {sum lexExp};
   for i from 0 to d-1 do macaulayCoeff = macaulayCoeff | {macaulayCoeff#-1 - lexExp#i};
   flatten for i from 0 to d list {projectiveHilbertPolynomial(i+1,-1),-projectiveHilbertPolynomial(i+1,-1-macaulayCoeff#i)}
) -- END macaulayDecomposition ProjectiveHilbertPolynomial

macaulayDecomposition RingElement := p -> (
   lexExp := lexIdealExponents p; 
   d := first degree p; 
   macaulayCoeff := {sum lexExp};
   for i from 0 to d-1 do macaulayCoeff = macaulayCoeff | {macaulayCoeff#-1 - lexExp#i};
   flatten for i from 0 to d list {projectiveHilbertPolynomial(i+1,-1),-projectiveHilbertPolynomial(i+1,-1-macaulayCoeff#i)}
) -- END macaulayDecomposition RingElement


--------
-- gotzmannNumber
--------
gotzmannNumber = method(TypicalValue => ZZ)

gotzmannNumber ProjectiveHilbertPolynomial := hp -> #gotzmannDecomposition hp

gotzmannNumber RingElement := p -> #gotzmannDecomposition p


--------
-- projectiveHilbertPolynomial
--------
projectiveHilbertPolynomial RingElement := p -> sum gotzmannDecomposition p



-----------------------------------------------------------------
--          STRONGLY STABLE IDEALS AND SEGMENT IDEALS          --
-----------------------------------------------------------------

--------
-- lexIdeal
--------
lexIdeal = method(TypicalValue => Ideal,Options => {CoefficientRing=>QQ,OrderVariables=>Down})

lexIdeal (ProjectiveHilbertPolynomial,PolynomialRing) := opts -> (hp,R) -> (
   if hp == hilbertPolynomial R then return ideal(0_R);
   lexExp := lexIdealExponents hp; 
   d := dim hp; 
   n := numgens R;
   if n < d+2 then error ("argument 2: expected at least " | toString(d+2) | " variables");
   lexExp = lexExp | for i from 0 to n-d-3 list 0;
   gensLex := {};
   for i from 0 to n-2 do
   (
      T := R_(n-2-i)^(lexExp#i);
      if i > 0 then T = T*R_(n-2-i);

      for j from i+1 to n-2 do T = T*R_(n-2-j)^(lexExp#j);	 
      gensLex = gensLex | {T};
   );
   trim ideal gensLex
) -- END lexIdeal (ProjectiveHilbertPolynomial,PolynomialRing)

lexIdeal (RingElement,PolynomialRing) := opts -> (p,R) -> lexIdeal(projectiveHilbertPolynomial p,R)

lexIdeal (ProjectiveHilbertPolynomial,ZZ) := opts -> (hp,n) -> (
   if n < 2 then error "argument 2: expected at least 2 variables";
   if not instance(opts.CoefficientRing,Ring) then error "option CoefficientRing: expected a ring";    
   if opts.OrderVariables != Up and opts.OrderVariables != Down then error "option OrderVariables: expected Down or Up";
   
   R := (opts.CoefficientRing)(monoid [VariableBaseName=>getSymbol "x",Variables=>n]);
   if opts.OrderVariables == Up then R = (opts.CoefficientRing)(monoid [sort gens R]);
   lexIdeal(hp, R)
) -- END lexIdeal (ProjectiveHilbertPolynomial,ZZ)

lexIdeal (RingElement,ZZ) := opts -> (p,n) -> lexIdeal(projectiveHilbertPolynomial p,n,opts)

lexIdeal (ZZ,PolynomialRing) := opts -> (d,R) -> lexIdeal(projectiveHilbertPolynomial d_(QQ[local t]),R)

lexIdeal (ZZ,ZZ) := opts -> (d,n) -> lexIdeal(projectiveHilbertPolynomial d_(QQ[local t]),n,opts)


--------
-- stronglyStableIdeals
--------
stronglyStableIdeals = method(TypicalValue => List,Options => {MaxRegularity=>null,CoefficientRing=>QQ,OrderVariables=>Down})

stronglyStableIdeals (ProjectiveHilbertPolynomial,PolynomialRing) := opts -> (hp,R) -> (
   gN := gotzmannNumber hp;
   d := dim hp;
   n := numgens(R);
   if n < 2 then error "argument 2: expected at least 2 variables";
   if d >= n-1 then return {}; -- there are no schemes of dimension d in the n-dimensional projective space
   r := gN;
   if opts.MaxRegularity =!= null then
   (
       if not instance(opts.MaxRegularity,ZZ) or opts.MaxRegularity <= 0 then error "option MaxRegularity: expected a positive integer";
       r = min(opts.MaxRegularity,r);        
   );   
   hpSeq := for i from 0 to d list (diff(hp,i)) r;
   local growthVec;
   local B;
   if d == n-2 then
   (
      if hpSeq#d > r then return {};
      B = new BorelSet from {NumVariables => 2, 
		             Degree => r,
			     Size => (hpSeq#d),
			     MinimalElements => {R_0^(hpSeq#d)*R_1^(r-hpSeq#d)},
	                     MinimalGenerators => {R_0^(hpSeq#d)},
	                     GrowthVector => toList ((r-hpSeq#d+1 : 0) | (hpSeq#d : 1)),
	                    };	    
      if n == 2 then {ideal(B.MinimalGenerators)} else recursiveCall(B,hpSeq)
   )
   else
   (
      B = new BorelSet from {NumVariables => n-d-1, 
  	                     Degree => r,
			     Size => 0,
			     MinimalElements => {R_(n-d-2)^r},
	                     MinimalGenerators => {1_R},
	                     GrowthVector => toList(r+1 : 0),
	                    };				    
      recursiveCall(B,hpSeq)
   )
) -- END stronglyStableIdeals (ProjectiveHilbertPolynomial,PolynomialRing) 
 
stronglyStableIdeals (ProjectiveHilbertPolynomial,ZZ) := opts -> (hp,n) -> (
   if n < 2 then error "argument 2: expected at least 2 variables";
   if not instance(opts.CoefficientRing,Ring) then error "option CoefficientRing: expected a ring"; 
   if opts.OrderVariables != Up and opts.OrderVariables != Down then error "option OrderVariables: expected Down or Up";
   
   R := (opts.CoefficientRing)(monoid [VariableBaseName=>getSymbol "x",Variables=>n]);
   if opts.OrderVariables == Up then R = (opts.CoefficientRing)(monoid [sort gens R]);
   stronglyStableIdeals(hp, R, MaxRegularity=>opts.MaxRegularity)  
) -- END stronglyStableIdeals (ProjectiveHilbertPolynomial,ZZ)

stronglyStableIdeals (RingElement,PolynomialRing) := opts -> (p,R) -> stronglyStableIdeals(projectiveHilbertPolynomial p, R, opts)

stronglyStableIdeals (RingElement,ZZ) := opts -> (p,n) -> stronglyStableIdeals(projectiveHilbertPolynomial p, n, opts)

stronglyStableIdeals (ZZ,PolynomialRing) := opts -> (d,R) -> stronglyStableIdeals (projectiveHilbertPolynomial d_(QQ[local t]), R, opts)

stronglyStableIdeals (ZZ,ZZ) := opts -> (d,n) -> stronglyStableIdeals (projectiveHilbertPolynomial d_(QQ[local t]), n, opts)


--------
--  isGenSegment
--------
isGenSegment = method(TypicalValue => Sequence)

isGenSegment MonomialIdeal := J -> (
    if not isBorel J then error "argument 1: expected a strongly stable ideal";
    R := ring J;
    RmodJ := R/J;
    markedTerms := gens R;
    completePolynomials := for i from 0 to numgens R - 1 list if i != numgens R - 1 then R_i+R_(i+1) else R_i + 1;
    for m in rsort J_* do
    (
       markedTerms = append(markedTerms,m);
       completePolynomials = append(completePolynomials, m + sum(for t in flatten entries basis(first degree m,RmodJ) list lift(t,R)));
    );
    GC := gfanGroebnerCone markedPolynomialList {markedTerms, completePolynomials};
    W := flatten entries interiorVector coneFromVData rays GC;
    if isSegmentWeightVector(W,markedTerms,completePolynomials) then (true,W) else (false,null) 
) -- END isGenSegment MonomialIdeal

isGenSegment Ideal := J -> (
   if not isMonomialIdeal J then error "argument 1: expected a monomial ideal";
   isGenSegment monomialIdeal J   
) 


--------
-- isRegSegment
--------
isRegSegment = method(TypicalValue => Sequence)

isRegSegment MonomialIdeal := J -> isGenSegment truncate(regularity J,J)

isRegSegment Ideal := J -> (
   if not isMonomialIdeal J then error "argument 1: expected a monomial ideal";
   isRegSegment monomialIdeal J
)


--------
-- isHilbSegment
--------
isHilbSegment = method(TypicalValue => Sequence)

isHilbSegment MonomialIdeal := J -> isGenSegment truncate(gotzmannNumber hilbertPolynomial J,J)

isHilbSegment Ideal := J -> (
   if not isMonomialIdeal J then error "argument 1: expected a monomial ideal";
   isHilbSegment monomialIdeal J
)



---------------------------------------------------------------
--          AUXILIARY TYPES AND METHODS (unexported)          --
---------------------------------------------------------------

---------------------------------
polynomialBinom = method (TypicalValue => RingElement)

polynomialBinom (ZZ,ZZ,Ring) := (a,b,R) -> (
   var := R_0;
   product(for i from 0 to b-1 list (var+a-i)/(i+1))
)
---------------------------------

---------------------------------
lexIdealExponents = method(TypicalValue => List)

lexIdealExponents ProjectiveHilbertPolynomial := hp -> (
   gotzmannDec := gotzmannDecomposition hp;
   d := dim hp;
   lexExp := new MutableList from for i from 0 to d list 0;
   for s in gotzmannDec do lexExp#(dim s) = lexExp#(dim s)+1;
   toList lexExp 
)

lexIdealExponents RingElement := p -> (  
   gotzmannDec := gotzmannDecomposition p;
   d := first degree p;
   lexExp := new MutableList from for i from 0 to d list 0;
   for s in gotzmannDec do lexExp#(dim s) = lexExp#(dim s)+1;
   toList lexExp 
)
---------------------------------

---------------------------------
BorelSet = new Type of HashTable 

protect NumVariables
protect MinimalElements
protect Size
protect GrowthVector
---------------------------------


---------------------------------
recursiveCall = method (TypicalValue => List)
recursiveCall (BorelSet,List) := (B,hpSeq) -> (
   newB := addNextVariable(B);
   S := ring(B.MinimalElements#0);
   toRemove := hpSeq#(numgens S - newB.NumVariables) - newB.Size;
   if toRemove >= 0 then removeMonomials(newB, toRemove, 1_S, hpSeq) else {}
)
---------------------------------


---------------------------------
removeMonomials = method (TypicalValue=>List)
removeMonomials (BorelSet,ZZ,RingElement,List) := (B,toRemove,lastRemoved,hpSeq) -> (
   S := ring(B.MinimalElements#0);
   lastVar := B.NumVariables-1;
   if toRemove == 0 then 
   (
      if B.NumVariables == numgens(S) then return {ideal(B.MinimalGenerators)} else return recursiveCall(B,hpSeq);
   ) 
   else 
   (
      output := {};
      for m in B.MinimalElements do 
      (
         if m > lastRemoved and degree(S_lastVar,m) > 0 then 
	 (
            newB := removeMinimal(B,m);
            output = output | removeMonomials(newB,toRemove-1,m,hpSeq);
         );
      );
      return output;
   );
) 
---------------------------------


---------------------------------
removeMinimal = method(TypicalValue => BorelSet)
removeMinimal (BorelSet,RingElement) := (B,m) -> (
   S := ring(B.MinimalElements#0);
   newMinimalElements := delete(m, B.MinimalElements);
   n := B.NumVariables;
   newMinimalElements = newMinimalElements | toList(select(apply(select(1..n-1, i -> degree(S_i, m) > 0), j -> (up := (m//S_j)*S_(j-1); if not precBorel(up, newMinimalElements) then return up)), i -> i=!=null));
   h := degree(S_(B.NumVariables-1), m);
   newGrowthVector := new MutableList from B.GrowthVector;
   newGrowthVector#h = newGrowthVector#h + 1;
   minGen := m//(S_(B.NumVariables-1)^h);
   newMinimalGenerators := delete(minGen,B.MinimalGenerators);
   newMinimalGenerators = newMinimalGenerators | for i in max(0, minimum(minGen)) .. B.NumVariables-2 list minGen*S_i;

   new BorelSet from {NumVariables => B.NumVariables, 
	                    Degree => B.Degree,
                              Size => B.Size-1,
                   MinimalElements => rsort(newMinimalElements),
                 MinimalGenerators => sort(newMinimalGenerators),
                      GrowthVector => toList(newGrowthVector)}
) 
---------------------------------


---------------------------------
addNextVariable = method(TypicalValue => BorelSet)
addNextVariable BorelSet := B -> (  
   v := B.NumVariables;
   S := ring(B.MinimalElements#0);
   newMinimalElements := {};
   for monomial in B.MinimalElements do 
   (
      lastDeg := degree(S_(v-1),monomial);
      if lastDeg > 0 then 
      (
         newMinimalElements = append(newMinimalElements, monomial//(S_(v-1)^lastDeg)*(S_v^lastDeg));
      ) 
      else 
      (
         newMinimalElements = append(newMinimalElements, monomial);
      );
   );
   newGrowthVector := {};
   newSize := 0;
   for i from 0 to B.Degree do 
   (
      newGrowthVector = append(newGrowthVector, sum (for j from i to B.Degree list B.GrowthVector#j));
      newSize = newSize + (B.GrowthVector#i)*(i+1);
   );

   new BorelSet from {NumVariables => v+1,
                            Degree => B.Degree,
                              Size => newSize,
                   MinimalElements => newMinimalElements,
                 MinimalGenerators => B.MinimalGenerators,
		      GrowthVector => newGrowthVector}
) 
---------------------------------


---------------------------------
minimum = method(TypicalValue => ZZ)
minimum RingElement := m -> (
   if first degree m == 0 then return 0;
   R := ring m;
   i := numgens R - 1;
   while degree(R_i,m) == 0 do i = i-1;
   i
) 
---------------------------------


---------------------------------
precBorel = method(TypicalValue => Boolean)
precBorel (RingElement,RingElement) := (m,n) -> (
   S := ring m;
   v := numgens S;
   s := 0;
   for i from 0 to v-1 do (
      s = s + degree(S_i,m) - degree(S_i,n);
      if s < 0 then return false;
   );
   true
) 
---------------------------------


---------------------------------
precBorel (RingElement, List) := (m,L) -> (
   l:=#L;
   for i from 0 to l-1 do (
      if precBorel(m, L#i) then return true;
   );
   false 
) 
---------------------------------


---------------------------------
isSegmentWeightVector = method(TypicalValue=>Boolean)
isSegmentWeightVector (List, List, List) := (W,markedTerms,completePolynomials) -> (
    for i from 0 to #markedTerms-1 do 
    (
       for t in terms (completePolynomials#i - markedTerms#i)  do 
       (
	   if  dotProduct(W,flatten exponents(markedTerms#i)) <= dotProduct(W,flatten exponents(t)) then return false;
       );	
    );
    true   
) 
---------------------------------


---------------------------------
dotProduct = method(TypicalValue=>RingElement)
dotProduct (List, List) := (L,M) -> sum for i from 0 to #L-1 list L#i*M#i
---------------------------------

------------------------------------------
-----          ASSERT TESTS          -----
------------------------------------------

-- isHilbertPolynomial
TEST ///
  QQ[t];
  assert (isHilbertPolynomial(4*t));
  assert (not isHilbertPolynomial(4*t-3));
  assert (isHilbertPolynomial(projectiveHilbertPolynomial(3)));
  assert (not isHilbertPolynomial(-projectiveHilbertPolynomial(2)));
///

-- gotzmannDecomposition
TEST ///
  QQ[t];
  p = 3*t+1;
  assert (gotzmannDecomposition p == {projectiveHilbertPolynomial(1,0),
	                              projectiveHilbertPolynomial(1,-1),
				      projectiveHilbertPolynomial(1,-2),
				      projectiveHilbertPolynomial(0,-3)});
  q = 2*projectiveHilbertPolynomial(1,0);
  assert (gotzmannDecomposition q == {projectiveHilbertPolynomial(1,0),
	                              projectiveHilbertPolynomial(1,-1),
		         	      projectiveHilbertPolynomial(0,-2)}); 
///

-- macaulayDecomposition
TEST ///
  QQ[t];
  p = 3*t+1;
  assert (macaulayDecomposition p == {projectiveHilbertPolynomial(1,-1),
	                              -projectiveHilbertPolynomial(1,-1-4),
				      projectiveHilbertPolynomial(2,-1),
				      -projectiveHilbertPolynomial(2,-1-3)});
  q = 2*projectiveHilbertPolynomial(1,0);
  assert (macaulayDecomposition q == {projectiveHilbertPolynomial(1,-1),
	                              -projectiveHilbertPolynomial(1,-1-3),
			              projectiveHilbertPolynomial(2,-1),
		                      -projectiveHilbertPolynomial(2,-1-2)}); 
///

-- gotzmannNumber
TEST ///
  QQ[t];
  p = 3*t+1;
  assert (gotzmannNumber p == 4);
  QQ[x,y,z];
  I = ideal(x^2,x*y,y^4);
  assert (gotzmannNumber(hilbertPolynomial I) == 5);
///

-- projectiveHilbertPolynomial
TEST ///
  QQ[t];
  p = 2*t+2;
  assert(projectiveHilbertPolynomial p == 2*projectiveHilbertPolynomial(1,0));
///

-- lexIdeal
TEST ///
   S = QQ[x_0..x_2];
   L = lexIdeal(10,S);
   assert (L == ideal(x_0,x_1^10));
///

-- stronglyStableIdeals
TEST ///
  QQ[t];
  SSI = stronglyStableIdeals(4*t,4);
  assert(#SSI == 4);
///

-- isGenSegment
TEST ///
  QQ[x,y,z];
  I = ideal(x^2,x*y,y^4);
  assert((isGenSegment I)#0);
///

-- isRegSegment
TEST ///
  QQ[x,y,z];
  I = ideal(x^2,x*y,y^4);
  assert((isRegSegment I)#0);
///

-- isHilbSegment
TEST ///
  QQ[x,y,z];
  I = ideal(x^2,x*y,y^4);
  assert((isHilbSegment I)#0);
///

-------------------------------------------
-----          DOCUMENTATION          -----
-------------------------------------------

beginDocumentation()

doc ///
  Key
    StronglyStableIdeals
  Headline
    Find strongly stable ideals with a given Hilbert polynomial
  Description
    Text
      {\bf Overview:}
      
      Strongly stable ideals are a key tool in commutative algebra and algebraic geometry. These ideals have nice combinatorial properties 
      that make them well suited for both theoretical and computational applications. In the case of polynomial rings with coefficients in 
      a field of characteristic zero, the notion of strongly stable ideals coincides with the notion of Borel-fixed ideals. Such ideals are 
      fixed by the action of the Borel subgroup of triangular matrices and play a special role in theory of Gröbner bases because initial 
      ideals in generic coordinates are of this type by a famous result by Galligo.
      In the context of parameter spaces of algebraic varieties, Galligo's theorem says that each component and each intersection of 
      components of a Hilbert scheme contains at least a point corresponding to a scheme defined by a Borel-fixed ideal. Hence, these ideals 
      are distributed throughout the Hilbert scheme and can be used to study its local structure. To this aim, in recent years several authors 
      developed algorithmic methods based on the use of strongly stable ideals to construct flat families corresponding to special loci of 
      the Hilbert scheme. In particular, a new open cover of the Hilbert scheme has been defined using strongly stable ideals and the action 
      of the projective linear group. In this construction, the list of all points corresponding to Borel-fixed ideals in a given Hilbert 
      scheme is needed. 
      The main feature of this package is a method to compute this set of points, i.e. the list of all saturated strongly stable ideals in
      a polynomial ring with a given Hilbert polynomial. The method has been theoretically introduced in [CLMR11] and improved in [Lel12].
      
      {\bf References:}
      
      [CLMR11] F. Cioffi, P. Lella, M.G. Marinari, M. Roggero: Segments and Hilbert schemes of points, {\it Discrete Mathematics}, 311(20):2238–2252, 2011.
      @BR{}@ Available at @HREF{"http://arxiv.org/abs/1003.2951"}@.
      
      [Lel12] P. Lella: An efficient implementation of the algorithm computing the Borel-fixed points of a Hilbert scheme, 
      {\it ISSAC 2012 — Proceedings of the 37th International Symposium on Symbolic and Algebraic Computation}, 242–248, ACM, New York, 2012.
      @BR{}@ Available at @HREF{"http://arxiv.org/abs/1205.0456"}@.
      
      {\bf Key user functions:}

        {\it Hilbert polynomials:}

          @TO isHilbertPolynomial@ -- Test whether a numerical polynomial is a Hilbert polynomial.

          @TO gotzmannDecomposition@ -- Compute Gotzmann's decomposition of a Hilbert polynomial.

          @TO gotzmannNumber@ -- Compute the Gotzmann number of a Hilbert polynomial.
	  
	  @TO macaulayDecomposition@ -- Compute Macaulay's decomposition of a Hilbert polynomial.

        {\it Strongly stable ideals and segment ideals:}

          @TO lexIdeal@ -- Compute the saturated lexicographic ideal with a given Hilbert polynomial.
          
          @TO stronglyStableIdeals@ -- Compute the saturated strongly stable ideals with a given Hilbert polynomial.
	  
	  @TO isGenSegment@ -- Test whether there exists a term ordering such that each minimal generator of a strongly stable ideal is greater than all monomials of the same degree outside the ideal.
      
	  @TO isRegSegment@ -- Test whether the truncation of a strongly stable ideal in degree equal to its regularity is a segment. 	                   

	  @TO isHilbSegment@ -- Test whether the truncation of a strongly stable ideal in degree equal to the Gotzmann number of its Hilbert polynomial is a segment. 
///   

doc ///
  Key
    isHilbertPolynomial
    (isHilbertPolynomial, ProjectiveHilbertPolynomial)
    (isHilbertPolynomial, RingElement)
  Headline
    Determine whether a numerical polynomial can be a Hilbert polynomial
  Usage
    isHilbertPolynomial p
  Inputs
    p : ProjectiveHilbertPolynomial
        or
    p : RingElement
        a numerical univariate polynomial.
  Outputs
    : Boolean
  Description
   Text
      Returns true if the input polynomial is an admissible Hilbert polynomial, false otherwise.
      
   Example
     QQ[t];
     isHilbertPolynomial(3*t+4)
     isHilbertPolynomial((2/3)*t-1)
     isHilbertPolynomial(2*projectiveHilbertPolynomial(2))
     isHilbertPolynomial(2*projectiveHilbertPolynomial(2,-1))
///

doc ///
  Key
    gotzmannDecomposition
    (gotzmannDecomposition, ProjectiveHilbertPolynomial)
    (gotzmannDecomposition, RingElement)
  Headline
    Compute Gotzmann's decomposition of Hilbert polynomial
  Usage
    gotzmannDecomposition hp
  Inputs
    hp : ProjectiveHilbertPolynomial
         or
    hp : RingElement
         a Hilbert polynomial.
  Outputs
    : List
  Description
   Text
      Returns the list of projective Hilbert polynomials of linear spaces summing up to the input polynomial:
 
   Example
     QQ[t];
     hp = projectiveHilbertPolynomial(3*t+4)
     gD = gotzmannDecomposition hp
     sum gD
  Description
   Text
     The decomposition suggests the most degenerate geometric object with the given Hilbert polynomial. 

   Example
     R = QQ[x,y,z,w];
     completeIntersection22 = ideal(random(2,R),random(2,R));
     hp = hilbertPolynomial completeIntersection22  
     gD = gotzmannDecomposition hp
  Description
   Text
     The degree of {\tt hp} is 1, so it is possible to obtain {\tt hp} as Hilbert polynomial of a
     scheme in the plane. Gotzmann's decomposition has 4 terms of degree 1 and 2 term of degree 0.
     This suggests that the generic union of 4 lines and 2 points in a plane should have Hilbert polynomial {\tt hp}:
 
   Example
     H = random(1,R);
     fourLines = for i from 1 to 4 list ideal(H,random(1,R));
     twoPoints = for i from 1 to 2 list ideal(H,random(1,R),random(1,R));         
     unionLinesPoints = intersect(fourLines|twoPoints);
     hilbertPolynomial unionLinesPoints == hp
///

doc ///
  Key
    macaulayDecomposition
    (macaulayDecomposition, ProjectiveHilbertPolynomial)
    (macaulayDecomposition, RingElement)
  Headline
    Compute Macaulay's decomposition of Hilbert polynomial
  Usage
    macaulayDecomposition hp
  Inputs
    hp : ProjectiveHilbertPolynomial
         or
    hp : RingElement
         a Hilbert polynomial.
  Outputs
    : List
  Description
   Text
      Returns the list of projective Hilbert polynomials of linear spaces summing up to the input polynomial:
      
   Example
     QQ[t];
     hp = projectiveHilbertPolynomial(3*t+4)
     mD = macaulayDecomposition hp
     sum mD
///

doc ///
  Key
    gotzmannNumber
    (gotzmannNumber, ProjectiveHilbertPolynomial)
    (gotzmannNumber, RingElement)
  Headline
    Compute the Gotzmann number of a Hilbert polynomial
  Usage
    gotzmannNumber hp
  Inputs
    hp : ProjectiveHilbertPolynomial
         or
    hp : RingElement
         a Hilbert polynomial.
  Outputs
    : ZZ
  Description
   Text
      Returns the Gotzmann number of the input polynomial, i.e. the length of its Gotzmann decomposition (see @TO gotzmannDecomposition@).
   
   Example
     QQ[t];
     gotzmannNumber(3*t+4)
     gotzmannDecomposition(3*t+4)
///

doc ///
  Key
    (projectiveHilbertPolynomial, RingElement)
  Headline
    
  Usage
    projectiveHilbertPolynomial p
  Inputs
    p : RingElement
        a Hilbert polynomial.
  Outputs
    : ProjectiveHilbertPolynomial
  Description
   Text
      Convert a @TO RingElement@ representing a Hilbert polynomial to a @TO ProjectiveHilbertPolynomial@.
   
   Example
     QQ[t];
     projectiveHilbertPolynomial (3*t+4)
///

doc ///
  Key
    OrderVariables
  Headline
    Option to set the order of indexed variables
  Description
   Text
    This option can be used to specify the order of the indexed variables of the polynomial ring containing the ideals,
    when calling @TO lexIdeal@ or @TO stronglyStableIdeals@ giving as input only the number of variables of the polynomial ring. 
    If @TO OrderVariables@ is set to @TO Up@ then @TT "x_i < x_j"@ iff @TT "i < j"@, otherwise 
    if @TO OrderVariables@ is set to @TO Down@ then @TT "x_i < x_j"@ iff @TT "i > j"@.
    
    The default is @TO Down@.

   Example
     lexIdeal(3, 3, OrderVariables=>Down)
     stronglyStableIdeals(3, 3, OrderVariables=>Up)
///

doc ///
  Key
    lexIdeal
    (lexIdeal, ProjectiveHilbertPolynomial, PolynomialRing)
    (lexIdeal, ProjectiveHilbertPolynomial, ZZ)
    (lexIdeal, RingElement, PolynomialRing)
    (lexIdeal, RingElement, ZZ)
    (lexIdeal, ZZ, PolynomialRing)
    (lexIdeal, ZZ, ZZ)
  Headline
    Compute the saturated lexicographic ideal in the given ambient space with given Hilbert polynomial
  Usage
    lexIdeal (hp ,S)
    lexIdeal (hp, n)
    lexIdeal (d, S)
    lexIdeal (d, n)
  Inputs
    hp : ProjectiveHilbertPolynomial
         or
    hp : RingElement
         a Hilbert polynomial; 
    d : ZZ
        a positive integer corresponding to a constant Hilbert polynomial;
    S : PolynomialRing
        with @TT "numgens S > 1"@;
    n : ZZ
         number of variables of the polynomial ring.
  Outputs
    : Ideal
  Description
   Text
      Returns the saturated lexicographic ideal defining a subscheme of \mathbb{P}^{n} or @TT "Proj S"@ 
      with Hilbert polynomial @TT "hp"@ or @TT "d"@.
   
   Example
     QQ[t];
     S = QQ[x,y,z,w];
     lexIdeal(4*t, S)
     lexIdeal(4*t, 5)
     hp = hilbertPolynomial oo
     lexIdeal(hp, S)
     lexIdeal(hp, 3)
     lexIdeal(5, S)
     lexIdeal(5, 3)
///

doc ///
  Key
    [lexIdeal,CoefficientRing]
  Headline
    Option to set the ring of coefficients
  Description
   Text
    This option can be used to specify the ring of coefficients of the polynomial ring containing the ideals,
    when calling @TO lexIdeal@ giving as input only the number of variables of the polynomial ring. 
    
    The default is @TO QQ@.

   Example
     lexIdeal(10, 3, CoefficientRing=>ZZ/101)
///

doc ///
  Key
    [lexIdeal,OrderVariables]
  Headline
    Option to set the order of indexed variables
  Description
   Text
    This option can be used to specify the order of the indexed variables of the polynomial ring containing the ideals,
    when calling @TO lexIdeal@ giving as input only the number of variables of the polynomial ring. 
    If @TO OrderVariables@ is set to @TO Up@ then @TT "x_i < x_j"@ iff @TT "i < j"@, otherwise 
    if @TO OrderVariables@ is set to @TO Down@ then @TT "x_i < x_j"@ iff @TT "i > j"@.
    
    The default is @TO Down@.

   Example
     lexIdeal(3, 3, OrderVariables=>Down)
     lexIdeal(3, 3, OrderVariables=>Up)
///

doc ///
  Key
    stronglyStableIdeals
    (stronglyStableIdeals, ProjectiveHilbertPolynomial, PolynomialRing)
    (stronglyStableIdeals, ProjectiveHilbertPolynomial, ZZ)
    (stronglyStableIdeals, RingElement, PolynomialRing)
    (stronglyStableIdeals, RingElement, ZZ)
    (stronglyStableIdeals, ZZ, PolynomialRing)
    (stronglyStableIdeals, ZZ, ZZ)
  Headline
    Compute the saturated strongly stable ideals in the given ambient space with given Hilbert polynomial
  Usage
    stronglyStableIdeals (hp ,S)
    stronglyStableIdeals (hp, n)
    stronglyStableIdeals (d, S)
    stronglyStableIdeals (d, n)
  Inputs
    hp : ProjectiveHilbertPolynomial
         or
    hp : RingElement
         a Hilbert polynomial; 
    d : ZZ
        a positive integer corresponding to a constant Hilbert polynomial;
    S : PolynomialRing
          with @TT "numgens S > 1"@;
    n : ZZ
         number of variables of the polynomial ring.
  Outputs
    : List
  Description
   Text
      Returns the list of all the saturated strongly stable ideals defining subschemes of \mathbb{P}^{n} or @TT "Proj S"@ 
      with Hilbert polynomial @TT "hp"@ or @TT "d"@.
   
   Example
     QQ[t];
     S = QQ[x,y,z,w];
     stronglyStableIdeals(4*t, S)
     stronglyStableIdeals(4*t, 4)
     hp = hilbertPolynomial(oo#0)
     stronglyStableIdeals(hp, S)
     stronglyStableIdeals(hp, 4)
     stronglyStableIdeals(5, S)
     stronglyStableIdeals(5, 4)
///

doc ///
  Key
    MaxRegularity
    [stronglyStableIdeals,MaxRegularity]
  Headline
    Option to set the maximum regularity
  Description
   Text
    This option can be used to give an upper bound to the regularity of the strongly stable ideals.
    
    The default is null (i.e. no bound).
    
   Example
     QQ[t];
     stronglyStableIdeals(4*t, 4, MaxRegularity=>4)
///

doc ///
  Key
    [stronglyStableIdeals,CoefficientRing]
  Headline
    Option to set the ring of coefficients
  Description
   Text
    This option can be used to specify the ring of coefficients of the polynomial ring containing the ideals,
    when calling @TO stronglyStableIdeals@ giving as input only the number of variables of the polynomial ring. 
    
    The default is @TO QQ@.

   Example
     QQ[t];
     stronglyStableIdeals(3, 3, CoefficientRing=>ZZ/101)
     oo#0
///

doc ///
  Key
    [stronglyStableIdeals,OrderVariables]
  Headline
    Option to set the order of indexed variables
  Description
   Text
    This option can be used to specify the order of the indexed variables of the polynomial ring containing the ideals,
    when calling @TO stronglyStableIdeals@ giving as input only the number of variables of the polynomial ring. 
    If @TO OrderVariables@ is set to @TO Up@ then @TT "x_i < x_j"@ iff @TT "i < j"@, otherwise 
    if @TO OrderVariables@ is set to @TO Down@ then @TT "x_i < x_j"@ iff @TT "i > j"@.
    
    The default is @TO Down@.

   Example
     stronglyStableIdeals(3, 3, OrderVariables=>Down)
     stronglyStableIdeals(3, 3, OrderVariables=>Up)
///

doc ///
  Key
    isGenSegment
    (isGenSegment, MonomialIdeal)
    (isGenSegment, Ideal)
  Headline
    gen-segment ideals
  Usage
    isGenSegment J
  Inputs
    J : MonomialIdeal
        or
    J : Ideal
        a strongly stable ideal.
  Outputs
    : Sequence
  Description
   Text
      The first element of the sequence is a boolean value. It tells if the ideal is a segment, i.e. if there exists a term ordering
      such that every generator of the ideal is strictly greater than every monomial outside the ideal of the same degree.     
      If true, the second element of the sequence contains the list of weights giving the corresponding ordering on the monomials.
   
   Example
     QQ[x,y,z];
     isGenSegment(ideal(x^2,x*y,y^4))
     isGenSegment(ideal(x^2,x*y^3,y^4))
///

doc ///
  Key
    isRegSegment
    (isRegSegment, MonomialIdeal)
    (isRegSegment, Ideal)
  Headline
    reg-segment ideals
  Usage
    isRegSegment J
  Inputs
    J : MonomialIdeal
        or
    J : Ideal
        a strongly stable ideal.
  Outputs
    : Sequence
  Description
   Text
      Calls the method @TO isGenSegment@ on the ideal @TT "truncate(regularity J, J)"@.
   
   Example
     QQ[x,y,z];
     J = ideal(x^2,x*y^3,y^4)
     isRegSegment J
     isGenSegment(truncate(regularity J,J))
     isGenSegment J
///

doc ///
  Key
    isHilbSegment
    (isHilbSegment, MonomialIdeal)
    (isHilbSegment, Ideal)
  Headline
    hilb-segment ideals
  Usage
    isHilbSegment J
  Inputs
    J : MonomialIdeal
        or
    J : Ideal
        a strongly stable ideal.
  Outputs
    : Sequence
  Description
   Text
      Calls the method @TO isGenSegment@ on the ideal @TT "truncate(gotzmannNumber(hilbertPolynomial J), J)"@.
   
   Example
     QQ[x,y,z];
     J = ideal(x^2,x*y,y^4);
     isHilbSegment J
     isGenSegment(truncate(gotzmannNumber(hilbertPolynomial J), J))
///

end
restart
installPackage "StronglyStableIdeals"
check "StronglyStableIdeals"
