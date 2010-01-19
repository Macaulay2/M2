-- taking code mostly from local/classes/math731-factoring

newPackage(
        "UPolynomials",
    	Version => "0.1", 
    	Date => "7 Aug 2009",
        Authors => {{Name => "Mike Stillman", 
                  Email => "mike@math.cornell.edu", 
                  HomePage => "http://www.math.cornell.edu/~mike"}},
        Headline => "gcds and factorization of univariate polynomials",
        DebuggingMode => true
        )

export {
     myIsDivisibleBy,
     myExactDivision,
     myGCD,
     myContent,
     myPrimitivePart,
     myCoefficients,
     myPseudoDivision,
     myPseudoRemainder,
     myResultant,
     CRA,
     primitivePolynomialGCD,
     subresultantGCD,
     mySquareFreeDecomposition,
     --myFactorization,
     --myFactor,
     factorization,
     setUFD,
     makeTower,
     deg,
     adjoinRoot}

myIsDivisibleBy = (f,g) -> (ring f).myIsDivisibleBy(f,g)

myExactDivision = (f,g) -> (ring f).myExactDivision(f,g)

-- Only keep one of these two versions...
myGCD = args -> (
     R := ring (args#0);
     if not R.?myGCD then (if isField R then setUFD R else error ("myGCD: expected " | toString R | " to be a UFD"));
     result := args#0;
     for i from 1 to #args-1 when result != 1_R do 
         result = R.myGCD(result, args#i);
     result
     );

ZZ.myIsDivisibleBy = (f,g) -> f % g === 0
ZZ.myExactDivision = (f,g) -> f // g;
ZZ.myGCD= (f,g) -> gcd(f,g);

QQ.myIsDivisibleBy = (f,g) -> g != 0 or f == 0
QQ.myExactDivision = (f,g) -> f // g;
QQ.myGCD = (f,g) -> if f != 0 or g != 0 then 1_QQ else 0_QQ;

deg = (f) -> if f == 0 then -1 else (degree f)#0

myCoefficients = (f) -> (
     if f == 0 then {} else (
     	  d := deg f;
     	  x := (ring f)_0;
	  for i from 0 to d list coefficient(x^i, f)))

myContent = (f) -> (if f == 0 or isField ring f then 1_(ring f) 
  else if isField coefficientRing ring f then leadCoefficient f
  else myGCD myCoefficients f
  )

myPrimitivePart = (f) -> (
     c := myContent f;
     if c == 1 then f else
     myExactDivision(f, c*1_(ring f)))   -- TODO: should divide each part of f?

myPolynomialIsDivBy = (f,g) -> try (myPolynomialExactDivision(f,g);true) else false;

myPolynomialExactDivision = (f,g) -> (
     -- f and g should be polynomials in a ring k[x], with k a UFD.
     -- if leadCoefficient g is invertible, a better method could be
     -- used.
     A := ring f;
     x := A_0;   -- the variable...
     R := f;
     Q := 0_A;
     K := coefficientRing A;
     if ring g === K then g = g * 1_A;
     cg := leadCoefficient g;
     while R != 0 and deg(R) >= deg(g) do (
	  c := K.myExactDivision(leadCoefficient R, cg);
          S := c * x^(deg R - deg g);
          Q = Q + S;
          R = R - S*g;
          );
     if R != 0 then error "expected divisor of element";
     Q)

setUFD = (R) -> (
     if isField R then (
     	  R.myIsDivisibleBy = ZZ.myIsDivisibleBy;
     	  R.myExactDivision = ZZ.myExactDivision;
     	  R.myGCD = (f,g) -> if f == 0 and g == 0 then 0_R else 1_R;
	  )
     else if isPolynomialRing R then (
     	  R.myIsDivisibleBy = myPolynomialIsDivBy;
     	  R.myExactDivision = myPolynomialExactDivision;
     	  R.myGCD = primitivePolynomialGCD;
     	  R.myGCD = subresultantGCD;	  
	  )
     );

makeTower = (R) -> (
     -- R should be a polynomial ring over either ZZ or a finite field
     g := generators R;
     A := coefficientRing R;
     scan(reverse g, v -> (
	       B := A[v,MonomialSize=>16];
	       setUFD B;
	       A = B));
     A)

--------------------------------------------
-- Lift numbers mod m to rational numbers --
--------------------------------------------

liftToQQ = (c, m) -> (
     -- return either null or a rational number a/b.
     -- a/b is returned if there are a, b s.t. 2*(a^2 + b^2) < m such
     --  that a == b*c (mod m)
     -- If not, returns null
     (v1,v2,v3) := (0,1,c);
     (u1,u2,u3) := (1,0,m);
     while 2*v3*v3 >= m do (
	  q := u3 // v3;
	  (r1,r2,r3) := (u1-q*v1,u2-q*v2,u3-q*v3);
	  (u1,u2,u3) = (v1,v2,v3);
	  (v1,v2,v3) = (r1,r2,r3);
	  );
     if 2*v2*v2 >= m then null
     else v3/v2)

-----------------------------------------
-- Integer Chinese remainder algorithm --
-- CRA for a set of integers mod a     --
-- set of prime moduli                 --
-- (or at least pairwise rel prime)    --
-----------------------------------------
myCoefficientsToPoly = (coeffs, var) -> (
     -- If var is a variable, this creates a polynomial
     -- If var is a scalar, this evaluates the corresponding polynomial
     n := #coeffs - 1;
     val := coeffs#n;
     for i from 1 to n do
       val = coeffs#(n-i) + val * var;
     val)

cra0 = (ps) -> (
     -- ps is a list of pairwise relatively prime integers, {m1, m2, ..., mn}
     -- returns a list {d1, ..., dn}, where
     -- di = (m1 m2 ... m(i-1))^(-1) mod mi
     for i from 1 to #ps-1 list (
	  m := ps#i;
	  val := ps#0 % m;
	  for j from 1 to i-1 do 
	    val = (val * ps#j) % m;
	  (g,u,v) = toSequence gcdCoefficients(val, m);
	  u))

cra1 = (ps,us,ds) -> (
     -- Returns a list vs of mixed-radix numbers.
     n := #ps;
     vs := new MutableList from toList(n:0);
     vs#0 = us#0 % ps#0;
     for k from 1 to n-1 do (
	  m := ps#k;
	  temp := vs#(k-1);
	  for j from 0 to k-2 do 
	    temp = (temp * ps#(k-2-j) + vs#(k-2-j)) % m;
	  vs#k = ((us#k - temp) * ds#(k-1)) % m);
     toList vs
     )

cra2 = (ps,vs) -> (
     -- Returns an integer.
     -- if ps = {m1,...,mn}
     -- and vs = {v1,...,vn}, this returns the integer
     -- v1 + v2 * m1 + v3 * m1 * m2 + ... + vn * m1 * ... * m(n-1)
     n := #ps-1;
     val := vs#n;
     for i from 1 to n do
       val = val * ps#(n-i) + vs#(n-i);
     val)

cra = (ps,us) -> (
     -- Returns an integer.
     ds := cra0 ps;
     vs := cra1(ps,us,ds);
     cra2(ps,vs)
     )

CRA = (ps,fs) -> (
     -- ps should be a list of pairwise rel prime integers
     -- fs should be a list of polynomials, same length as ps.
     -- lift each coeff using integer Chinese remainder alg
     x := (ring (fs#0))_0;
     cfs := apply(fs, myCoefficients);
     lens := apply(cfs, a -> #a);
     d := max lens;
     lifted := apply(d, i -> (
	       cs := apply(cfs, cf -> if #cf >= i then cf#i else 0);
	       cra(ps,cs)));
     myCoefficientsToPoly(lifted,x))

///
restart
debug loadPackage "UPolynomials"

kk = ZZ/32003
a = lift(3_kk/5_kk,ZZ)
liftToQQ(a, 32003)

a = lift(3_kk/8_kk,ZZ)
liftToQQ(a, 32003)

a = lift(28_kk/29_kk,ZZ)
liftToQQ(a, 32003)

cra0{2,3,5,7,11}
cra({2,3,5},{1,1,2})
m = cra({1048583, 1048589, 1048601, 1048609, 1048613, 1048627, 1048633, 1048661},{1,1,3,4,7,123,654,1000})
apply(select(1..10000, i -> isPrime(2^29+2*i+1)), a -> 2^29 + 2*a + 1)
product toList oo
ms = {1048583, 1048589, 1048601, 1048609, 1048613, 1048627, 1048633, 1048661}
product ms
apply(ms, p -> m % p)


R = ZZ[x]
f1 = x^3+x+1
f2 = x^3+x^2+4*x+6
g = CRA({23,31},{f1,f2})
apply(myCoefficients g, c -> c % 23)
apply(myCoefficients g, c -> c % 31)


R = ZZ[x]
f1 = x^3+x+1
f2 = x^3+x^2+4*x+6
f3 = 2*x^3+13*x^2+4*x+16
g = CRA({23,1048583,101},{f1,f2,f3})
///

---------------------
-- Pseudo division --
---------------------

myPseudoDivision = method()
myPseudoDivision(RingElement,RingElement) := (F,G) -> (
     R := F;
     Q := 0_(ring F);
     m := deg F;
     n := deg G;
     d := leadCoefficient G;
     X := (ring F)_0;
     if m < n then (
	  error "myPseudoDivision: degree assumption violated";
	  )
     else (
	  e := m-n+1;
	  while R != 0 and deg R >= n do (
	       S := (leadCoefficient R) * X^(deg R - n);
	       Q = d*Q + S;
	       R = d*R - S*G;
	       e = e-1;
	       );
	  q := d^e;
	  Q = q*Q;
	  R = q*R;
	  );
     (R,Q)
     )

myPseudoRemainder = method()
myPseudoRemainder(RingElement,RingElement) := (F,G) -> (
     R := F;
     m := deg F;
     n := deg G;
     d := leadCoefficient G;
     X := (ring F)_0;
     if m < n then (
	  error "myPseudoRemainder: degree assumption violated";
	  )
     else (
	  e := m-n+1;
	  while R != 0 and deg R >= n do (
	       S := (leadCoefficient R) * X^(deg R - n);
	       R = d*R - S*G;
	       e = e-1;
	       );
	  q := d^e;
	  R = q*R;
	  );
     R
     )

testPseudoDivision = (f,g) -> (
     ans := myPseudoDivision(f,g);
     n := deg f;
     m := deg g;
     c := leadCoefficient g;
     assert(ans#1 * g + ans#0 == c^(n-m+1) * f))

-------------------------------
-- GCD algorithms over UFD's --
-------------------------------

-- The next routine needs to be rewritten.
polynomialGCDCoefficients = (F,G) -> (
     -- F and G should be elements of a ring R = K[x], with 
     -- k a field.
     -- Returns a list {H,U,V}, such that H = gcd(F,G)
     -- and H = U*F + V*G, and U and V have smallest degree
     -- possible: deg(U) < deg(F/H), deg(V) < deg(G/H).
     A := ring F;
     U := 1_A;
     H := F;
     V1 := 0_A;
     V3 := G;
     while V3 != 0 do (
	  ans := division(H,V3);
	  R := ans#0;
	  Q := ans#1;
	  T := U - V1 * Q;
	  U = V1;
	  H = V3;
	  V1 = T;
	  V3 = R;
	  );
     {H, U, (H - F*U) // G}
     )

primitivePolynomialGCD = (F,G) -> (
     -- F, G in K[x], K a UFD
     A := ring F;
     K := coefficientRing A;
     if G == 0 then F
     else if F == 0 then G
     else (
	  -- Replace F,G with their primitive parts
	  f := myContent F;
	  g := myContent G;
	  d := myGCD(f,g);
	  F = myExactDivision(F,f);
	  G = myExactDivision(G,g);
	  if deg F < deg G then (T := F; F = G; G = T);
	  while (
	       R := myPseudoRemainder(F,G);
	       if R != 0 and deg R === 0 then (R = 0_A; G = 1_A);
	       R != 0) do (
	       F = G;
	       G = myPrimitivePart R;
	       print toString G;
	       );
	  d*G))

subresultantGCD = (F,G) -> (
     -- F, G in K[x], K a UFD
     A := ring F;
     K := coefficientRing A;
     if G == 0 then F
     else if F == 0 then G
     else (
	  -- Replace F,G with their primitive parts
	  f := myContent F;
	  g := myContent G;
	  d := myGCD(f,g);
	  F = myExactDivision(F,f);
	  G = myExactDivision(G,g);
	  if deg F < deg G then (T := F; F = G; G = T);
	  g = 1_K;
	  h := 1_K;
	  c := 0;
     	  while (
	       delta := deg F - deg G;
	       R := myPseudoRemainder(F,G);
	       if R != 0 and deg R === 0 then (R = 0_A; G = 1_A);
	       R != 0) do (
	       	    F = G;
		    G = myExactDivision(R,g*h^delta);
		    c = c+1;
		    if c < 10 then (
			 g = leadCoefficient F;
			 if delta === 0 then h = h*g^delta
			 else if delta === 1 then h = g^delta
			 else h = myExactDivision(g^delta, h^(1-delta));
			 )
		    else (
			 F = myPrimitivePart F;
			 G = myPrimitivePart G;
			 g = h = 1_K;
			 c = 0;
			 );
		    );
          d * myPrimitivePart G 
	  )
     )


myResultant = method()
myResultant(RingElement,RingElement) := (F,G) -> (
     -- F and G should be polynomials in R[x], R a UFD.
     if F == 0 or G == 0 then return F;
     A := ring F;
     K := coefficientRing A;
     --R := coefficientRing ring A;
     g := 1_K;
     h := 1_K;
     s := 1;
     a := myContent F;
     b := myContent G;
     F = myExactDivision(F,a);
     G = myExactDivision(G,b);
     t := a^(deg G) * b^(deg F);
     if deg F < deg G then (
	  -- swap F,G.  Change sign of resultant.
	  H := F;
	  F = G;
	  G = H;
	  if (deg F) % 2 =!= 0 and (deg G) % 2 =!= 0
	  then s = -1;
	  );
     while (
	  delta := deg F - deg G;
	  if (deg F) % 2 =!= 0 and (deg G) % 2 =!= 0
	  then s = -s;
	  --<< "F = " << toString(F) << endl;
	  --<< "G = " << toString(G) << endl;
	  R := myPseudoRemainder(F,G);
	  --<< "R = " << toString R << endl;
	  F = G;
	  --<< "G = " << toString G << endl;
	  --<< "g*h^delta = " << toString(g*h^delta) << endl;
	  G = myExactDivision(R, g*h^delta);
	  --<< "B//oo = " << toString B << endl;
	  g = leadCoefficient F;
	  if delta > 0 then
	    h = myExactDivision(g^delta, h^(delta-1));
	  deg G > 0) do ();
     if deg F === 0 then (
	  print "deg F === 0";
	  )
     else (
	  h = myExactDivision((leadCoefficient G)^(deg F), h^(deg F - 1));
	  );
     s*t*h
     )

mySquareFreeDecomposition = method()
mySquareFreeDecomposition RingElement := (f) -> (
     R := ring f;
     p := char R;
     x := R_0;
     result := {};
     T := myGCD(f,diff(x,f));
     V := myExactDivision(f,T);
     k := 0;
     while deg(V) =!= 0 do (
	  k = k+1;
	  W := myGCD(T,V);
	  Ak := myExactDivision(V,W);
	  V = W;
	  T = myExactDivision(T,V);
	  if deg(Ak) =!= 0 then 
	      result = append(result,(k,myPrimitivePart Ak));
	  );
     if deg T != 0 then (
	  -- we have a polynomial in x^p
	  error "char p square free decomposition is not yet implemented";
	  result2 := squarefree lowerP(T);
	  result2 = apply(result2, a -> (p*a#0, a#1));
	  result = join(result,result2);
	  );
     result
     );

---------------------------------------------------
-- Factorization over algebraic extension fields --
---------------------------------------------------
-- See H. Cohen's book (starting pg. 143) for the algorithm used
-- Is this correct over finite fields? (Always? Mostly?)

myfac0 = (F, S, G, phis) -> (
     -- F is a squarefree polynomial in R = K[x], K = kk[a]/G(a)
     -- S = kk[x][a],
     -- G is in S
     -- phi: a list of pairs of ring maps (S <-- R, R <-- S),
     --   changing coordinates to find a resultant which is squarefree
     --   (created in myFactorization)
     for k from 0 to #phis-1 do (
	  -- we will return on the first one that works
	  (f,g) := phis#k;
	  H := myResultant(G, f F);
	  --H := resultant(G, f F, S_0);
	  --<< H << endl << endl;
	  if H != 1 and gcd(diff((ring H)_0,H),H) == 1 then (
	       -- yes! we are almost there...
	       facsH := factor H;
	       pows := (toList facsH)/toList/last;
	       facsH = (toList facsH)/toList/first;
	       if any(pows, x -> x > 1) then << "powers of irred factors of resultant are " << pows << endl;
	       facsH = select(facsH, f -> first degree f > 0);
	       result := apply(facsH, h -> myGCD(F, g h));
	       result = select(result, f -> first degree f > 0);
	       return result
	       )
	  );
     error "wow -- the first 9 tries failed!";
     )

myFactorization = method()
myFactorization RingElement := (F) -> (
     -- For univariate polynomials over a singly generated extension field
     R := ring F;
     K := coefficientRing R;
     -- check: if R is a tower of form kk[a]/(g(a))[t], then keep it
     --        if not, is R a ring of this form? if so, do setUFD's.
     --        if not: error
     if numgens R =!= 1 then error "expected univariate polynomial";
     if deg F === 1 then return {(1,F)};
     if not isField K then error "expected coefficient ring to be a field";
     if numgens K >= 2 then error "expected prime field or singly generated extension field";
     kk := coefficientRing coefficientRing K;  -- K = kk[a]/(g(a)) []
     if not K.?myGCD then setUFD K;
     if not R.?myGCD then setUFD R;
     S1 := kk (monoid [t]); setUFD S1;
     S := S1 (monoid [a]); setUFD S;
     alpha := K_0;
     phi0 := map(S,R,{S_1,S_0});
     phi0' := map(R,S,{alpha,R_0});
     IR := (ideal coefficientRing K)_0; -- coefficientRing K is, if K is a toField, is the corresp poly ring
     i0 := map(S,ring IR,{S_0});
     G := i0 IR;
     phis := apply(1..8, k -> (
	       (map(S,R,{S_1-k*S_0,S_0}),
     		map(R,S,{alpha,R_0+k*alpha})
	       )));
     sqfree := mySquareFreeDecomposition F;
     -- set up S, G, and the various maps: R --> S, inverses S --> R.
     flatten  apply(sqfree, (d,Fi) -> (
	       facs := myfac0(Fi,S,G, phis);
	       apply(facs, f -> (d,f))
	       ))
     )

factorGF = (F) -> (
     -- assumption: coeff ring of F is a Galois field
     -- and the ring of F has one variable
     K := coefficientRing ring F;
     K1 := toField(ambient K);
     R1 := K1[Variables=>1];
     toR := map(R, R1, {R_0, K_0});
     toR1 := map(R1, R, {R1_0, K1_0});
     apply(myFactorization toR1 F, (i,f) -> (i,toR f))
     )

-- not used currently:
myFactor = method()
myFactor RingElement := (F) -> (
     facs := myFactorization F;
     product apply(facs, (i,g) -> (hold g)^(hold i)))

factorization = method()
factorization RingElement := (F) -> (
     --<< "factoring " << F << " over " << describe ring F << endl;
     R := ring F;
     K := coefficientRing R;
     result := if isPolynomialRing K and numgens R === 1 then
       VerticalList myFactorization F
     else if instance(K, GaloisField) and numgens R === 1 then
       VerticalList factorGF F
     else
       select(VerticalList factor F, f -> first degree f#0 > 0)/toSequence/reverse;
     --<< "  result: " << netList result << endl;
     result
     )

----------------------------------------------
-- adjoining a root of a polynomial ----------
-- this likely will change interface, and ----
-- leave this file! --------------------------
----------------------------------------------
adjoinRoot = method()
adjoinRoot RingElement := (f) -> (
     K1top := null;
     K1 := null;
     R := ring f;
     t := R_0;
     K := coefficientRing R;
     if deg f == 1 then (
	  -- we need to solve for the root
	  a := coefficient(t, f);
	  b := coefficient(1_R, f);
	  if a == 1 then -b else -b/a
	  )
     else (
	  -- We need to create a new ring
	  -- The default will be to make one non-tower ring out of all of this
	  -- Assuming that f is monic?
	  if not isPolynomialRing K then (
     	       -- in this case, K is QQ or a prime finite field
	       K1top = K[vars 0];
	       K1 = K1top/sub(f, t => K1top_0);
	       K1 = toField K1;
	       setUFD K1;
	       K1_0
	       )
	  else (
	    K = coefficientRing K;
     	    I := ideal K;
     	    n := numgens K;
	    K1top = (coefficientRing K)[vars n, gens K, MonomialOrder=>Lex];
	    to1 := map(K1top, ring I, drop(gens K1top,1));
	    to2 := map(K1top, R, gens K1top);
	    J := ideal to2 f + to1 I;
	    K1 = K1top/J;
	    K1 = toField K1;
	    setUFD K1;
	    K1_0
	  )
     ))


beginDocumentation()

multidoc ///
Node
  Key
    UPolynomials
  Headline
    univariate polynomial operations implemented at top level in Macaulay2
  Description
    Text
    Example
  Caveat
  SeeAlso
Node
  Key
    adjoinRoot
    (adjoinRoot,RingElement)
  Headline
    adjoin an algebraic element to a field
  Usage
    a = adjoinRoot f
  Inputs
    f:RingElement
      An element of a ring A[t] (t can be any name), where A is a field
  Outputs
    a:RingElement
      An element in an extension field B of A, whose minimal polynomial is f(t)
  Description
   Text
     The variables are currently named a,b,c,...  In the future, more flexible names
     might be allowed.  The way this works is to take all of the variables in A, if any,
     and the new variable, and make one quotient ring out of it.  @TO toField@ is called
     to make the ring usable as a field, and for Groebner basis coefficient fields.
   Example
     R = QQ[t];
     a = adjoinRoot(t^2+t+1)
     A = ring a
     a^2+a+1
     S = A[t];
     b = adjoinRoot(t^3-a)
     b^6
  Caveat
    The polynomial does not have to be irreducible, but if it is not,
    then it is possible for some operations to give errors
  SeeAlso
    toField
Node
   Key
     factorization
     (factorization, RingElement)
   Headline
     factor a univariate polynomial over a number field
   Usage
     L = factorization F
   Inputs
     F:RingElement
       F should be an element of K[x], where K is a field.  
   Outputs
     L:List
       of pairs (i, G), where G is irreducible, and F = G1^i1 * ... * Gr^ir
       up to a scalar
   Description
    Text
      If the base ring is 
    Example
      K = toField(QQ[i]/(i^2+1))
      R = K[x]
      F = 3*(x^8-1)
      factorization F
      netList oo
   Caveat
     This function does not factor multivariate polynomials over number fields, nor
     does it factor univariate polynomials over towers of number fields.
     This function is based on toplevel Macaulay2 code, and so can sometimes be slow.
     It should be rewritten in the engine!
   SeeAlso
     factor
///

///
Node
   Key
   Headline
   Usage
   Inputs
   Outputs
   Consequences
    Item
   Description
    Text
    Code
    Pre
    Example
   Subnodes
   Caveat
   SeeAlso
///

TEST ///
R = ZZ[x]
setUFD R
g = (5*x-2)*(3*x^2-13)
f = (2*x^2+3*x+17)*g
assert(myExactDivision(f,g) == 2*x^2+3*x+17)
S = R[y]
setUFD S
h = (y^3-x*y-13)^2 * (y-x) * f
assert(myExactDivision(h,y-x) == (y^3-x*y-13)^2 *  f)
assert(myExactDivision(h,f*1_S) == (y^3-x*y-13)^2 * (y-x))
assert(myExactDivision(h,f) == (y^3-x*y-13)^2 * (y-x))
assert(not myIsDivisibleBy(h+1_S,f*1_S))
assert(myIsDivisibleBy(h,f))
assert(not myIsDivisibleBy(h+1_S,f))

R = ZZ[x,y,z,w]
Rlex = makeTower R
f = (x^2+103467*x*y+z)*(x+y+w)
g = (x^2+103467*x*y+z)^2*(x-y+w)
time ans = subresultantGCD(f,g)
time assert(ans == x^2+103467*x*y+z)
time assert((ans = myGCD(f,g)) == x^2+103467*x*y+z)
use R
substitute(ans,R) == x^2+103467*x*y+z
///

TEST ///
assert not myIsDivisibleBy(13,3) 
assert(myGCD(12,15,21) == 3)
assert(myExactDivision(137*23213123200,137*10) == 2321312320)
///

TEST ///
R = ZZ[x]
S = R[y]
setUFD S
setUFD R
debug UPolynomials
(a1,a2) = myPseudoDivision(x*y^3-2, (x+1)*y - 3)
assert(a1 == myPseudoRemainder(x*y^3-2, (x+1)*y - 3))
testPseudoDivision(x*y^3-2, (x+1)*y - 3)
(a1,a2) = myPseudoDivision(3*x^5-17*x^4+23*x^2-41*x-13, 7*x^3-x-120)
assert(a1 == myPseudoRemainder(3*x^5-17*x^4+23*x^2-41*x-13, 7*x^3-x-120))
testPseudoDivision(3*x^5-17*x^4+23*x^2-41*x-13, 7*x^3-x-120)
///

TEST ///
R = ZZ[x]
setUFD R
g = (5*x-2)*(3*x^2-13)
f = (2*x^2+3*x+17)*g
h = (3*x^3-x^2-12)*g
time assert(primitivePolynomialGCD(f,h) == g)
time assert(subresultantGCD(f,h) == g)

use R
S = R[y]
setUFD S
F = (3*x^2-y^2-13*x*y)^5*(x+y-1)^3
G = (3*x^2+y^2-13*x*y)^5*(x+y-1)^5
time assert(subresultantGCD(F,G) == (x+y-1)^3)
time assert(primitivePolynomialGCD(F,G) == (x+y-1)^3)
S1 = flattenRing S
S1 = S1_0
F = sub(F,S1)
G = sub(G,S1)
use S1
time assert(gcd(F,G) == (x+y-1)^3) -- much faster
///

TEST ///
R = QQ[x]
setUFD R
F = (3*x^3-1)^2*(13*x^4-x^2-53)^3*(x-4)
time D = mySquareFreeDecomposition F
assert(F == leadCoefficient F * product apply(D, v -> (v#1)^(v#0)))
///

TEST ///
R = QQ[t];
a = adjoinRoot(t^2+t+1)
A = ring a
assert(a^2+a+1 == 0)
S = A[t];
b = adjoinRoot(t^3-a)
b^6
b^-1
assert(b * b^-1 == 1)

B = ring b
S = B[t]
setUFD S
F = (b*t^4-a*t-1)*(a*t^3-b*t-1)
myExactDivision(F, a*t^3-b*t-1)
///

-- Factorization tests --
TEST ///
K = toField(QQ[i]/(i^2+1))
R = K[x]
F = 3*(x^8-1)
L = factorization F
--assert(product(L, (r,f) -> f^r) == F) -- FAILS, since the 3 is lost...
assert(
     set factorization F
     === set {(1,x+i), (1,x-i), (1,x-1), (1,x+1), (1,x^2+i), (1,x^2-i)})
///

TEST ///
K = toField(ZZ/32003[a]/(a^8-a-1))
R = K[x]
F = x^8-x-1
L = factorization F
assert(#L == 8)
assert(product(L, (r,f) -> f^r) == F)
///

TEST ///
K = GF(25, Variable=>a)
R = K[x]
F = x^8-x-1
L = factorization F
assert(#L == 4)
assert(product(L, (r,f) -> f^r) == F)
///

end

------------------------------------------------------------
-- Below this needs to be rewritten as useful tests
TEST ///
-- Here we test gcd's and square free decomposition over algebraic number fields
R = QQ[t];
a = adjoinRoot(t^2+t+1)
A = ring a
S = A[t];
b = adjoinRoot(t^3-a)
B = coefficientRing ring b -- this coefficientRing picks out the poly ring from the field
use ring ideal B
eliminate(a,ideal B)

A = QQ[b]/(b^6+b^3+1)
A = toField A
setUFD A

R = A[t]
setUFD R
F = myExactDivision(t^6+t^3+1, t-b)

use A
use ring F
sub(F, {t => t - b})

R' = QQ[b,t]
F' = sub(F, R')
use R'
phi = map(R',R,{t-2*b,b})
F' =  phi F
G' = sub((ideal coefficientRing A)_0, R')
resultant(F',G',b)
factor oo
facs = oo//toList/toList/first
phi1 = map(R,R',{A_0, R_0 + 2*A_0}) -- FAILS
apply(facs, f -> myGCD(phi1 f, F))

G' = sub((ideal coefficientRing A)_0, R')

resultant(F',G',t)
-- We want to factor F...


///

TEST ///
--loadPackage "UPolynomials"
-- test of factorization over algebraic extensions of QQ
A = toField(QQ[b]/(b^6+b^3+1)); setUFD A
R = A[t]; setUFD R
F = t^6+t^3+1
time myFactorization F
time myFactor F

A = toField(QQ[b]/(b^6+b^3+1));
R = A[t];
F = t^6+t^3+1
time myFactorization F
time myFactor F
value oo

A = toField(QQ[b]/(b^8+b^3+1))
setUFD A

R = A[t]
setUFD R
F = t^8+t^3+1
time fac = myFactorization F
use A; use R
F1 = myExactDivision(F,t-b)

-- TODO: adjoin a root of this, and factor over that ring.

F1 = sub(F1, {t => t-2*b})
D = myResultant(diff(t,F1), F1)
     
use A
use R
time gcd(F, t+b^5+b^2)

{*
restart
loadPackage "UPolynomials"
*}
R1 = QQ[t]
a = adjoinRoot(t^8+t^3+1)
A = ring a;
B = A[t]; setUFD B
facs = myFactorization(t^8+t^3+1)
b = adjoinRoot(facs#1#1)
B = ring b
describe B

use B
C = B[t]; setUFD C
F = sub(t^8+t^3+1, t => t-2*b-3*a)
myResultant(F, diff(t,F))
use B
ker map(B, QQ[t], {2*a+b})
ker map(B, QQ[t], {a+2*b})

--restart
R1 = QQ[t,b,a]
I = ideal"a8+a3+1,b8+b3+1"
time primaryDecomposition I
J = trim(I + oo_1)
J = J + ideal"t8+t3+1"
L = trim sub(J, {t=>t-3*a-7*b})
gbTrace=3
L_0
L_1
L_2
resultant(L_0,L_2,b)
eliminate(L, {b})

--time primaryDecomposition J

gens gb L;
see ideal oo

R2 = ZZ/32003[a,b,t,MonomialOrder=>{2,1}]
L2 = sub(L,R2)
gbTrace=3
g2 = ideal gens gb L2
see g2
g2_0
factor g2_0 
F = (selectInSubring(1,gens gb L2))_(0,0)
factor F
--time decompose L

Rrevlex = ZZ/32003[a,b,t]
La = ideal gens gb sub(trim(L2 + ideal o31_2),Rrevlex)
ideal gens gb sub(La,R1)
///
end

-- minimal polynomial
-- primitive element
-- or better: find norm in this non-primitive extension field...

A = toField(QQ[a]/(a^2-a+1))
B = A[t]
F = t^5+2*a*t^3+2*a*t^2+(a+3)*t-2*a+2
myFactorization F
factor F -- wrong
C = makeTower B
G = sub(F,C)
myFactorization G

doc ///
Key
Headline
Inputs
Outputs
Consequences
Description
  Text
  Example
Caveat
SeeAlso
///

end

restart
loadPackage "UPolynomials"
installPackage oo
check UPolynomials
uninstallPackage "UPolynomials"

KK = toField(QQ[a]/(a^2-a+1))
R = KK[t]
F = t^5+2*a*t^3+2*a*t^2+(a+3)*t-2*a+2
myFactorization F
factorization F
myFactor F
G = (2*a+4)*t^2+(-12*a+8)*t+a-9
factorization G
myFactor G
myFactor F
myFactor (13*G) -- BUG!!  misses the 13...!
