-- taking code mostly from local/classes/math731-factoring

newPackage(
        "UPolynomials",
        Version => "0.1", 
        Date => "",
        Authors => {{Name => "", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "",
        DebuggingMode => true
        )

-- We consider univariate polynomials over:
-- (a) finite fields
-- (b) ZZ or QQ
-- (c) number fields (given as a polynomial ring in a lex order)


-- For now: assume the ring is of the form K[x]
-- where K is a field.

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
     primitivePolynomialGCD,
     subresultantGCD,
     setUFD,
     makeTower,
     deg,
     squarefree, 
     adjoinRoot}

-- polynomials.m2
-- This package consists of routines for computing
-- division, gcd's, resultants, and factorization.

myIsDivisibleBy = (f,g) -> (ring f).myIsDivisibleBy(f,g)

myExactDivision = (f,g) -> (ring f).myExactDivision(f,g)

-- Only keep one of these two versions...
myGCD = args -> (
     R := ring (args#0);
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

TEST ///
loadPackage "UPolynomials"
assert not myIsDivisibleBy(13,3) 
assert(myGCD(12,15,21) == 3)
assert(myExactDivision(137*23213123200,137*10) == 2321312320)
///

GCD = (f,g) -> (gens gb ideal(f,g))_(0,0)

deg = (f) -> if f == 0 then -1 else (degree f)#0

myCoefficients = (f) -> (
     if f == 0 then {} else (
     	  d := deg f;
     	  x := (ring f)_0;
     	  apply(d+1, i -> f_(x^i))))

myContent = (f) -> if f == 0 or isField ring f then 1_(coefficientRing ring f) else myGCD myCoefficients f

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
     if isPolynomialRing R then (
     	  R.myIsDivisibleBy = myPolynomialIsDivBy;
     	  R.myExactDivision = myPolynomialExactDivision;
     	  R.myGCD = primitivePolynomialGCD;
     	  R.myGCD = subresultantGCD;	  
	  )
     else if isField R then (
     	  R.myIsDivisibleBy = ZZ.myIsDivisibleBy;
     	  R.myExactDivision = ZZ.myExactDivision;
     	  R.myGCD = (f,g) -> if f == 0 and g == 0 then 0_R else 1_R;
	  );
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

TEST ///
restart
loadPackage "UPolynomials"
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

time subresultantGCD(f,g)
time myGCD(f,g)
substitute(oo,R)  -- This should work...
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

TEST ///
restart
loadPackage "UPolynomials"
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

TEST ///
restart
loadPackage "UPolynomials"

R = ZZ[x]
setUFD R
g = (5*x-2)*(3*x^2-13)
f = (2*x^2+3*x+17)*g
h = (3*x^3-x^2-12)*g
time primitivePolynomialGCD(f,h) == g
time subresultantGCD(f,h) == g

use R
S = R[y]
setUFD S
F = (3*x^2-y^2-13*x*y)^5*(x+y-1)^3
G = (3*x^2+y^2-13*x*y)^5*(x+y-1)^5
time subresultantGCD(F,G)
time primitivePolynomialGCD(F,G)
S1 = flattenRing S
S1 = S1_0
F = sub(F,S1)
G = sub(G,S1)
time gcd(F,G) -- still much faster...
///

myResultant = (F,G) -> (
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

squarefree = (f) -> (
     R := ring f;
     p := char R;
     x := R_0;
     result := {};
     T := GCD(f,diff(x,f));
     V := f//T;
     k := 0;
     while deg(V) =!= 0 do (
	  k = k+1;
	  W := GCD(T,V);
	  Ak := V//W;
	  V = W;
	  T = T//V;
	  if deg(Ak) =!= 0 then 
	      result = append(result,(k,Ak));
	  );
     if deg T != 0 then (
	  -- we have a polynomial in x^p
	  result2 := squarefree lowerP(T);
	  result2 = apply(result2, a -> (p*a#0, a#1));
	  result = join(result,result2);
	  );
     result
     );

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
     I := ideal K;
     n := numgens K;
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
	  if numgens K == 0 then (
	       K1top = K[vars 0];
	       K1 = K1top/sub(f, t => K1top_0);
	       toField K1;
	       K1_0
	       )
	  else (
	    K1top = (coefficientRing K)[vars n, gens K, MonomialOrder=>Lex];
	    to1 := map(K1top, ring I, drop(gens K1top,1));
	    to2 := map(K1top, R, gens K1top);
	    J := ideal to2 f + to1 I;
	    K1 = K1top/J;
	    toField K1;
	    K1_0
	  )
     ))


beginDocumentation()

doc ///
Key
  UPolynomials
Headline
  univariate polynomial operations implemented at top level in Macaulay2
Description
  Text
  Example
Caveat
SeeAlso
///

end

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

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
R = ZZ[x]
g = (5*x-2)*(3*x^2-13)
f = (2*x^2+3*x+17)*g
assert(myExactDivision(f,g) == 2*x^2+3*x+17)
S = R[y]
setUFD S
h = (y^3-x*y-13)^2 * (y-x) * f
assert(myPolynomialExactDivision(h,y-x) == (y^3-x*y-13)^2 *  f)
assert(myPolynomialExactDivision(h,f*1_S) == (y^3-x*y-13)^2 * (y-x))
assert(myPolynomialExactDivision(h,f) == (y^3-x*y-13)^2 * (y-x))
assert(not myPolynomialIsDivBy(h+1_S,f*1_S))
assert(myPolynomialIsDivBy(h,f))
assert(not myPolynomialIsDivBy(h+1_S,f))

///

end
restart
path = prepend("~/src/M2/Macaulay2/packages/development", path)
loadPackage "UPolynomials"

R = QQ[t];
a = adjoinRoot(t^2+t+1)
A = ring a
a^2+a+1
S = A[t];
b = adjoinRoot(t^3-a)
b^6
b^-1
b * b^-1 == 1

B = ring b
S = B[t]
F = (b*t^4-a*t-1)*(a*t^3-b*t-1)
myPolynomialExactDivision(F, a*t^3-b*t-1)

-- division over this ring
-- gcd over this ring
-- evaluation
-- norm
-- would like multivariate factorization too...!