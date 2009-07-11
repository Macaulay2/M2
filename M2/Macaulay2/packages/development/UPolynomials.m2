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
     mySquareFreeDecomposition,
     myFactorization,
     setUFD,
     makeTower,
     deg,
     adjoinRoot}

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

deg = (f) -> if f == 0 then -1 else (degree f)#0

myCoefficients = (f) -> (
     if f == 0 then {} else (
     	  d := deg f;
     	  x := (ring f)_0;
     	  apply(d+1, i -> f_(x^i))))

myContent = (f) -> if f == 0 or isField ring f then 1_(coefficientRing ring f) else myGCD myCoefficients f
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

myfac0 = (F, S, G, phis) -> (
     for k from 0 to #phis do (
	  -- we will return on the first one that works
	  (f,g) := phis#k;
	  H := myResultant(G, f F);
	  --H := resultant(G, f F, S_0);
	  --<< H << endl << endl;
	  if H != 1 and gcd(diff((ring H)_0,H),H) == 1 then (
	       -- yes! we are almost there...
	       facsH := factor H;
	       facsH = (toList facsH)/toList/first;
	       --<< "k = " << k << " gives factorization" << endl;
	       return apply(facsH, h -> myGCD(F, g h))
	       )
	  );
     error "wow -- the first 9 tries failed!";
     )

myFactorization = method()
myFactorization RingElement := (F) -> (
     R := ring F;
     S1 := QQ[t]; setUFD S1;
     S := S1[a]; setUFD S;
     phi0 := map(S,R,{S_1,S_0});
     phi0' := map(R,S,{R_1,R_0});
     IR := (ideal coefficientRing R)_0;
     i0 := map(S,ring IR,{S_0});
     G := i0 IR;
     phis := apply(1..8, k -> (
	       (map(S,R,{S_1-k*S_0,S_0}),
     		map(R,S,{R_1,R_0+k*R_1})
	       )));
     sqfree := mySquareFreeDecomposition F;
     -- set up S, G, and the various maps: R --> S, inverses S --> R.
     product flatten  apply(sqfree, (d,Fi) -> (
	       facs := myfac0(Fi,S,G, phis);
	       apply(facs, f -> (hold f)^d)
	       ))
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
	       setUFD K1;
	       K1_0
	       )
	  else (
	    K1top = (coefficientRing K)[vars n, gens K, MonomialOrder=>Lex];
	    to1 := map(K1top, ring I, drop(gens K1top,1));
	    to2 := map(K1top, R, gens K1top);
	    J := ideal to2 f + to1 I;
	    K1 = K1top/J;
	    toField K1;
	    setUFD K1;
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

TEST ///
-- Here we test gcd's and square free decomposition over algebraic number fields
R = QQ[t];
a = adjoinRoot(t^2+t+1)
A = ring a
S = A[t];
b = adjoinRoot(t^3-a)
B = ring b
eliminate(a,ideal B)

A = QQ[b]/(b^6+b^3+1)
toField A
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
G' = sub((ideal A)_0, R')
resultant(F',G',b)
factor oo
facs = oo//toList/toList/first
phi1 = map(R,R',{R_1, R_0 + 2*R_1})
apply(facs, f -> myGCD(phi1 f, F))

G' = sub((ideal A)_0, R')

resultant(F',G',t)
-- We want to factor F...


///

TEST ///
-- test of factorization over algebraic extensions of QQ
A = QQ[b]/(b^6+b^3+1); toField A; setUFD A
R = A[t]; setUFD R
F = t^6+t^3+1
time myFactorization F


A = QQ[b]/(b^8+b^3+1)
toField A
setUFD A

R = A[t]
setUFD R
F = t^8+t^3+1
time fac = myFactorization F
use A; use R
myExactDivision(F,t-b)
-- TODO: adjoin a root of this, and factor over that ring.

restart
loadPackage "UPolynomials"
R1 = QQ[t]
a = adjoinRoot(t^8+t^3+1)
A = ring a;
B = A[t]; setUFD B
facs = myFactorization(t^8+t^3+1)
b = adjoinRoot(facs#1)
B = ring b
describe B

use B
ker map(B, QQ[t], {2*a+b})
ker map(B, QQ[t], {a+2*b})
///
end

-- minimal polynomial
-- primitive element
-- or better: find norm in this non-primitive extension field...


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

