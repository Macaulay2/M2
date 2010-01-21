-- taking code mostly from local/classes/math731-factoring
-- and also UPolynomials.m2
-- However, the intent is to implement modular gcd over extension fields
-- of QQ, and/or function fields.
-- It should work too for extension field towers of ZZ/p or GF(q).

newPackage(
        "UPolynomials2",
    	Version => "0.1", 
    	Date => "15 Jan 2010",
        Authors => {{Name => "Mike Stillman", 
                  Email => "mike@math.cornell.edu", 
                  HomePage => "http://www.math.cornell.edu/~mike"}},
        Headline => "gcds and factorization of univariate polynomials",
        DebuggingMode => true
        )

export {
     deg,
     UPolyRing,
     UPoly,
     upoly,
     upolyRing,
     tower,
     towerP,
     useTower,
     varNames,
     extensions,
     integerContent,
     makePrimitive,
     makeMonic,
     reduceModM
     }

tower = method()
tower(Ring, List) := (kk,L) -> if #L == 0 then kk else (tower(kk, drop(L,1)) [L#0, Join=>false])
tower(Ring, List, List) := (kk,L, extensions) -> (
     if #L == 0 then kk else (
	  L1 := drop(L, 1);
	  ext1 := drop(extensions,1);
	  R1 := (tower(kk, L1, ext1)) [L#0, Join=>false];
	  if extensions#0 === null 
	  then R1
	  else R1/(sub(extensions#0, R1))))

isPolyRing = method()
isPolyRing Ring := (R) ->
     isPolynomialRing R or (isQuotientRing R and numgens R > 0)

useTower = method()
useTower Ring := (R) -> (
     if isPolyRing R then (
	  useTower(coefficientRing R);
	  use R;
	  ))

varNames = method()
varNames Ring := (R) -> (
     if isPolyRing R 
     then join(gens R, varNames coefficientRing R)
     else {}
     )

deg = method()
deg RingElement := (f) -> degree((ring f)_0, f)

towerP = method()
towerP(ZZ, Ring) := (p,R) -> tower(ZZ/p, varNames R)

extensions = method()
extensions Ring := (R) -> (
     -- returns a list of polynomials and/or nulls
     I := ideal R;
     if not isPolynomialRing ring I 
     then {}
     else (
	  f := if I == 0 then null else I_0;
	  prepend(f, extensions coefficientRing ring I))
     )

-----------
-- Types --
-----------
UPolyRing = new Type of List
 -- {char, varnames, extensions}

UPolyList = new Type of List

UPoly = new Type of List
 -- {R:UPolyRing, array:UPolyList}

poly UPoly := (F) -> F#1
ring UPoly := (F) -> F#0
varNames UPolyRing := (R) -> R#1
char UPolyRing := (R) -> R#0
coefficientRing UPolyRing := (R) -> new UPolyRing from {R#0, drop(R#1,1), drop(R#2,1)}
------------------------------------------
-- Translation to/from usual ring types --
------------------------------------------

------------------
-- Functions -----
------------------

UPoly == UPoly := (F,G) -> F === G

degree UPoly := (F) -> (
     -- degree of F in the main variable of F
     #(poly F) - 1
     )

integerContent = method()
integerContent UPoly := (F) -> integerContent poly F
     -- Return the positive integer content of F (this is a rational number)
integerContent UPolyList := (f) -> (
     if #f == 0 then 0_QQ 
     else (
     	  a := integerContent(f#(#f-1));
	  for j from 0 to #f-2 do (
	       if f#j === null then continue;
	       b := integerContent f#j;
	       a = gcd(a,b);
	       );
	  a
	  )
     )
integerContent ZZ := (f) -> f
integerContent QQ := (f) -> f

multiplyByConstant = method()
multiplyByConstant(UPoly, QQ) := (F,c) -> upoly(multiplyByConstant(poly F, c), ring F)
multiplyByConstant(UPolyList, QQ) := (f, c) -> apply(f, t -> multiplyByConstant(t, c))
multiplyByConstant(Nothing, QQ) := (f,c) -> f
multiplyByConstant(QQ, QQ) := (f,c) -> f*c
multiplyByConstant(ZZ, QQ) := (f,c) -> f*c


isOne = method()
isOne(UPoly) := (F) -> isOne(poly F);
isOne(UPolyList) := (f) -> #f == 1 and isOne f#0
isOne(Nothing) := (f) -> false
isOne(QQ) := (f) -> f == 1
isOne(ZZ) := (f) -> f == 1

makePrimitive = method()
makePrimitive UPoly := (F) -> (
     -- returns (G, c), where c is a rational number, and G is F//c.
     c := integerContent F;
     (multiplyByConstant (F, 1/c), c)
     )

leadCoefficient UPoly := (F) -> upoly(leadCoefficient(poly F), coefficientRing ring F)
leadCoefficient UPolyList := (f) -> f#(#f-1)

makeMonic = method()
makeMonic UPoly := (F) -> (
     -- returns (G, c), where c is the lead coefficient of F, and G is F//c.
     c := leadCoefficient F;
     if isOne c
     then c 
     else (
	  c1 := invert c;
	  multiplyByCoefficient(c1, F)
     ))

compress UPolyList := (f) -> (
     if #f == 0 or f#(#f-1) =!= null then f
     else (
	  d := #f-1;
	  topdeg := -1;
	  for i from 0 to #f-1 do if f#(d-i) =!= null then (topdeg = d-i; break;);
	  new UPolyList from for i from 0 to topdeg list f#i
	  )
     )

reduceModM = method()
reduceModM(UPolyRing, ZZ) := (R,M) -> new UPolyRing from {M, R#1, apply(R#2, f -> reduceModM(f,M))}
reduceModM(UPoly, ZZ) := (F,M) -> upoly(reduceModM(poly F, M), reduceModM(ring F, M))
reduceModM(UPolyList, ZZ) := (f, M) -> compress apply(f, t -> reduceModM(t, M))
reduceModM(Nothing, ZZ) := (f,M) -> f
reduceModM(QQ, ZZ) := (f,M) -> (
     -- assumption: f is primitive?
     a := numerator f;
     b := denominator f;
     if b != 1 then error "reduceModM expected primitive polynomial";
     reduceModM(a, M)
     )
reduceModM(ZZ, ZZ) := (f,M) -> (c := f % M; if c == 0 then null else if c > M//2 then c-M else c)


liftToQQ = method()
liftToQQ UPoly := (F) -> (
     -- F should be a poly defined over ZZ/m, for some m
     -- returns a poly over QQ, or null
     error "not implemented yet"
     )

CRA = method()
CRA(UPoly, UPoly) := (F,G) -> (
     -- F is a polynomial mod m1
     -- G is a polynomial mod m2
     -- returns a polynomial H modulo m1*m2
     -- such that H == F mod m1, H == G mod m2.
     -- (here: m1, m2 are integers)
     error "not implemented yet"
     )

UPoly + UPoly := (F,G) -> (
     -- rewrite to use only upoly structure...
     upoly(relem F + relem G, ring F)
     )

UPoly - UPoly := (F,G) -> (
     -- rewrite to use only upoly structure...
     upoly(relem F - relem G, ring F)
     )

UPoly * UPoly := (F,G) -> (
     -- rewrite to use only upoly structure and reduction...
     upoly(relem F * relem G, ring F)
     )

divisionAlgorithm = method()
divisionAlgorithm(UPoly, UPoly) := (F,G) -> (
     -- char p only?
     -- assumption: G is monic, mod a prime p.
     -- returns: (Q, R), where F = Q*G + R,
     --  and deg R < deg F, or R == 0.
     -- (degree in the main variable of F, which should
     --  be the main variable in G too).
     error "not implemented yet"
     )

gcdCoefficients(UPoly, UPoly) := (F,G) -> (
     -- returns (gcd,U,V), where gcd = U*F + V*G
     -- or null
     error "not implemented yet"
     )
gcd(UPoly, UPoly) := (F,G) -> (
     -- returns the monic gcd of F and G, or null
     error "not implemented yet"
     )

trialDivision = method()
trialDivision(UPoly, UPoly) := (F,G) -> (
     -- returns F//G, if G divides F, else null
     error "not implemented yet"
     )

invert = method()
invert(UPoly) := (F) -> (
     -- works mod p only
     -- F should be an element of a (possible) field.
     -- returns 1/F, if it can be computed, else null
     error "not implemented yet"
     )

------------------------------------------------------
modularGCD = method()
modularGCD(UPoly, UPoly) := (F1, F2) -> (
     -- F1, F2 are in K[x], K of char 0, alg ext of QQ
     -- return the monic gcd G of F1 and F2.
     R := ring F1;
     n := 0;
     fibonacci := (1,1);
     nextprime := 0;
     degG := null; -- will be set below before being used
     m := null; -- will be the product of all the primes giving H.
     P := null; -- current prime
     G := null; -- the gcd as computed so far
     H := null; -- will be tentative gcd
     d := null; -- will be the degree of H
     D := null; -- latest char P gcd
     -- Step 1: remove integer content of F, G
     --  WRITE THIS!!
     -- Step 2: main loop:
     while true do (
	  P = listPrimes#nextprime;
	  nextprime = nextprime+1;
	  H = modPGCD(F1,F2,P);
	  if H === null then continue;
	  if n == fibonacci#1 then (
	    -- the test means: n is the next fibonacci number
	    fibonacci = (fibonacci#1, sum fibonacci);
	    h := liftToQQ G;
	    if h =!= null then (
		 -- we might be done:  we need to check div
		 -- (For Trager's alg, we can skip the expensive one of these)
	         if reduceModM(h, P) == H and trialDivision(H,F1) and trialDivision(H,F2)
		 then return H;
		 ));
	  d = degree H;
	  if d === 0 then return 1_R;
	  if n == 0 or d < degG then (
	       G = H;
	       degG = d; -- is mod P
	       n = 1;
	       )
	  else if d > degG then continue
	  else (
	       -- G is mod m
	       -- H is mod P
	       G = CRA(G,H);
	       -- now G is mod m*P
	       -- degG doesn't change
	       n = n+1;
	       );
	  );
     )
---------------------------------------------

upolyRing = method()
upolyRing(ZZ, List) := (p, L) -> new UPolyRing from {p,L,{},tower(ZZ/p,L)}
upolyRing Ring := (R) -> new UPolyRing from {char R, varNames A, extensions A, R}

upoly0 = (L, F) -> (
     if #L == 0 
     then F
     else if F == 0 then new UPolyList from {null}
     else (
       L1 := drop(L,1);
       x := L#0;
       d := degree(x,F);
       (mons, cfs) := coefficients(F, Variables=>{x});
       C := for i from 0 to d list (
	    c := coefficient(x^i, F);
	    if c == 0 then null else upoly0(L1, c)
	    );
       new UPolyList from if #L1 == 0 then flatten C else C
     ))

upoly = method()
upoly(RingElement, UPolyRing) := (F, R) -> new UPoly from {R, upoly0(R#1, F)}
  -- take an element from a tower ring, and make a UPoly from it
upoly(UPolyList, UPolyRing) := (F, R) -> new UPoly from {R, F}

relem = method()
relem(List, UPolyList) := (V, f) -> (
     V1 := drop(V,1);
     x := V#0;
     result := sum for i from 0 to #f-1 list (
	  if f#i === null then continue;
	  relem(V1, f#i) * x^i
	  );
     result
     )
relem(List, ZZ) := (V, f) -> f
relem(List, QQ) := (V, f) -> f
relem(UPoly) := (F) -> relem(varNames ring F, poly F)

net UPoly := (F) -> net relem F

beginDocumentation()


end

restart
debug loadPackage "UPolynomials2"

A = QQ[x,y,z]
R = tower(QQ, gens A)
R3 = tower(ZZ/3, gens A)
use A
x
useTower R
x
y
z
describe R
degree x
degree y
F = x^3-(x+y)^2-x-y^10+3+z^3-z^2
assert(degree_x F == 3)
assert(coefficient(x^2, F) == -1)
assert(coefficient(x, F) == -(2*y+1))

use A
F = x^3-(x+y)^2-x-y+3+z^3-z^2
G = sub(F,R)
useTower R
H = x^3-(x+y)^2-x-y+3+z^3-z^2
H == G

varNames R
useTower R
H = x^3-(x+y)^2-x-y+3+121*z^3-100*z^2
towerP(101, R)
sub(H,oo)

-- towers:
kk = QQ
A = kk[t]/(t^2-2)[s]/(s^3-s-t)[x]
varNames A
extensions A
tower(ZZ/101, varNames A, extensions A)

R = upolyRing A
useTower A
F = x^2 + (s+t)*x + s*t
upoly(F, R)
relem oo

G = F^3
G1 = upoly(G,R)
G2 = relem G1
assert(G == G2)

G = upoly(1_A,R)
assert(1_A == relem G)

G = upoly(0_A,R)
assert(0_A == relem G)
relem G

F = upoly(34*x^2 + 17/5*(s+t)*x + 85*s*t, R)
integerContent F
multiplyByConstant(F, 5/17)
makePrimitive F

TEST ///
restart
debug loadPackage "UPolynomials2"
kk = QQ
A = kk[t]/(t^2-2)[s]/(s^3-s-t)[x]
R = upolyRing A
F = x^2 + 1/2*(s+t)*x + s*t
G = upoly(3*F^3 + 6*F, R)
F = upoly(F, R)
(G1,c1) = makePrimitive G -- wrong
makePrimitive F 
assert(integerContent F == 1/2)
assert(integerContent G == 3/8)
assert(first makePrimitive F == upoly(2*x^2+(s+t)*x+2*t*s, R))
assert(relem leadCoefficient G1 == 8)
reduceModM(G1, 12)
///

TEST ///
restart
debug loadPackage "UPolynomials2"
kk = QQ
A = kk[t]/(t^2-2)[s]/(s^3-s-t)[x]
R = upolyRing A
F = x^2 + 1/2*(s+t)*x + s*t
G = upoly(3*F^3 + 6*F, R)
F = upoly(F, R)
F + F
F + G
F*G
///