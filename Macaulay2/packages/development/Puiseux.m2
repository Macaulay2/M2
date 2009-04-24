-- -*- coding: utf-8 -*-
newPackage(
	"Puiseux",
    	Version => "0.1", 
    	Date => "March 23, 2009",
    	Authors => {},
    	Headline => "Computing Puiseux expansions of plane curves",
    	DebuggingMode => true
    	)

export {NegativeSlopeOnly, polygon, slope}

polynomialGCD = (f,g) -> (gens gb ideal(f,g))_(0,0)

deg = (f) -> if f == 0 then -1 else (degree f)#0

squarefree = (f) -> (
     R := ring f;
     p := char R;
     x := R_0;
     result := {};
     T := polynomialGCD(f,diff(x,f));
     V := f//T;
     k := 0;
     while deg(V) =!= 0 do (
	  k = k+1;
	  W := polynomialGCD(T,V);
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

adjoinRoot = (f) -> (
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

slope = (pts,i,j) -> (pts#j#1 - pts#i#1)/(pts#j#0 - pts#i#0)

transformPolynomial = (F, tm, ell) -> (
     R := ring F;
     Rnew := R;
     (q,mu,m,beta) := tm;
     if ring beta =!= coefficientRing R then
	  Rnew = (ring beta)[x,y];
     coeffvars := drop(gens ring beta, numgens coefficientRing Rnew - numgens coefficientRing R);
     toRnew = map(Rnew,R,join({Rnew_0^q,Rnew_0^m*(beta+Rnew_1)},coeffvars));
     F = (toRnew F) // Rnew_0^ell
     )

termsToSeries = (tms) -> (
     -- tms is a list of (q,u,m,b)
     -- this returns the corresponding truncated power series,
     -- where t^q = x
     truncdegree := 1 + sum apply(tms, tm -> tm#2);
     K1 := ring(tms_0_3);
     Rtrunc = K1[t, MonomialOrder=>RevLex, Global=>false]/(t^truncdegree);
     t := Rtrunc_0;
     lastm := 0;
     sum apply(#tms, i -> (
	       lastm = lastm + tms#i#2;
	       tms#i#3 * t^(lastm)
	       )
     ))

polygon = method(Options=>{NegativeSlopeOnly => false})
polygon RingElement := opts -> (F) -> (
     -- F should be in a polynomial ring with 2 variables (over something else)
     -- find the lower lines for the Newton polygon of F.
     minpairs = sort apply(pairs partition(x -> x#1, exponents F), (k,v) -> (k, min apply(v, first)));
     lastpoint = 0;     
     ylo = (position(minpairs, x -> x#1 == 0));
     while lastpoint < ylo list (
       slopes = for i from lastpoint+1 to ylo list slope(minpairs,lastpoint,i);
       m := min slopes;
       thisedge = apply(positions(slopes, x -> x == m), y -> y+lastpoint+1);
       result := prepend(minpairs_lastpoint,minpairs_thisedge);
       lastpoint = max thisedge;
       result)
  )


edgesToInfo = method()
edgesToInfo(List, RingElement, RingElement) := (elist,F,x) -> (
     -- elist should be the output of 'polygon'
     -- x should be a variable in a ring, this will be the variable used
     --  to create the edge polynomials
     R := ring F;
     S := ring x;
     apply(elist, e -> (
	    y0 := e#1#0 - e#0#0;
	    x0 := e#1#1 - e#0#1;
	    g := gcd(y0,x0);
	    q := -x0//g;
	    m := y0//g;
	    ell := q * e#0#0 + m * e#0#1;
	    i0 := e#-1#1;
	    xpowers = apply(e, ji -> (ji#1-i0) // q);
	    G = sum apply(e, ji -> (
		      d := (ji#1-i0) // q;
		      coefficient(R_{ji#1,ji#0},F) * x^d
		      ));
	    (m, q, ell, G)
	    ))
  )

regularPart = method()
regularPart(RingElement, ZZ) := (F, nterms) -> (
     R := ring F;
     x := R_0;
     y := R_1;
     for i from 0 to nterms list (
	  m := min apply(terms sub(F, {y => 0}), g -> (first exponents g)_0);
	  c1 := coefficient(x^m, F);
	  c2 := coefficient(y, F);
	  time b := -c1/c2;
	  ans := (1, 1, m, b);
	  time F = sub(F, {y => x^m*(b+y)}) // x^m;
	  --<< "new F = " << F << endl << endl;
	  ans
	  )
     )

singularPart1 = (F) -> (
     -- Return: a set of ((q,mu,m,beta),ell,r)
     --   the mu and beta are in either K = coefficientRing ring F, or in an extension field of K.
     K := coefficientRing ring F;
     Kt := K[t];
     E := edgesToInfo(polygon F, F, Kt_0);
     flatten apply(E, e -> (
	       (q,m,ell,g) := e;
	       sq := squarefree g; -- squarefree decomp of g(t)
	       -- we now need to add in a root for each poly in sq (if the degree is > 1)
     	       << "e = " << e << endl;
	       apply(sq, (r, f) -> (
		 a := adjoinRoot f; -- doesn't adjoin a root if f is linear
		 if ring a =!= K then A = (ring a)[t] else A = Kt;
		 b := if q == 1 then a else adjoinRoot(A_0^q-a);
		 K1 := ring b;
	         ((q,1_K1,m,b), ell, r)
	       ))))
     )

singularParts = method()
singularParts(RingElement, List) := (F,L) -> (
     -- L is a list of quadruples (q,mu,m,beta), which tell how to reconstruct the Puiseux series so far
     --   On the first call, L is usually {}.
     -- F is the result of having applied these to the original F.
     L1 := singularPart1 F;
     splice \ join apply(L1, v -> (
	       (t,ell,r) := v;
	       L' := append(L,t);
	       F1 := transformPolynomial(F,t,ell);
	       if r == 1 then {(L',F1)}
	       else singularParts(F1,L')
	       ))
     )

puiseux = method()
puiseux(RingElement, ZZ) := (F, deglimit) -> (
     -- first find the singular parts.  Each knows what denominator is being used.
     -- then compute the regular part of each, up to the degree bound.
     -- NOTE: for integral bases, we will, from the singular parts, compute the bounds needed.
     L := singularParts(F,{});
     apply(L, s -> (tms := regularPart(s_1,deglimit); termsToSeries join(s_0,tms)))
     )
beginDocumentation()

end
restart
load "development/Puiseux.m2"
debug Puiseux
kk = ZZ/32003
A1 = kk[a]/(a^2-2)
A2 = A1[b]/(b^2-a)
toField A2
R = A2[y,x]
F = y^3-(a+1)*y^2-a*x*y-b*x^4
discriminant(F,y)
factor oo
value oo
F1 = sub(F, {x=>a*x, y=>x*(1-y)})

F = poly"y16-4y12x6-4y11x8+y10x10+6y8x12+8y7x14+14y6x16+4y5x18+y4(x20-4x18)-4y3x20+y2x22+x24"

R = QQ[x,y]
F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15
polygon F
edgesToInfo(oo,F,x)

K1 = QQ[a,b]/(a^2+1, b^2+b+1)
toField K1
R1 = K1[x,y]

time sub(F, {R_0 => x, R_1 => x^6*(a+y)}) // x^15
time tms = regularPart(oo,6)
y1 = termsToSeries prepend((1,1,6,a),tms)
y1 = sub(y1, t=>t^3)

time sub(F, {R_0 => x, R_1 => x^6*(-a+y)}) // x^15
time tms = regularPart(oo,6)
y2 = termsToSeries prepend((1,1,6,-a),tms)
y2 = sub(y2, t=>t^3)

time sub(F, {R_0 => x, R_1 => x*(a+y)}) // x^5
time tms = regularPart(oo,6)
y3 = termsToSeries prepend((1,1,1,a),tms)
y3 = sub(y3, t=>t^3)

time sub(F, {R_0 => x, R_1 => x*(-a+y)}) // x^5
time tms = regularPart(oo,6)
y4 = termsToSeries prepend((1,1,1,-a),tms)
y4 = sub(y4, t=>t^3)

time sub(F, {R_0 => x^3, R_1 => x*(-1+y)}) // x^7
time tms = regularPart(oo,6)
y5 = termsToSeries prepend((3,1,1,-1),tms)

time sub(F, {R_0 => x^3, R_1 => x*(-b+y)}) // x^7
time tms = regularPart(oo,6)
y6 = termsToSeries prepend((3,1,1,-b),tms)

time sub(F, {R_0 => x^3, R_1 => x*(b+1+y)}) // x^7
time tms = regularPart(oo,6)
y7 = termsToSeries prepend((3,1,1,b+1),tms)

K1 = QQ[a,b]/(a^2+1, b^2+b+1)
toField K1
R1 = K1[x,y]
1/a
1/b
1/(a*b)

S = Rtrunc[y,x, MonomialOrder=>Lex]
(y - sub(y1,S)) * (y-sub(y2,S)) * (y - sub(y3,S)) * (y-sub(y4,S))
(y - sub(y5,S)) * (y-sub(y6,S)) * (y-sub(y7,S))
     
use ring F
sub(F, {x=>t, y=>o14})
R = QQ[x,y]
F = y^4-y^2+x^3+x^4
polygon F
edgesToInfo(oo,F,x)

f1 = sub(F, {x=>x^2, y => x^3*(1+y)}) // x^6

time tms = regularPart(f1,20)
tms = prepend((2,1,3,1),tms)
termsToSeries(tms)
sub(F, {x=>t^2, y => oo})

R43 = QQ[x]/(x^43)
y1 = sum apply(#tms, i -> tms_i#3 * x^(2+2*i))

map(R43, R, {R43_0, y1})
oo f1
f2 = sub(F, {x=>x^2, y => x^3*(-1+y)}) // x^6
f11 = sub(f1, {y=>1/2 * x^2*(1+y)}) // x^2

sub(F, {x=>x^2, y => x^3 + 1/2 * x^5 + x^5*y}) // x^8
sub(F, {x=>x^2, y => x^3 + 1/2 * x^5 - 1/8*x^7 + 9/16*x^9 +  x^9*y}) // x^12

sub(F, {x=>x^2, y => -x^3 + x^3*y})  // x^6
sub(F, {x=>x^2, y => -x^3 - 1/2 * x^5 + x^5*y})  // x^8
sub(F, {x=>x^2, y => -x^3 + 1/2 * x^5 + 1/8*x^7 + x^7 * y})  


minpairs = apply(pairs partition(x -> x#1, polygon F), (k,v) -> (k, min apply(v, first)))



minpairs#0
ylo = (position(minpairs, x -> x#1 == 0))

lastpoint = 0;
slopes = for i from 1 to ylo-1 list slope(minpairs,lastpoint,i)
m = min slopes
thisedge = positions(slopes, x -> x == m)
lastpoint = max thisedge

min for i from 1 to ylo-1 list slope(minpairs,lastpoint,i)
     s := slope(lastpoint, i);
     if s > lastslope then (
	  
	  );
     )


restart
load "development/Puiseux.m2"
debug Puiseux
load "hermite.m2"
S = QQ[y,x,MonomialOrder=>Lex]
F = poly"y4-y2+x3+x4"

singularParts(F,{})

possibleDenominators(F,y)
K1 = QQ[a]/(a^4+2*a^3-4)
toField K1
S1 = K1 (monoid S)
G = sub(F, {S_1 => x - 2*a, S_0 => y})
S2 = K1[x,y]
G = sub(G,S2)
polygon G
edgesToInfo(oo,F,x)

restart
load "development/Puiseux.m2"
debug Puiseux
load "hermite.m2"

R = QQ[x,y]
F = y^4-y^2+x^3+x^4
polygon F
edgesToInfo(oo,F,x)
singularPart1 F
singularParts(F,{})
netList oo
adjoinRoot(t-3)

R = QQ[t]
a = adjoinRoot(t^2-3)

R1 = (ring a)[t]
b = adjoinRoot(t^3-a-3)

R2 = (ring b)[t]
c = adjoinRoot(t^2-a*t-b)

1/c
oo * c

-------------------------------------------
TEST ///
restart
load "development/Puiseux.m2"
debug Puiseux
R = QQ[x,y]
F = x^3*y^2 + x*y^4 + x^2*y^4 + y^7 + x^12*y + x^15
polygon F
edgesToInfo(oo,F,x)
L = singularParts(F, {})
netList oo
regularPart(L_0_1,5)
termsToSeries oo
puiseux(F,10)
///
-------------------------------------------