-- What needs to be done: rework these examples so that
-- they test minimalPresentation of a ring.
-- The code in M2 incorporates this stuff, I believe.

-------------------------------------------
-- Code added in 1.1.1x -------------------
-------------------------------------------

reductorVariable = (f) -> (
     -- assumes all variables in ring have degree 1.
     -- 1 argument: a polynomial.
     -- return: a list of two elements.  The first element is a number
     -- and the second a monomial, that form a linear term in f such
     -- that the variable does not occur in any other term of f.  If
     -- no such term exists in f, it returns {}.
     inf := part(1,f);
     restf := set support(f-inf);
     supInf := set support(inf);
     varList := toList (supInf-restf);
     -- either varList = {} and there are no linear terms whose
     -- variables don't occur elsewhere, otherwise we found such
     -- linear terms. 
     if varList === {} then varList
     else (
     	  termf := terms f;
     	  s := select(termf, i -> member(leadMonomial i , varList));
     	  coef := s/leadCoefficient;
     	  pos := position(coef, i -> (i == 1) or (i == -1));
	  -- best to choose linear terms with coefficient 1 or -1 if
	  -- possible. 
     	  if pos =!= null then (coef_pos, leadMonomial s_pos)
     	  else {coef_0, leadMonomial s_0}
     	  )
     )
     
findReductor = (L) -> (
     -- 1 argument: A list of polynomials, generally formed from
     -- generators of an ideal. 
     -- Returns: a pair consisting of a variable x and x - 1/c f where
     -- cx is a linear term of f such that x does not occur in any
     -- other term of f. 
     --
     -- Sorts through polynomials in the list to find the smallest one
     -- to use first to find a variable to use to find a "smaller
     -- ring" in minimalPresentation. 
     L1 := sort apply(L, f -> (size f,f));
     redVar := {};
     L2 := select(1, L1, p -> (
	       redVar = reductorVariable(p#1);
	       redVar =!= {})
	  );
     if redVar =!= {} then (redVar#1, redVar#1 - (1/(redVar#0))*L2#0#1)
     )
	       


reduceLinears = method(Options => {Limit=>infinity})
reduceLinears Ideal := o -> (I) -> (
     -- 1 argument: an ideal
     -- 1 optional argument: a limit on the recursion through the
     -- generators of the ideal.  
     -- Return: (J,L), where J is an ideal, and L is a list of:
     -- (variable x, poly x+g) where x+g is in I, and x doesn't appear
     -- in J and x does not appear in any polynomial later in the L list.
     R := ring I;
     L := flatten entries gens I;
     count := o.Limit;
     M := while count > 0 list (
       count = count - 1;
       g := findReductor L;
       if g === null then break;
       --<< "reducing using " << g#0 << endl << endl;
       F = map(R,R,{g#0 => g#1});
       L = apply(L, i -> F(i));
       g
       );
       -- Now loop through and improve M
     M = backSubstitute M;  
     (ideal L, M)
     )

backSubstitute = (M) -> (
     -- 1 argument: A list of pairs of variable and a polynomial. 
     -- Return: A list of pairs of the form (variable x, poly x+g)
     -- where x+g is in I, and x does not appear in any polynomial
     -- later. 
     --
     -- If M has length <= 1, then nothing needs to be done
     if #M <= 1 then M
     else (
     	  xs := set apply(M, i -> i#0);
     	  R := ring M#0#0;
     	  F := map(R,R, apply(M, g -> g#0 => g#1));
     	  H := new MutableHashTable from apply(M, g -> g#0 =>  g#1);
     	  scan(reverse M, g -> (
	       v := g#0;
	       restg := H#v;
	       badset := xs * set support restg;
	       if badset =!= set{} then (
		    H#v = F(restg))
	       ));
         pairs H)
     )

flattenQuotient = method()
flattenQuotient Ideal := (I) -> (
     -- returns triple (J,f,g)
     -- where if R = ring I, S = ring J, then
     -- (a) S is a flattened poly ring (not a quotient ring)
     -- (b) f : R --> S, g : S --> R
     --     and f, g induce isomorphisms of R/I with T/J.
     -- (c) T/J cannot be written as a polynomial ring in fewer variables
     --     (can we really get this?)
     -- Assumptions: R is not a Weyl algebra, but can be skew commutative
     --
     R := ring I;
     (S1,F) := flattenRing R;
     I1 := if S1 =!= R then F I else I;
     S := ambient S1;
     J := lift(I1,S); -- includes quotient ideal of S1, if any.
     f := map(R,S,generators(R, CoefficientRing => coefficientRing S));
     g := map(S,R,vars S);  -- ONLY well-defined R --> S/J !!
     (J,f,g)
     )

minPressy = method()

-- Modified version, 10/24/08
minPressy Ideal := (I) -> (
     --  Returns: an ideal J in a polynomial ring over ZZ or a base field
     --  such that (ring I)/I is isomorphic to  (ring J)/J.
     --  Approach: look at generators of I, find those with linear
     --  terms that don't use the linear variables in any other
     --  terms. Then map that variable to remainder. Collect mapping
     --  information and cache in the ideal (ring in the case of the
     --  ring call).  
     --
     --  if the ring I is a tower, flatten it first, and if the ring has quotient
     -- elements, add them to flatI
     R := ring I;
     (flatI,F,G) := flattenQuotient I;
     flatR := ring flatI;
     (S,IS) := (flatR, flatI);
     if any(degrees flatR, d -> d =!= {1}) then (
     	  -- reset all the degrees to 1's, to use our reductor algorithm
	  S = newRing(flatR, Degrees=>{(numgens flatR : 1)});
	  IS = sub(IS, vars S);
	  );
     (J,H) := reduceLinears IS;
     StoFlatR = map(flatR,S,vars flatR);
     J = ideal compress gens StoFlatR J; -- now in flatR
     H = apply(H, (a,b) -> (StoFlatR a, StoFlatR b)); -- everything in flatR now

     xs := set apply(H, index@@first); -- indices of the variables reduced out
     varskeep := sort (toList(set (gens S/index) - xs));
     newS := first selectVariables(varskeep, flatR);
     I.cache.minimalPresentationMapInv = map(R,newS,apply(varskeep, i -> R_i));

     vs := new HashTable from apply(#varskeep, i -> varskeep#i => i);
     trivialToNewS = map(newS, flatR, toList apply(numgens flatR, i -> if vs#?i then newS_(vs#i) else 0_newS));
     X := new MutableList from gens flatR; 
     scan(pairs vs, (v,i) -> X#v = newS_i);
     scan(H, (v,g) -> X#(index v) = trivialToNewS g);
     I.cache.minimalPresentationMap = map(newS, R, toList X);
     trivialToNewS J)

minimalPresentation Ideal := Ideal => opts -> (I) -> minPressy I

minimalPresentation Ring := Ring => opts -> (R) -> (
     -- 1 argument: A ring R (most often a quotient ring).
     -- Return:  A ring isomorphic to R using minimalPresentation on
     -- the presentation ideal of R.  For more information see
     -- minimalPresentation Ideal.
     I := ideal R;
     result := minPressy I;
     finalRing := (ring result)/result;
     -- put the maps cached on I in the right place for the ring. 
     f := substitute(matrix I.cache.minimalPresentationMap, finalRing);
     fInv := substitute(matrix I.cache.minimalPresentationMapInv, R);
     R.cache.minimalPresentationMap = map(finalRing, R, f);
     R.cache.minimalPresentationMapInv = map (R, finalRing, fInv);
     finalRing
     )

end

R = QQ[x, y, z]/(-z+x*y^2)
S = R [w_0, w_1, w_2, Degrees => {{1, 1}, {1, 1}, {1, 1}}, Heft => {0, 1}]
I = ideal(z*w_1-y*w_2,z*w_0-x*w_2,y*w_0-x*w_1,x*y*w_1-z^8*w_2,x*w_1^2-z^7*w_2^2,w_0*w_1^2-z^6*w_2^3)
T = S/I
I

viewHelp selectVariables
T = ambient first flattenRing S
describe T
(T0,F) = selectVariables({0,3,4},T)
describe T0
T1 = newRing(T0, Degrees=>{(numgens T0 : 1)})
describe T1
describe S
describe T
describe T0
describe T1

restart
load "/Users/mike/src/M2/Macaulay2/bugs/mike/minpressy.m2"
A = ZZ/101[a,b]/(a^3-b-1)
B = A[x,y,z]/(a*x-1, b*y^3-x-a)
I = ideal(0_B)
J = minPressy I
peek I.cache
----------------
A = ZZ[a,b]/(a^3-2*b-1)
B = A[x,y,z]/(a*x-1, b*y^3-x-a)
I = ideal(0_B)
J = minPressy I
peek I.cache
----------------





restart
load "/Users/mike/src/M2/Macaulay2/bugs/mike/minpressy.m2"

TEST ///
-- Test of flattenQuotient and flattenRing
A = ZZ/101[a,b]/(a^3-b-1)
B = A[x,y,z]/(a*x-1, b*y^3-x-a)
I = ideal(0_B)
(J,f,g) = flattenQuotient(ideal 0_B)
(C,h) = flattenRing B
assert(toString ambient C == toString ring J) -- if these fail, that could still be OK
assert(toString J ==toString ideal C)
assert(degrees ring J == degrees C)
assert((monoid ring J).Options.MonomialOrder == (monoid C).Options.MonomialOrder)
f*g == map(B,B)
g*f == map(ring J, ring J)

A = ZZ[a,b]/(a^3-3*b-1)
B = A[x,y,z]/(a*x-1, b*y^3-x-a)
I = ideal(0_B)
(J,f,g) = flattenQuotient(ideal 0_B)
(C,h) = flattenRing B
assert(toString ambient C == toString ring J) -- if these fail, that could still be OK
assert(toString J ==toString ideal C)
assert(degrees ring J == degrees C)
assert((monoid ring J).Options.MonomialOrder == (monoid C).Options.MonomialOrder)
assert(f*g == map(B,B))
g*f == map(ring J, ring J)
///

TEST ///
A = ZZ[a,b,c]/(a^3-3*b-1, c)
B = A[x,y,z]/(a*x-1, b*y^3-x-a)
I = ideal(0_B)
(J,f,g) = flattenQuotient(ideal 0_B)
(C,h) = flattenRing B
assert(toString ambient C == toString ring J) -- if these fail, that could still be OK
assert(toString J ==toString ideal C)
assert(degrees ring J == degrees C)
assert((monoid ring J).Options.MonomialOrder == (monoid C).Options.MonomialOrder)
assert(f*g == map(B,B))
--assert(g*f == map(ring J, ring J))
///

TEST ///
A = ZZ[a,b,c]/(a^3-3*b-1, a)
B = A[x,y,z]/(a*x-1, b*y^3-x-a)
I = ideal(0_B)
(J,f,g) = flattenQuotient(ideal 0_B)
(C,h) = flattenRing B
assert(toString ambient C == toString ring J) -- if these fail, that could still be OK
assert(toString J ==toString ideal C)
assert(degrees ring J == degrees C)
assert((monoid ring J).Options.MonomialOrder == (monoid C).Options.MonomialOrder)
assert(f*g == map(B,B))
assert (B == 0)
///

TEST ///
A = ZZ/101[a..e,SkewCommutative=>true]
B = A/(a*b-d*e)
C = B[x,y,z]
D = C/(a*x-b*y^2)
describe D
(E,f) = flattenRing D
toExternalString f
use E
b*a
describe E
toExternalString E
monoid E
///

TEST ///
-- Tests of new minimal presentation (of ring) code:
-- written by Amelia Taylor
A = ZZ/101[x,y]/(y-x^3-x^5-x^7)
B = minimalPresentation A
F = A.minimalPresentationMap
G = A.minimalPresentationMapInv
assert(G*F == map(A,A,gens A))
assert(F*G == map(B,B,gens B))
assert(ideal B == 0)
assert(numgens B == 1)

R = ZZ/101[x,y]
I = ideal(y-x^3-x^5-x^7)
J = minimalPresentation I
F = I.cache.minimalPresentationMap
G = I.cache.minimalPresentationMapInv
assert(numgens ring J == 1)
assert(J == 0)
assert(source F === ring I)
assert(target F === ring J)
assert(source G === ring J)
assert(target G === ring I)

A = QQ[x,y]/(y-x^3-x^5-x^7)
B = minimalPresentation A
F = A.minimalPresentationMap
G = A.minimalPresentationMapInv
assert(G*F == map(A,A,gens A))
assert(F*G == map(B,B,gens B))
assert(ideal B == 0)
assert(numgens B == 1)

A = QQ[x,y]/(2*y-x^3-x^5-x^7)
B = minimalPresentation A
F = A.minimalPresentationMap
G = A.minimalPresentationMapInv
assert(G*F == map(A,A,gens A))
assert(F*G == map(B,B,gens B))
assert(ideal B == 0)
assert(numgens B == 1)

R = QQ[x,y]
I = ideal(2*y-x^3-x^5-x^7)
J = minimalPresentation I
F = I.cache.minimalPresentationMap
G = I.cache.minimalPresentationMapInv
assert(numgens ring J == 1)
assert(J == 0)
assert(source F === ring I)
assert(target F === ring J)
assert(source G === ring J)
assert(target G === ring I)

A = ZZ[x,y,z]/(2*y+z-x^3-x^5-x^7, z^2)
B = minimalPresentation A
F = A.minimalPresentationMap
G = A.minimalPresentationMapInv
assert(G*F == map(A,A,gens A))
assert(F*G == map(B,B,gens B))
assert(numgens B == 2)

R = ZZ[x,y,z]
I = ideal(2*y+z-x^3-x^5-x^7, z^2)
J = minimalPresentation I
assert(numgens ring J == 2)
use ring J
assert(J == ideal"x14+2x12+3x10+2x8-4x7y+x6-4x5y-4x3y+4y2")
F = I.cache.minimalPresentationMap
G = I.cache.minimalPresentationMapInv
assert(numgens ring J == 2)
assert(source F === ring I)
assert(target F === ring J)
assert(source G === ring J)
assert(target G === ring I)

A = QQ[a,b,c]/(a^2-3*b,a*c-c^4*b)
I = ideal 0_A
flattenRing I
J = minimalPresentation I
F = I.cache.minimalPresentationMap
G = I.cache.minimalPresentationMapInv
assert(numgens ring J == 2)
assert(source F === ring I)
assert(target F === ring J)
assert(source G === ring J)
assert(target G === ring I)

A = QQ[a,b,c]/(a^2-3*b^2,a^3-c^4*b)
I = ideal 0_A
J = minimalPresentation I
F = I.cache.minimalPresentationMap
G = I.cache.minimalPresentationMapInv
assert(source F === ring I)
assert(target F === ring J)
assert(source G === ring J)
assert(target G === ring I)

A = ZZ/101[a,b]/(a^2+b^2)
B = A[c,d]/(a*c+b*d-1)
C = B[e,f]/(e^2-b-1)
I = ideal 0_C
J = minimalPresentation I
F = I.cache.minimalPresentationMap
G = I.cache.minimalPresentationMapInv
assert(source F === ring I)
assert(target F === ring J)
assert(source G === ring J)
assert(target G === ring I)

I = ideal presentation (flattenRing C)#0
J = minimalPresentation I
F = I.cache.minimalPresentationMap
G = I.cache.minimalPresentationMapInv
assert(source F === ring I)
assert(target F === ring J)
assert(source G === ring J)
assert(target G === ring I)
///

