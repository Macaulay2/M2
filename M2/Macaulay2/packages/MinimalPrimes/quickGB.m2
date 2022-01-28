-- Input  : an ideal and a subset of variables
-- Output : A GB of I in a block order, where the fiberVars are bigger than any variable in the complement
--          (the independentVars) and the fiberVars block is in Lex order.  independentVars is done in GRevLex order (it will be an option later)
--quickGB = method()
--quickGB Ideal := I -> (
--  R := ring I;
--  monOrder := (options monoid R).MonomialOrder;
--  monOrder = drop(take(monOrder,#monOrder-1),1);
--  if first toList first monOrder === Lex then computeLexGB I else gb I
--)

--containsLexBlock Ring := R -> containsLexBlock monoid R
--containsLexBlock Monoid := M -> (
--  monOrder := options M
--)

MYTEST = str -> null

MYTEST ///
restart
load "quickGB.m2"
R = QQ[a,b,c,d,e,MonomialOrder=>{Lex=>2,GRevLex=>3}]
I = ideal gens R
computeGB I
///

computeGB = method()
computeGB Ideal := (I) -> (
  T := symbol T;
  R := ring I;
  gensR := gens R;  
  monOrder := toList (options monoid R).MonomialOrder;
  numLexGens := (toList(monOrder#1))#1;
  
  -- define the rings and ring maps that we need to perform the computation.  
  grevLexRT := (coefficientRing R) (monoid [gensR,T, MonomialOrder=>GRevLex]);
  homogR := (coefficientRing R) (monoid [gensR,T, MonomialOrder=>monOrder | {GRevLex=>1}]);
  phi1 := map(grevLexRT,R);
  phi2 := map(homogR,grevLexRT);
  phi3 := (R,homogR);
  phiDeHomog := map(R,homogR, matrix{gens R | {1}});
  
  -- move around and compute the GBs
  grevLexGB := gens gb phi1 I;
  grevLexHomogGB := homogenize(grevLexGB, sub(homogR_(numgens homogR-1),grevLexRT));
  hilbI := poincare ideal grevLexHomogGB;
  homogFastI := phi2 grevLexHomogGB;
  homogFastGB := gens gb(homogFastI,Hilbert=>hilbI);
  fastGB := phiDeHomog homogFastGB;
  fastGB = forceGB sub(fastGB,R);
  fastGB
)

-- just a function to get the indices of the variables
-- we are eliminating
getIndices = (R,v) -> unique apply(v, index)

isFlatPolynomialRing := (R) -> (
  -- R should be a ring
  -- determines if R is a poly ring over ZZ or a field
  kk := coefficientRing R;
  isPolynomialRing R and (kk === ZZ or isField kk)
)

eliminationRing = (elimvars, R) -> (
     -- input: R: flat polynomial ring
     --        elimvars: list of integer indices of vars to eliminate
     --        homog:Boolean: whether to add another variable for homogenization
     -- output: (F:R-->S, G:S-->R), where S is the new ring
     -- S is the same as R, except that the variables have been permuted, the 
     -- names of the variables are private, and the monomial ordering is an elim order.
     -- If R is a WeylAlgebra, homogenized Weyl algebra, skew commutative ring, or poly
     -- ring, then S will be the same, with the correct multiplication and grading
     keepvars := sort toList(set(0..numgens R-1) - set elimvars);
     perm := join(elimvars,keepvars);
     invperm := inversePermutation perm;
     vars := (options R).Variables;
     degs := (options R).Degrees;
     weyl := (options R).WeylAlgebra;
     skew := (options R).SkewCommutative;
     degs = degs_perm;
     vars = vars_perm;
     M := monoid [vars,MonomialOrder=>Eliminate(#elimvars), Degrees=>degs, 
	  WeylAlgebra => weyl, SkewCommutative => skew, MonomialSize=>16];
     k := coefficientRing R;
     R1 := k M;
     toR1 := map(R1,R,apply(invperm,i->R1_i));
     toR := map(R,R1,apply(perm,i->R_i));
     (toR1,toR)
     )

quickEliminate = method()
quickEliminate (List,Ideal) := (v,I) -> (
   R := ring I;
   -- if R is a quotient ring, then give error
   if not isFlatPolynomialRing R then
     error "expected a polynomial ring over ZZ or a field";
   if #v === 0 then return I;
   if not all(v, x -> class x === R) then error "expected a list of elements in the ring of the ideal";
   varlist := getIndices(ring I,v);
   quickEliminate1(varlist, I)
)

quickEliminate (Ideal,List) := (I,v) -> quickEliminate(v,I)

quickEliminate1 = (elimindices,I) -> (
   -- helper function to quickEliminate
   -- at this point, I is an ideal in a flat ring, 
   -- and elimindices represents the variables
   -- to eliminate.
   (toR1,toR) := eliminationRing(elimindices,ring I);
   J := toR1 I;
   -- cache poincare
   if isHomogeneous I then poincare J = poincare I;
   -- compare with eliminate1 from Elimination
   ideal mingens ideal toR selectInSubring(1,generators computeGB J)
)

MYTEST ///
restart
load "quickGB.m2"
R = QQ(monoid[e_1, e_2, e_3, g_2, g_3, g_4, r, e_4, g_1, Degrees => {9:1}, Heft => {1}, MonomialOrder => VerticalList{MonomialSize => 32, Lex => 9, Position => Up}, DegreeRank => 1])
L = ideal(g_2^2+20*g_2*g_4+2*g_2*r+100*g_4^2+20*g_4*r+r^2-3,9*g_4^4+12*g_4^2*e_4^2+9*g_4^2*g_1^2+4*e_4^4+18*e_4^2*g_1^2,32*g_2^3*e_4*g_1+24*g_2^2*g_4^3+16*g_2^2*g_4*e_4^2+24*g_2^2*g_4*g_1^2+36*g_2*g_4^2*e_4*g_1+8*g_2*e_4^3*g_1+36*g_2*e_4*g_1^3-12*g_4^3*e_4^2+27*g_4^3*g_1^2-8*g_4*e_4^4-30*g_4*e_4^2*g_1^2+27*g_4*g_1^4)
gbTrace = 3
-- this takes ~70 seconds!  Can we make it even faster with modular gbs?
time quickEliminate({g_2,g_4},L);
-- this takes about 30 minutes!
time eliminate({g_2,g_4},L);
///

MYTEST ///
-- ordinary lex
R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l, MonomialOrder=>Lex]
I = ideal(h*j*l-2*e*g+16001*c*j+16001*a*l,h*j*k-2*e*f+16001*b*j+16001*a*k,h*j^2+2*e^2+16001*a*j,d*j^2+2*a*e,g*h*j+e*h*l+8001*d*j*l+16001*c*e+16001*a*g,f*h*j+e*h*k+8001*d*j*k+16001*b*e+16001*a*f
          ,e*g*j+8001*c*j^2+e^2*l,d*g*j+d*e*l+16001*a*c,e*f*j+8001*b*j^2+e^2*k,d*f*j+d*e*k+16001*a*b,d*e*j-a*h*j-16001*a^2,d*e^2-a*e*h-8001*a*d*j,d*g*k*l-c*h*k*l-d*f*l^2+b*h*l^2-2*c*f*g+2*b*g^2-16001
       	  *c^2*k+16001*b*c*l,d*g*k^2-c*h*k^2-d*f*k*l+b*h*k*l-2*c*f^2+2*b*f*g-16001*b*c*k+16001*b^2*l,d*g^2*k-c*g*h*k-d*f*g*l+c*f*h*l-8001*c*d*k*l+8001*b*d*l^2+16001*c^2*f-16001*b*c*g,d*f*g*k-b*g*h*k-
       	  8001*c*d*k^2-d*f^2*l+b*f*h*l+8001*b*d*k*l+16001*b*c*f-16001*b^2*g,c*f*g*k-b*g^2*k-8001*c^2*k^2-c*f^2*l+b*f*g*l-16001*b*c*k*l-8001*b^2*l^2,e^2*g*k+8001*c*e*j*k-e^2*f*l-8001*b*e*j*l,d*g*h*l^2
       	  -c*h^2*l^2-8001*d^2*l^3+2*d*g^3-2*c*g^2*h+16000*c*d*g*l+c^2*h*l-8001*c^3,d*f*h*l^2-b*h^2*l^2-8001*d^2*k*l^2+2*d*f*g^2-2*b*g^2*h+16001*c*d*g*k+16001*c*d*f*l+16001*b*d*g*l+b*c*h*l-8001*b*c^2,
       	  d*f*h*k*l-b*h^2*k*l-8001*d^2*k^2*l+2*d*f^2*g-2*b*f*g*h+16001*c*d*f*k+16001*b*d*g*k-16001*b*c*h*k+16001*b*d*f*l-16001*b^2*h*l-8001*b^2*c,d*f*h*k^2-b*h^2*k^2-8001*d^2*k^3+2*d*f^3-2*b*f^2*h+
       	  16000*b*d*f*k+b^2*h*k-8001*b^3)
I = sub(I, {(last gens ring I) => random(1,R)});
-- looong time!
time Igb = gb I;
-- quick!
time Igb' = computeGB I;
-- make sure correct
gens Igb === gens Igb'
///

MYTEST ///
-- gb over a fraction field
load "quickGB.m2"
debug needsPackage "PD"
gbTrace = 3
R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l, MonomialOrder=>Lex]
I = ideal(h*j*l-2*e*g+16001*c*j+16001*a*l,h*j*k-2*e*f+16001*b*j+16001*a*k,h*j^2+2*e^2+16001*a*j,d*j^2+2*a*e,g*h*j+e*h*l+8001*d*j*l+16001*c*e+16001*a*g,f*h*j+e*h*k+8001*d*j*k+16001*b*e+16001*a*f
          ,e*g*j+8001*c*j^2+e^2*l,d*g*j+d*e*l+16001*a*c,e*f*j+8001*b*j^2+e^2*k,d*f*j+d*e*k+16001*a*b,d*e*j-a*h*j-16001*a^2,d*e^2-a*e*h-8001*a*d*j,d*g*k*l-c*h*k*l-d*f*l^2+b*h*l^2-2*c*f*g+2*b*g^2-16001
       	  *c^2*k+16001*b*c*l,d*g*k^2-c*h*k^2-d*f*k*l+b*h*k*l-2*c*f^2+2*b*f*g-16001*b*c*k+16001*b^2*l,d*g^2*k-c*g*h*k-d*f*g*l+c*f*h*l-8001*c*d*k*l+8001*b*d*l^2+16001*c^2*f-16001*b*c*g,d*f*g*k-b*g*h*k-
       	  8001*c*d*k^2-d*f^2*l+b*f*h*l+8001*b*d*k*l+16001*b*c*f-16001*b^2*g,c*f*g*k-b*g^2*k-8001*c^2*k^2-c*f^2*l+b*f*g*l-16001*b*c*k*l-8001*b^2*l^2,e^2*g*k+8001*c*e*j*k-e^2*f*l-8001*b*e*j*l,d*g*h*l^2
       	  -c*h^2*l^2-8001*d^2*l^3+2*d*g^3-2*c*g^2*h+16000*c*d*g*l+c^2*h*l-8001*c^3,d*f*h*l^2-b*h^2*l^2-8001*d^2*k*l^2+2*d*f*g^2-2*b*g^2*h+16001*c*d*g*k+16001*c*d*f*l+16001*b*d*g*l+b*c*h*l-8001*b*c^2,
       	  d*f*h*k*l-b*h^2*k*l-8001*d^2*k^2*l+2*d*f^2*g-2*b*f*g*h+16001*c*d*f*k+16001*b*d*g*k-16001*b*c*h*k+16001*b*d*f*l-16001*b^2*h*l-8001*b^2*c,d*f*h*k^2-b*h^2*k^2-8001*d^2*k^3+2*d*f^3-2*b*f^2*h+
       	  16000*b*d*f*k+b^2*h*k-8001*b^3)
I = sub(I, {(last gens ring I) => random(1,R)});
basevars = {d, e, f, g, h, k, l}
(S,SF) = makeFiberRings basevars
ISF = sub(I,SF)
-- 5 seconds...
time ISFgb = gb ISF
-- this one takes *way* longer (never let it finish).  Because over a fraction field?
time ISFgb' = computeGB ISF
///

MYTEST ///
-- block ordering
load "quickGB.m2"
debug needsPackage "PD"
gbTrace = 3
R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l, MonomialOrder=>Lex]
I = ideal(h*j*l-2*e*g+16001*c*j+16001*a*l,h*j*k-2*e*f+16001*b*j+16001*a*k,h*j^2+2*e^2+16001*a*j,d*j^2+2*a*e,g*h*j+e*h*l+8001*d*j*l+16001*c*e+16001*a*g,f*h*j+e*h*k+8001*d*j*k+16001*b*e+16001*a*f
          ,e*g*j+8001*c*j^2+e^2*l,d*g*j+d*e*l+16001*a*c,e*f*j+8001*b*j^2+e^2*k,d*f*j+d*e*k+16001*a*b,d*e*j-a*h*j-16001*a^2,d*e^2-a*e*h-8001*a*d*j,d*g*k*l-c*h*k*l-d*f*l^2+b*h*l^2-2*c*f*g+2*b*g^2-16001
       	  *c^2*k+16001*b*c*l,d*g*k^2-c*h*k^2-d*f*k*l+b*h*k*l-2*c*f^2+2*b*f*g-16001*b*c*k+16001*b^2*l,d*g^2*k-c*g*h*k-d*f*g*l+c*f*h*l-8001*c*d*k*l+8001*b*d*l^2+16001*c^2*f-16001*b*c*g,d*f*g*k-b*g*h*k-
       	  8001*c*d*k^2-d*f^2*l+b*f*h*l+8001*b*d*k*l+16001*b*c*f-16001*b^2*g,c*f*g*k-b*g^2*k-8001*c^2*k^2-c*f^2*l+b*f*g*l-16001*b*c*k*l-8001*b^2*l^2,e^2*g*k+8001*c*e*j*k-e^2*f*l-8001*b*e*j*l,d*g*h*l^2
       	  -c*h^2*l^2-8001*d^2*l^3+2*d*g^3-2*c*g^2*h+16000*c*d*g*l+c^2*h*l-8001*c^3,d*f*h*l^2-b*h^2*l^2-8001*d^2*k*l^2+2*d*f*g^2-2*b*g^2*h+16001*c*d*g*k+16001*c*d*f*l+16001*b*d*g*l+b*c*h*l-8001*b*c^2,
       	  d*f*h*k*l-b*h^2*k*l-8001*d^2*k^2*l+2*d*f^2*g-2*b*f*g*h+16001*c*d*f*k+16001*b*d*g*k-16001*b*c*h*k+16001*b*d*f*l-16001*b^2*h*l-8001*b^2*c,d*f*h*k^2-b*h^2*k^2-8001*d^2*k^3+2*d*f^3-2*b*f^2*h+
       	  16000*b*d*f*k+b^2*h*k-8001*b^3)
I = sub(I, {(last gens ring I) => random(1,R)});
basevars = {d, e, f, g, h, k, l}
(S,SF) = makeFiberRings basevars
IS = sub(I,S)
-- 80 seconds
time ISgb = gb IS
-- ~5 seconds
time ISgb' = computeGB IS
assert(gens ISgb === gens ISgb')
///
