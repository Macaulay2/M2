-- This file is intended to be turned into an independent package,
-- at which point VirtualResolutions will simply include it as an
-- exported package.

-*
newPackage(
        "Colon",
        Version => "0.1",
        Date => "",
        Authors => {{Name => "",
                  Email => "",
                  HomePage => ""}},
        Headline => "saturation and ideal and submodule colon/quotient routines",
        DebuggingMode => true
        )

export {
    "grevLexRing",
    "eliminationInfo",
    "saturationByElimination",
    "saturationByGrevLex",
    "intersectionByElimination"
    }
*-

-- quotient methods:
-- 1. syzygyies
-- 2. use eliination methods? I forget how?
-- 3. case: x is a variable, I is homogeneous
--    case: x is a polynomial
--    case: x is an ideal
----------------
-- saturation --
----------------

-- Bayer trick
-- eliminate(t,(I, tf-1))
--   a. use MGB
--   b. use gb
-- iterated quotient

grevLexRing = method()
grevLexRing(ZZ, Ring) := (i,R) -> (
     n := numgens R;
     degs := degrees R;
     kk := coefficientRing R;
     X := local X;
     perm := (L) -> L_{0..i-1} | L_{i+1..n-1} | {L_i};
     perminv := (L) -> L_{0..i-1} | {L_(n-1)} | L_{i..n-2};
     M := monoid [X_1 .. X_n,Degrees=>perm degs,MonomialSize=>16];
     R1 := kk M;
     fto := map(R1,R,perminv generators R1);
     fback := map(R,R1,perm generators R);
     (R1, fto, fback)
    )

grevLexRing(ZZ, Ring) := (i,R) -> (
     n := numgens R;
     if i === n-1 then return (R,identity,identity);
     degs := degrees R;
     kk := coefficientRing R;
     X := local X;
     perm := (L) -> L_{0..i-1} | {L_(n-1)} | L_{i+1..n-2} | {L_i};
     M := monoid [X_1 .. X_n,Degrees=>perm degs,MonomialSize=>16];
     R1 := kk M;
     fto := map(R1,R,perm generators R1);
     fback := map(R,R1,perm generators R);
     (R1, fto, fback)
    )

-- This is the temporary fast saturation that Mike Stillman created
eliminationInfo = method()
eliminationInfo Ring := (cacheValue symbol eliminationInfo)(R -> (
     n := numgens R;
     k := coefficientRing R;
     X := local X;
     M := monoid [X_0 .. X_n,MonomialOrder=>Eliminate 1,MonomialSize=>16];
     R1 := k M;
     fto := map(R1,R,drop(generators R1, 1));
     fback := map(R,R1,matrix{{0_R}}|vars R);
     (R1, fto, fback)
    ))

saturationByGrevLex = method()
saturationByGrevLex(Ideal, RingElement) := (I, v) -> (
    R := ring I;
    if ring I =!= ring v then error "expected same ring";
    if index v === null then error "expected ring element to be a variable in the ring";
    (R1, fto, fback) := grevLexRing(index v, R);
    J := fto I;
    g := groebnerBasis(J, Strategy=>"F4");
    (g1, maxpower) := divideByVariable(g, R1_(numgens R1-1));
    (ideal fback g1, maxpower)
    )

-- Computing (I : f^\infty) = saturate(I,f)
-- version #1: elimination
saturationByElimination = method()
saturationByElimination(Ideal, RingElement) := (I, f) -> (
     R := ring I;
     (R1,fto,fback) := eliminationInfo R;
     f1 := fto f;
     I1 := fto I;
     J := ideal(f1*R1_0-1) + I1;
     --g := groebnerBasis(J, Strategy=>"MGB");
     --g := gens gb J;
     g := groebnerBasis(J, Strategy=>"F4");
     p1 := selectInSubring(1, g);
     ideal fback p1
     )

intersectionByElimination = method()
intersectionByElimination(Ideal, Ideal) := (I,J) -> (
     R := ring I;
     (R1,fto,fback) := eliminationInfo R;
     I1 := R1_0 * fto I;
     J1 := (1-R1_0) * fto J;
     L := I1 + J1;
     --g := groebnerBasis(J, Strategy=>"MGB");
     --g := gens gb J;
     g := groebnerBasis(L, Strategy=>"F4");
     p1 := selectInSubring(1, g);
     ideal fback p1
    )

intersectionByElimination List := (L) -> fold(intersectionByElimination, L)

saturationByElimination(Ideal, Ideal) := (I, J) -> (
    L := for g in J_* list saturationByElimination(I, g);
    intersectionByElimination L
    )

-- used when P = decompose irr
saturationByElimination(Ideal, List) := (I, P) -> (
    apply(P, J -> I = saturationByElimination(I,J));
    I
    )

saturationByGrevLex(Ideal, Ideal) := (I, J) -> (
    L := for g in J_* list saturationByGrevLex(I, g);
    pows := L/last;
    ids := L/first;
    if any(pows, x -> x == 0) then
      I
    else
      intersectionByElimination ids
    )

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (M,B)=(Module,Ideal)
----- Output: Returns true if saturate(M,B)==0 and false otherwise
----- Description: This checks whether the saturation of a module M
----- with respects to an ideal B is zero. This is done by checking
----- whether for each generator of B some power of it annihilates
----- the module M. We do this generator by generator.
--------------------------------------------------------------------
--------------------------------------------------------------------
saturationZero = method()
saturationZero (Module,Ideal) := (M,B) ->(
    Vars := flatten entries vars ring B;
    bGens := flatten entries mingens B;
    for i from 0 to #bGens-1 do (
          b := bGens#i;
          bVars := support b;
              rVars := delete(bVars#1,delete(bVars#0,Vars))|bVars;
              R := coefficientRing ring B [rVars,MonomialOrder=>{Position=>Up,#Vars-2,2}];
              P := sub(presentation M,R);
              G := gb P;
              if (ann coker selectInSubring(1,leadTerm G)) == 0 then return false;
    );
    true
)

--------------------------------------------------------------------
--------------------------------------------------------------------
----- Input: (I,B)=(Ideal,Ideal)
----- Output: Returns true if saturate(comodule I,B)==0 and false otherwise.
--------------------------------------------------------------------
--------------------------------------------------------------------
saturationZero (Ideal,Ideal) := (I,B) ->(
    saturationZero(comodule I,B)
    )

end--

beginDocumentation()

doc ///
    Key
        saturationZero
        (saturationZero,Module,Ideal)
        (saturationZero,Ideal,Ideal)
    Headline
        checks whether the saturation of a module with respects to a given ideal is zero
    Usage
        saturationZero(M,B)
        saturationZero(I,B)
    Inputs
        M:Module
        B:Ideal
        I:Ideal
    Outputs
        :Boolean
    Description
        Text
            Given an module M and an ideal B saturationZero checks whether the saturation of M by B is zero. If it is
            saturationZero returns true otherwise it returns false. This is done without computing the saturation of M by B.
            Instead we check whether for each generator of B some power of it annihilates the module M. We do this
            generator by generator.

            If M is an ideal saturationZero checks whether the saturation comodule of M by B is zero.
///


end--
doc ///
Key
  Colon
Headline
  saturation and ideal and submodule colon/quotient routines
Description
  Text
  Example
Caveat
SeeAlso
///

end--

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

------ Tests for saturationZero
TEST ///
    S = ZZ/11[x_0,x_1,x_2,x_3,x_4];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    I' = saturate(I,irr);
    R = S^1/I';
    t = (saturate(R,irr)==0);
    assert (saturationZero(R,irr)==t)
    ///

TEST ///
    S = ZZ/11[x_0,x_1,x_2,x_3,x_4];
    irr = intersect(ideal(x_0,x_1),ideal(x_2,x_3,x_4));
    I = ideal(x_0^2*x_2^2+x_1^2*x_3^2+x_0*x_1*x_4^2, x_0^3*x_4+x_1^3*(x_2+x_3));
    I' = saturate(I,irr);
    R = S^1/I';
    t = (saturate(R,irr)==0);
    assert (saturationZero(I',irr)==t)
    ///

end--

restart
needsPackage "Colon"

kk = ZZ/32003
R = kk(monoid[x_0, x_1, x_2, x_3, x_4, Degrees => {2:{1, 0}, 3:{0, 1}}, Heft => {1,1}])
B0 = ideal(x_0,x_1)
B1 = ideal(x_2,x_3,x_4)

I = ideal(x_0^2*x_2^2*x_3^2+44*x_0*x_1*x_2^2*x_3^2+2005*x_1^2*x_2^2*x_3^2+12870
     *x_0^2*x_2*x_3^3-725*x_0*x_1*x_2*x_3^3-15972*x_1^2*x_2*x_3^3-7768*x_0^2*x_2
     ^2*x_3*x_4-13037*x_0*x_1*x_2^2*x_3*x_4-14864*x_1^2*x_2^2*x_3*x_4+194*x_0^2*
     x_2*x_3^2*x_4-2631*x_0*x_1*x_2*x_3^2*x_4-2013*x_1^2*x_2*x_3^2*x_4-15080*x_0
     ^2*x_3^3*x_4-9498*x_0*x_1*x_3^3*x_4+5151*x_1^2*x_3^3*x_4-12401*x_0^2*x_2^2*
     x_4^2+4297*x_0*x_1*x_2^2*x_4^2-13818*x_1^2*x_2^2*x_4^2+7330*x_0^2*x_2*x_3*x
     _4^2-13947*x_0*x_1*x_2*x_3*x_4^2-12602*x_1^2*x_2*x_3*x_4^2-14401*x_0^2*x_3^
     2*x_4^2+8101*x_0*x_1*x_3^2*x_4^2-1534*x_1^2*x_3^2*x_4^2+8981*x_0^2*x_2*x_4^
     3-11590*x_0*x_1*x_2*x_4^3+1584*x_1^2*x_2*x_4^3-13638*x_0^2*x_3*x_4^3-5075*x
     _0*x_1*x_3*x_4^3-14991*x_1^2*x_3*x_4^3,x_0^7*x_2-6571*x_0^6*x_1*x_2+13908*x
     _0^5*x_1^2*x_2+11851*x_0^4*x_1^3*x_2+14671*x_0^3*x_1^4*x_2-14158*x_0^2*x_1^
     5*x_2-15190*x_0*x_1^6*x_2+6020*x_1^7*x_2+5432*x_0^7*x_3-8660*x_0^6*x_1*x_3-
     3681*x_0^5*x_1^2*x_3+11630*x_0^4*x_1^3*x_3-4218*x_0^3*x_1^4*x_3+6881*x_0^2*
     x_1^5*x_3-6685*x_0*x_1^6*x_3+12813*x_1^7*x_3-11966*x_0^7*x_4+7648*x_0^6*x_1
     *x_4-10513*x_0^5*x_1^2*x_4+3537*x_0^4*x_1^3*x_4+2286*x_0^3*x_1^4*x_4+733*x_
     0^2*x_1^5*x_4+11541*x_0*x_1^6*x_4+660*x_1^7*x_4)

ans1 = elapsedTime saturationByGrevLex(saturationByGrevLex(I, B0), B1);
ans2 = elapsedTime saturationByGrevLex(saturationByGrevLex(I, B1), B0);

elapsedTime saturationByGrevLex(I, x_0);
elapsedTime saturationByGrevLex(I, x_1);

ans3 = elapsedTime saturationByElimination(saturationByElimination(I, B0), B1);
ans4 = elapsedTime saturationByElimination(saturationByElimination(I, B1), B0);


elapsedTime J1 = saturationByElimination(I, x_0);
elapsedTime J2 = saturationByElimination(I, x_1);
elapsedTime J = intersectionByElimination(J1,J2);
elapsedTime J' = intersectionByElimination(J2,J1);
elapsedTime J'' = intersect(J1,J2);
elapsedTime J''' = intersect(J2,J1);
J == J'
J == J''

time gens gb I;
J2 = elapsedTime saturationByElimination(I, x_0);
assert isHomogeneous J2
J2' = elapsedTime saturationByElimination(I, x_1);

J2 = elapsedTime saturationByElimination(I, ideal(x_0,x_1));
J2' = elapsedTime saturationByElimination(J2, ideal(x_2,x_3,x_4));

J1 = elapsedTime saturate(I, x_0);
J1' = elapsedTime saturate(I, x_1);
J1 == J2
J1' == J2'

betti J2
betti J1

restart
needsPackage "Colon"
load "badsaturations.m2"

J = paramRatCurve({2,2},{3,3},{4,2});
elapsedTime genSat(J,2) -- 200 sec
elapsedTime genSat2(J,2) -- 50 sec
elapsedTime genSat3(J,2) -- 35 sec

J = paramRatCurve({2,2},{3,3},{5,2});
elapsedTime genSat(J,2) -- 691 sec
elapsedTime genSat2(J,2) -- 104 sec
elapsedTime genSat3(J,2) -- 71 sec

J = paramRatCurve({2,2},{3,4},{4,3});
elapsedTime genSat(J,2) --  sec
elapsedTime genSat2(J,2) --  sec
elapsedTime genSat3(J,2) -- 75 sec

I = ideal J_*_{5,13}
use ring I
elapsedTime I1 = saturate(I, x_0);
elapsedTime (I2,pow) = saturationByGrevLex(I,x_0);
I1 == I2

elapsedTime I1 = saturate(I, x_1);
elapsedTime (I2,pow) = saturationByGrevLex(I,x_1);
I1 == I2
elapsedTime J1 = intersectionByElimination(I1,I2);

elapsedTime I1 = saturationByGrevLex(I, B0);
elapsedTime I2 = saturationByGrevLex(I1, B1);

elapsedTime saturationByGrevLex(saturationByGrevLex(I, B0), B1);
elapsedTime saturationByGrevLex(saturationByGrevLex(I, B1), B0);

elapsedTime saturationByElimination(saturationByElimination(I, B0), B1);
elapsedTime saturationByElimination(saturationByElimination(I, B1), B0);

elapsedTime J0a = saturationByGrevLex(I,x_0);
elapsedTime J0b = saturationByGrevLex(I,x_1);
--J1 = elapsedTime intersectionByElimination(first J0a,first J0b);
La = elapsedTime trim first J0a;
Lb = elapsedTime trim first J0b;
J1 = elapsedTime intersectionByElimination(La, Lb);
J1a = elapsedTime saturationByGrevLex(J1,x_2);
J1b = elapsedTime saturationByGrevLex(J1,x_3);
J1c = elapsedTime saturationByGrevLex(J1,x_4);
J1a#1, J1b#1, J1c#1
J1ab = elapsedTime intersectionByElimination(J1a,J1b);
elapsedTime J2 = intersectionByElimination{first J1a, first J1b, first J1c};
elapsedTime saturationByGrevLex(I,B0);

saturationByElimination(I,x_0);

(R1,fto,fback) = grevLexRing(0,S)
L = fto I;
satL = ideal first divideByVariable(gens gb L, R1_4);
fback satL
oo == I1
leadTerm oo
ideal oo
(R1,fto,fpack) = grevLexRing(1,S)
use S

R = ZZ/101[a..d]
I = ideal"ab-ac,b2-cd"
I1 = saturate(I,a)
elapsedTime (I2,pow) = saturationByGrevLex(I,a);
I1 == I2
pow
(R1,fto,fback) = grevLexRing(0,R)
fto I
fto

----------------------------
-- example:
R = ZZ/101[vars(0..14)]
M = genericMatrix(R,a,3,5)
I = minors(3,M);
codim I
J = ideal((gens I) * random(R^10, R^5))
elapsedTime(J : I);
degree I
elapsedTime(J : I_0);
