-- This function is not exported.  "debug MinimalPrimes" if you need it.
checkMinimalPrimes = method(Options => {"Answer" => null, "CheckPrimality" => false})
checkMinimalPrimes(Ideal, List) := opts -> (I, C1) -> (
    -- check that the intersection of the components in C1
    --   is contained in the radical of I
    -- check that each comp contains I
    for c in C1 do assert isSubset(I, c);
    J := intersect C1;
    assert (radicalContainment(J, I) === null);
    if opts#"CheckPrimality" then assert all(C1, isPrime);
    if opts#"Answer" =!= null then (
        C2 := if opts#"Answer" === decompose then decompose I else opts#"Answer";
        set1 := C1/(i -> set flatten entries gens gb i)//set;
        set2 := C2/(i -> set flatten entries gens gb i)//set;
        assert(set1 === set2);
        );
    )

-- SIMPLETEST checks the results with the original decompose function
-- but this should only be run on examples that 'decompose' can do quickly
SIMPLETEST = (str) -> TEST str

-- These are slower tests, that we will eventually run explicitly
BENCHMARK = (str) -> TEST str

-- These are the tests that are too slow to run on a regular basis
-- Some are no longer so slow, and so need to have their heading changed.
TOODAMNSLOW = (str) -> null

-------------------------------
-- simple tests of minprimes --
-------------------------------

TEST ///
  -- used to be in tests/normal/decompose3.m2
  R = QQ [a, b, c, d, e]
  S = R [w_0, w_1, w_2, Join=>false]
  J = ideal(b*w_0-c*w_1+d*w_2,a*w_0-b*w_1+c*w_2)
  assert( intersect decompose J == J )
///

TEST get(currentFileDirectory | "decompose-test.m2") -- FIXME: one test here may be wrong
TEST get(currentFileDirectory | "decompose2-test.m2") -- FIXME: not testing anything
TEST get(currentFileDirectory | "decompose4-test.m2")
TEST get(currentFileDirectory | "decompose5-test.m2")
TEST get(currentFileDirectory | "minprimes-test.m2")
TEST get(currentFileDirectory | "minprimes2-test.m2") -- TODO: add Binomial strategy
TEST get(currentFileDirectory | "radical-test.m2") -- FIXME: not testing anything

TEST ///
  R = ZZ/101[a..d]
  assert(minprimes ideal(0_R) == {ideal(0_R)})
  assert(minprimes ideal(1_R) == {})
  assert(minprimes ideal(a^2-2*a*b+b^2) == {ideal(a-b)})
  I = ideal(a^2-b^2, c*d)
  C = minprimes I
  A = R/I
  assert( # minprimes ideal(0_A) == 4)
  assert( all(minprimes ideal(0_A), I -> ring I === A))
  assert(minprimes ideal(1_A) == {})
///

TEST /// -- test of CodimensionLimit option
  R1 = QQ[d,f,j,k,m,r,t,A,D,G,I,K];
  I1 = ideal(I*K-K^2, r*G-G^2, A*D-D^2, j^2-j*t, d*f-f^2, d*f*j*k-m*r, A*D-G*I*K);
  time assert(18 == # minprimes(I1, CodimensionLimit => 6)) -- this should take some time
  time assert(22 == # minprimes I1) -- this takes more time, reasonably
  time assert(22 == # minprimes I1) -- fast, sweet!
  time assert(22 == # minprimes(I1, Strategy => "Legacy")) -- fast
  time assert(22 == # minprimes(I1, CodimensionLimit => 7, Verbosity => 2)) -- fast
  time assert(18 == # minprimes(I1, CodimensionLimit => 6, Verbosity => 2)) -- fast
///

TEST ///
   R = QQ[x,r,v,u,b, MonomialOrder=>{Lex=>5}]
   I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
   elapsedTime C = minprimes(I, Verbosity=>2);
   assert(#C == 2)
   assert(intersect C == I)
   C1 = ideal(b^3-7*b^2+14*b-7,r^2-r*u-2*u^2*b^2+9*u^2*b-5*u^2+b^2-4*b,x-u*b^2+5*u*b-4*u);
   C2 = ideal(b^3-7*b^2+14*b-7,r^2-r*u-2*u^2*b^2+9*u^2*b-5*u^2+b^2-4*b,x+r*b-2*r+u*b^2-5*u*b+4*u)
   assert(C1 == C_0 or C1 == C_1)
   assert(C2 == C_0 or C2 == C_1)

   -- -- much faster using the Legacy code...
   -- Legacy version of algorithm (via Char Sets).
   I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
   elapsedTime C = minimalPrimes(I, Strategy=>"Legacy", Verbosity=>2)
   assert(#C == 2)
   assert(intersect C == I)
   C1 = ideal(b^3-7*b^2+14*b-7,r^2-r*u-2*u^2*b^2+9*u^2*b-5*u^2+b^2-4*b,x-u*b^2+5*u*b-4*u);
   C2 = ideal(b^3-7*b^2+14*b-7,r^2-r*u-2*u^2*b^2+9*u^2*b-5*u^2+b^2-4*b,x+r*b-2*r+u*b^2-5*u*b+4*u)
   assert(C1 == C_0 or C1 == C_1)
   assert(C2 == C_0 or C2 == C_1)
///

TEST ///
   -- Over a tower of rings
   R = QQ[b][u][x,r,v, MonomialOrder=>{Lex=>3}]
   I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
   time C = minprimes(I, Verbosity=>2)
   elapsedTime C = minimalPrimes(ideal I_*, Strategy=>"Legacy", Verbosity=>2) -- does this line sometimes crash?  not sure...
   assert(#C == 2)
   assert(intersect C == I)
   C1 = ideal(b^3-7*b^2+14*b-7,r^2-r*u-2*u^2*b^2+9*u^2*b-5*u^2+b^2-4*b,x-u*b^2+5*u*b-4*u);
   C2 = ideal(b^3-7*b^2+14*b-7,r^2-r*u-2*u^2*b^2+9*u^2*b-5*u^2+b^2-4*b,x+r*b-2*r+u*b^2-5*u*b+4*u)
   assert(C1 == C_0 or C1 == C_1)
   assert(C2 == C_0 or C2 == C_1)
///

///
-- XXX
-- MES: working to simplify interface 27 July 2019.
  restart
  debug needsPackage "MinimalPrimes"
  R = QQ[x,r,v,u,b, MonomialOrder=>{Lex=>5}]
  I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
  J = annotatedIdeal(I, {}, {}, {})
  splitFunction#Factorization(J, {})
  J1 = splitFunction#IndependentSet(J, new HashTable from {Verbosity => 2})
  #J1
  L = first J1
  netList L.LexGBOverBase
  factorTower(L.LexGBOverBase, Verbosity => 3)

  J2 = splitFunction#SplitTower(first J1, new HashTable from {Verbosity => 4})
  J2

  -- splitFunction#IndependentSet: calls splitLexGB
  -- splitLexGB calls factors, ideal, gb.

  use R
  (S, SF) = makeFiberRings({v,u}, R)
  describe S
  describe SF

  use S
  G = resultant(F = b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b, b)
  I = ideal(F,G)
  S.cache#"StoSF" I
  minprimes oo
///

-- radicalContainment
TEST ///
    debug needsPackage "MinimalPrimes"
    R = ZZ/32003[a..f]
    F = map(R,R,symmetricPower(2,matrix{{a,b,c}}))
    I = ker F
    J = I^2
    G = I_0
    assert radicalContainment(G,J)
    assert not radicalContainment(G-a^2,J)
    assert (radicalContainment(I, I^2) === null)
///

TEST ///
    debug needsPackage "MinimalPrimes"
    R = (frac(QQ[a,b]))[x,y,z]
    F = 15 * a * (a*x-y-1/a)^2 * (1/b * x * z - a * y)^2
    assert(set factors F === set {(2, a^2*x-a*y-1), (2, x*z - a*b*y)})
    assert ( vars coefficientRing ring numerator F == 0 )

    G = a * (a*x-y-1/a)^2 * (1/b * x * z - a * y)^2
    assert( set factors F === set factors G)
///

TEST ///
    debug needsPackage "MinimalPrimes"
    R = QQ[a,b,x,y,z]
    F = 15 * a * (a*x-y-a^2)^2 * (b^2 * x * z - a * y)^2
    assert(set factors F === set {(1,a), (2, -a*x+y+a^2), (2, b^2*x*z - a*y)})
    --assert(numerator F == F) -- this would fail at the moment.  Should it be made to work?
///

TEST ///
    debug needsPackage "MinimalPrimes"
    R = QQ[a,b]
    assert (factors 0_R == {(1,0_R)})
    assert (factors 1_R == {})
///

TEST ///
    debug needsPackage "MinimalPrimes"
    R = ZZ/32003[a..d]
    I = monomialCurveIdeal(R,{1,3,4})
    J = I + ideal(a^5-b^5)
    assert(findNonMemberIndex(I,J) == -1)-- which (index of)  element of I is not in J
    assert(findNonMemberIndex(J,I) == 4) -- J_4 is not in I
    assert(selectMinimalIdeals {I,J} === {I})
    assert(selectMinimalIdeals {J,I} === {I})
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,e,f,g,h]
  (S,SF) = makeFiberRings {c}
  -- TODO NOTE: S whould contain ringmaps S-->R, R-->S, S-->SF,
  --          : Frank has done this now.  They are all stashed in S.
  --  note: for SF-->S we have numerator, denominator
  assert ( first sort gens S == c )
  assert ( not member(c, gens SF) )
  use SF
  use coefficientRing SF
  F = (1/c) * a
  assert ( F*c == a )
  assert(ring F === SF)
  assert(ring numerator F === S)
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,x]
  I = ideal( a*x + a^2 )
  (S,SF) = makeFiberRings {a}
  IS = sub(I,S)
  use SF
  use coefficientRing SF
  (L, M) = minimalizeOverFrac(IS, SF)
  assert( ring ideal L === SF )
  assert( ring ideal M === S )
  assert( ( ideal(x+a)  === ideal first minimalizeOverFrac(IS, SF) ) )
  assert( ( ideal(sub(a,S))  === ideal last minimalizeOverFrac(IS, SF) ) )
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,x]
  I = ideal( a*x + a^2 )
  (S,SF) = makeFiberRings {a,x}
  IS = sub(I,S)
  assert((ideal 1_SF === ideal first minimalizeOverFrac(IS, SF) ) )
  assert((IS  ===  ideal last minimalizeOverFrac(IS, SF) ) )
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,x]
  I = ideal( a*x + a^2 )
  assert try (S,SF) = makeFiberRings {} else true
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d,e,h]
  (S,SF) = makeFiberRings {c}
  use SF
  use coefficientRing SF
  I = ideal(h^4+c*h^3+6*c^2*h^2-4*c^3*h+c^4,e+(1/(11*c^2))*h^3+((-2)/(11*c))*h^2+(1/11)*h+(4*c)/11,d+((-3)/(11*c^2))*h^3+(6/(11*c))*h^2+((-3)/11)*h+(-c)/11,b+(1/(11*c^2))*h^3+((-2)/(11*c))*h^2+(1/11)*h+(4*c)/11,a+(1/(11*c^2))*h^3+((-2)/(11*c))*h^2+(1/11)*h+(4*c)/11)
  use S
  J = ideal(d+3*e+c,b-e,a-e,e*h+3*e*c-h^2+h*c+c^2,e^2+3*e*c+c^2)
  assert( J == contractToPolynomialRing I )
///

--- TODO : (5/14/2013) Get these working with the new version of splitTower code.
---        Also, for every minprimes check below, make one for each of
---        the built in general strategies.  At this time, there are only 2
---        namely BirationalStrat and NoBirationalStrat.  Also should come up with
---        some better names for these probably.
-*
TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d,e,h]
  J = ideal(d+3*e+c,b-e,a-e,e*h+3*e*c-h^2+h*c+c^2,e^2+3*e*c+c^2)
  minprimes J
  assert( independentSets J == {h})
  Je = extendIdeal J
  use ring Je
  use coefficientRing ring Je
  assert( Je == ideal(e^4+h*e^3+h^2*e^2+h^3*e+h^4,d+(1/(h))*e^2+2*e+h,c+((-1)/(h))*e^2+e-h,b-e,a-e))
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,h]
  I = ideal(a+b+c+d,a*b+b*c+c*d+d*a,a*b*c+b*c*d+c*d*a+d*a*b,a*b*c*d-h^4)
  (part1,I2) = equidimSplitOneStep I;
  (I1, basevars, ISF) = part1;
  assert (I1 == ideal(b+d,a+c,c^2*d^2-h^4))
  assert (ring I1 === R)
  assert (ring I2 === R)
  assert (all (basevars, x -> ring x === R))
  assert (basevars == {c, h})
  assert (I2 == ideal(b+d,a+c,c^2-d^2,d^4-h^4))
  use ring first ISF
  use coefficientRing ring first ISF
  assert (ISF == {d^2+(-h^4)/(c^2), b+d, a+c})
///

TEST ///
  -- boundary cases for equidimSplitOneStep
  debug needsPackage "MinimalPrimes"
  R = QQ[x,y]
  I = ideal{0_R}
  (part1,I2) = equidimSplitOneStep I;
  (I1, basevars, ISF) = part1;
  J = ideal{1_R}
  assert try equidimSplitOneStep J else true;
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d]
  I = ideal {a*b,a*c,a^2*d,b^2*c,b*d,c^2*d^2}
  -- redundant ideals in list; can we avoid this?
  Isplits = splitLexGB I
  assert (unique Isplits == {ideal(d,c,b), ideal(c,b,a), ideal(d,b,a), ideal(d,c,a)})
///

TEST ///
  -- make a better (more complicated?) test (without simply using old output)
  debug needsPackage "MinimalPrimes"
  R = (frac (QQ[c]))[a, b, d, e, h]
  use coefficientRing R
  I = ideal{h^20+122*c^5*h^15-122*c^10*h^10-c^15*h^5, e*h^10-c^5*e*h^5+((-1)/(55*c^4))*h^15+((2*c)/5)*h^10+((-21*c^6)/55)*h^5, e^4*h^5-c^5*e^4+2*c*e^3*h^5-2*c^6*e^3+c^2*e^2*h^5-c^7*e^2+(3/(55*c^6))*h^15+(34/(5*c))*h^10+((-377*c^4)/55)*h^5, e^7+3*c*e^6+c^2*e^5+((-4)/(c))*e^3*h^5+4*c^4*e^3-4*e^2*h^5+3*c^5*e^2-3*c*e*h^5+((-12)/(55*c^8))*h^15+((-131)/(5*c^3))*h^10+((1398*c^2)/55)*h^5, d*h^15+122*c^5*d*h^10-122*c^10*d*h^5-c^15*d-c^8*e^3*h^5+c^13*e^3-2*c^9*e^2*h^5+2*c^14*e^2-c^10*e*h^5+c^15*e+((8*c)/55)*h^15+((89*c^6)/5)*h^10+((-987*c^11)/55)*h^5, d*e*h^5-c^5*d*e+((-1)/(55*c^4))*d*h^10+((2*c)/5)*d*h^5+((-21*c^6)/55)*d+((-21)/(55*c))*e^3*h^5+((21*c^4)/55)*e^3+((-42)/55)*e^2*h^5+((42*c^5)/55)*e^2+((-21*c)/55)*e*h^5+((21*c^6)/55)*e+(168/(3025*c^8))*h^15+(1869/(275*c^3))*h^10+((-20727*c^2)/3025)*h^5, d*e^2+3*c*d*e+c^2*d+((-1)/(c^2))*e^5+((-3)/(c))*e^4+(3/(c^5))*e^3*h^5-4*e^3+(3/(c^4))*e^2*h^5-3*c*e^2+(1/(c^3))*e*h^5-c^2*e+((-6)/(55*c^12))*h^15+((-68)/(5*c^7))*h^10+(754/(55*c^2))*h^5, d^2+d*e+(3/(275*c^9))*d*h^10+(34/(25*c^4))*d*h^5+((173*c)/275)*d+(7/(5*c^4))*e^6+(18/(5*c^3))*e^5+((-1)/(c^2))*e^4+(1603/(275*c^6))*e^3*h^5+((-2153)/(275*c))*e^3+(1446/(275*c^5))*e^2*h^5+((-1446)/275)*e^2+((-487)/(275*c^4))*e*h^5+((-173*c)/275)*e+(981/(15125*c^13))*h^15+(10123/(1375*c^8))*h^10+((-166784)/(15125*c^3))*h^5, b*h^5-c^5*b+d*h^5-c^5*d+((-1)/(2*c^2))*e^3*h^5+((c^3)/2)*e^3+((-3)/(2*c))*e^2*h^5+((3*c^4)/2)*e^2-e*h^5+c^5*e+((-1)/(22*c^9))*h^15+((-11)/(2*c^4))*h^10+((61*c)/11)*h^5, b*e-c*b+(3/(275*c^9))*d*h^10+(34/(25*c^4))*d*h^5+((-377*c)/275)*d+((-3)/(5*c^4))*e^6+((-7)/(5*c^3))*e^5+((-872)/(275*c^6))*e^3*h^5+(597/(275*c))*e^3+((-479)/(275*c^5))*e^2*h^5+(754/275)*e^2+((-212)/(275*c^4))*e*h^5+((377*c)/275)*e+((-1219)/(15125*c^13))*h^15+((-12977)/(1375*c^8))*h^10+(165141/(15125*c^3))*h^5, b*d-c*b-c*d+((-4)/(5*c^4))*e^6+((-11)/(5*c^3))*e^5+((-29)/(10*c^6))*e^3*h^5+(39/(10*c))*e^3+((-5)/(2*c^5))*e^2*h^5+(7/2)*e^2+((-1)/(5*c^4))*e*h^5+c*e+((-1)/(10*c^13))*h^15+((-119)/(10*c^8))*h^10+(66/(5*c^3))*h^5, b^2+3*c*b+((-6)/(275*c^9))*d*h^10+((-68)/(25*c^4))*d*h^5+((754*c)/275)*d+(2/(c^4))*e^6+(5/(c^3))*e^5+(5083/(550*c^6))*e^3*h^5+((-4533)/(550*c))*e^3+(3291/(550*c^5))*e^2*h^5+((-4391)/550)*e^2+(479/(275*c^4))*e*h^5+((-754*c)/275)*e+(7901/(30250*c^13))*h^15+(84633/(2750*c^8))*h^10+((-529932)/(15125*c^3))*h^5, a+b+d+e+c}
  Isplits = splitLexGB I
  assert (Isplits == {ideal(h-c,e-c,d-c,a+b+3*c,b^2+3*c*b+c^2), ideal(h-c,d-c,b-c,a+e+3*c,e^2+3*c*e+c^2), ideal(h-c,a+b+d+e+c,d*e+c*b+c*d+c*e+c^2,d^2-4*b*e-2*e^2+2*c*b+4*c*d-c*e,b*d+2*b*e+e^2-c*b-2*c*d-c^2,b^2+c*b+c*d+c*e+c^2,e^3+3*c*e^2-3*c^2*b-c^2*d,b*e^2-c*e^2+2*c^2*b+c^2*d+c^2*e+c^3), ideal(h,e,d,b,a+c), ideal(h,e+c,d,b,a), ideal(h,e,d,b,a+c), ideal(h,e+c,d,b,a), ideal(e-h,b+d+2*h+c,a-h,h^2+3*c*h+c^2,d^2+2*d*h+c*d+8*c*h+3*c^2), ideal(b+d+2*e+c,a-e,e*h+(1/2)*h^2+((-3*c)/2)*e+((c)/2)*h+(-c^2)/2,e^2+3*c*e+c^2,d^2+2*d*e+c*d+8*c*e+3*c^2,h^3+((-5*c)/2)*h^2+((-11*c^2)/2)*e+((-c^2)/2)*h+(-5*c^3)/2), ideal(e-c,d-c,a+b+3*c,b^2+3*c*b+c^2,h^4+c*h^3+c^2*h^2+c^3*h+c^4), ideal(d-c,b-c,a+e+3*c,e^2+3*c*e+c^2,h^4+c*h^3+c^2*h^2+c^3*h+c^4), ideal(a+b+d+e+c,d*e+c*b+c*d+c*e+c^2,d^2-4*b*e-2*e^2+2*c*b+4*c*d-c*e,b*d+2*b*e+e^2-c*b-2*c*d-c^2,b^2+c*b+c*d+c*e+c^2,e^3+3*c*e^2-3*c^2*b-c^2*d,b*e^2-c*e^2+2*c^2*b+c^2*d+c^2*e+c^3,h^4+c*h^3+c^2*h^2+c^3*h+c^4), ideal(b+d+2*e+c,a-e,e*h-h^2+3*c*e+c*h+c^2,e^2+3*c*e+c^2,d^2+2*d*e+c*d+8*c*e+3*c^2,h^3-2*c*h^2+11*c^2*e+c^2*h+4*c^3)})
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[x,y]
  I = ideal{0_R}
  assert (splitLexGB I == {I})
  J = ideal{1_R}
  assert (splitLexGB J == {})
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[x,y,z,MonomialOrder=>Lex]
  f = x^2+y^2+z^2
  g = x + y^2
  assert not hasLinearLeadTerm f
  assert hasLinearLeadTerm g
  assert not hasLinearLeadTerm 0_R
  assert not hasLinearLeadTerm 1_R
///

TEST ///
  -- again, find a better test that is not simply comparing against old return values?
  debug needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d,e,h]
  (S,SF) = makeFiberRings {c}
  use SF
  use coefficientRing SF
  J = ideal(h^4+c*h^3+c^2*h^2+c^3*h+c^4,e^4+c*e^3+c^2*e^2+c^3*e+c^4,d+((-1)/(c^2))*e^3,b+((-1)/(c))*e^2,a+(1/(c^2))*e^3+(1/(c))*e^2+e+c)
  answer = {ideal(h^4+c*h^3+c^2*h^2+c^3*h+c^4,e+((-1)/(c))*h^2,d-h,b+(1/(c^2))*h^3+(1/(c))*h^2+h+c,a+((-1)/(c^2))*h^3), ideal(h^4+c*h^3+c^2*h^2+c^3*h+c^4,e-h,d+((-1)/(c^2))*h^3,b+((-1)/(c))*h^2,a+(1/(c^2))*h^3+(1/(c))*h^2+h+c), ideal(h^4+c*h^3+c^2*h^2+c^3*h+c^4,e+((-1)/(c^2))*h^3,d+(1/(c^2))*h^3+(1/(c))*h^2+h+c,b-h,a+((-1)/(c))*h^2), ideal(h^4+c*h^3+c^2*h^2+c^3*h+c^4,e+(1/(c^2))*h^3+(1/(c))*h^2+h+c,d+((-1)/(c))*h^2,b+((-1)/(c^2))*h^3,a-h)};
  -- on rare occasion, this fails.  Does this have to do with a bad specialization again?
  assert (splitTower J == answer)
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d,e,h]
  assert (splitTower ideal 1_R == {ideal 1_R})
  assert (splitTower ideal 0_R == {ideal 0_R})
///

----------------------------------------------------------
-- Tests of routines for birationalSplit, simplifyIdeal --
----------------------------------------------------------

TEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z]
  I = ideal"
    -ab-ad+2ah,
    ad-bd-cf-2ah+2bh+2ck,
    ab-ad-2bh+2dh-2ck+2fk+2gl,
    ac-2cs-at+2bt,
    ac-cs-2at+bt,
    -d-3s+4u,
    -f-3t+4v,
    -g+4w,
    -a+2x,
    -b2-c2+2bx+2cy,
    -d2-f2-g2+2dx+2fy+2gz"
  (J,phi) = simplifyIdeal I
  J1 = ideal gens phi J
  assert(I == J1)
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z]
  F = -a+2*x
  assert(
    makeLinearElement(a,F)
      === (a, 1_R, a-2*x)
    )

  assert(makeLinearElement(x,F) === (x, 1_R, x -1/2 * a))
  assert(makeLinearElement(a, a^2-b) === null)
  assert(makeLinearElement(a, b^2-c) === null)
  assert(makeLinearElement(a, a^100) === null)
  assert(makeLinearElement(a, a+a^100) === null)
  assert(makeLinearElement(a,a) === (a,1_R,a))
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z]
  I = ideal"
    -ab-ad+2ah,
    ad-bd-cf-2ah+2bh+2ck,
    ab-ad-2bh+2dh-2ck+2fk+2gl,
    ac-2cs-at+2bt,
    ac-cs-2at+bt,
    -d-3s+4u,
    -f-3t+4v,
    -g+4w,
    -a+2x,
    -b2-c2+2bx+2cy,
    -d2-f2-g2+2dx+2fy+2gz"
  (J,phi) = simplifyIdeal2 I
  J1 = ideal gens phi J
  assert(I == J1)

  (J1, phi1) = simplifyIdeal I
///

TEST ///
restart
  debug needsPackage "MinimalPrimes"
  R = ZZ/101[a,b,c]
  I = ideal(a-b^2, b^2-c^2+a^2)
  simplifyIdeal2 I
  simplifyIdeal I
///

-------------------------------------
-- new splitIdeal tests -------------
-------------------------------------
TEST ///
  restart
  debug needsPackage "MinimalPrimes"
  needsPackage "UnitTestsPD"
  kk = QQ
  kk = ZZ/32003
  R = kk[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z]
  I = ideal"
    -ab-ad+2ah,
    ad-bd-cf-2ah+2bh+2ck,
    ab-ad-2bh+2dh-2ck+2fk+2gl,
    ac-2cs-at+2bt,
    ac-cs-2at+bt,
    -d-3s+4u,
    -f-3t+4v,
    -g+4w,
    -a+2x,
    -b2-c2+2bx+2cy,
    -d2-f2-g2+2dx+2fy+2gz"

///
-------------------------------------

-------------------------------------
-- splitIdeal tests -----------------
-------------------------------------
TEST ///
  restart
  debug needsPackage "MinimalPrimes"
  needsPackage "UnitTestsPD"
  kk = QQ
  kk = ZZ/32003
  R = kk[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z]
  I = ideal"
    -ab-ad+2ah,
    ad-bd-cf-2ah+2bh+2ck,
    ab-ad-2bh+2dh-2ck+2fk+2gl,
    ac-2cs-at+2bt,
    ac-cs-2at+bt,
    -d-3s+4u,
    -f-3t+4v,
    -g+4w,
    -a+2x,
    -b2-c2+2bx+2cy,
    -d2-f2-g2+2dx+2fy+2gz"
///
*-

-------------------------------------
--- Primary Decomposition tests below
-------------------------------------

BENCHMARK ///
  debug needsPackage "MinimalPrimes"
  --debug needsPackage "UnitTestsPD"
  Q = ZZ/32003[a,b,c,d]
  -- 3 random cubics in R
  I = ideal(-840*a^3-7687*a^2*b+9625*a*b^2-3820*b^3-10392*a^2*c-13100*a*b*c-11362*b^2*c-7463*a*c^2-11288*b*c^2+1417*c^3-14802*a^2*d-7804*a*b*d+5834*b^2*d-10186*a*c*d-11900*b*c*
     d+5062*c^2*d+14848*a*d^2+1270*b*d^2+4670*c*d^2+14589*d^3,6046*a^3-1565*a^2*b-10455*a*b^2+13719*b^3+9618*a^2*c+4969*a*b*c+14049*b^2*c+7621*a*c^2-15861*b*c^2-11905*c^3-
     13456*a^2*d+2029*a*b*d+8067*b^2*d-10420*a*c*d-14441*b*c*d-13965*c^2*d-3634*a*d^2-4035*b*d^2+350*c*d^2-8942*d^3,-12512*a^3-11973*a^2*b-8963*a*b^2-12001*b^3-10663*a^2*c-
     7202*a*b*c+9856*b^2*c-7955*a*c^2-8818*b*c^2+398*c^3+4259*a^2*d+13332*a*b*d+1576*b^2*d+3008*a*c*d+2588*b*c*d-6135*c^2*d-5311*a*d^2+6731*b*d^2-13991*c*d^2-9315*d^3)
  C1 = minprimes I;
  singularList = {ideal " c25+13767c24d+333c23d2+2478c22d3-7440c21d4-15655c20d5-4815c19d6+2905c18d7-9657c17d8+6596c16d9-259c15d10+15292c14d11+8119c13d12-1206c12d13-11455c11d14+11807c10d15-2026c9d16+8307c8d17+14982c7d18+3497c6d19+12142c5d20+11624c4d21-54c3d22-9394c2d23+1916cd24+15319d25 ,
      -4407c24-15196c23d+5428c22d2-15255c21d3-5669c20d4+2730c19d5-14633c18d6-278c17d7-7870c16d8+4996c15d9+5806c14d10-7410c13d11-6200c12d12+13830c11d13+2838c10d14+1136c9d15-14230c8d16-6507c7d17+545c6d18+2167c5d19+8969c4d20-3248c3d21-13200c2d22+bd23-3900cd23-8607d24 ,
       11176c23-10521c22d+13102c21d2-11217c20d3+15230c19d4+8358c18d5+6861c17d6+3523c16d7+3510c15d8-15747c14d9-8542c13d10-4549c12d11-13819c11d12+15835c10d13+6926c9d14-14048c8d15+6377c7d16+14057c6d17+12177c5d18-12108c4d19+15854c3d20+bcd21-7965c2d21-6940bd22+9878cd22-515d23 ,
       12762c22-14850c21d-3995c20d2-6922c19d3+5412c18d4-13776c17d5+9593c16d6+10827c15d7+12336c14d8+2025c13d9-9274c12d10+4644c11d11+12458c10d12-14403c9d13-10400c8d14+7511c7d15+6520c6d16-5229c5d17-12005c4d18+bc2d19+7674c3d19+13347bcd20+11720c2d20-8765bd21+6151cd21-12886d22 ,
       -3c21-5557c20d+14789c19d2+9824c18d3+13399c17d4+12204c16d5-398c15d6+13708c14d7+14057c13d8-6450c12d9+3768c11d10-13048c10d11-1393c9d12+2881c8d13+1748c7d14-4528c6d15+10831c5d16+bc3d17-14929c4d17+9796bc2d18-293c3d18-6243bcd19+5945c2d19-5013bd20-1892cd20+8819d21 ,
       2821c20+5738c19d-11541c18d2+13088c17d3-3838c16d4-9756c15d5-2493c14d6+5899c13d7-12949c12d8-7505c11d9-12554c10d10-11153c9d11-5715c8d12+2361c7d13+1799c6d14+bc4d15+13649c5d15+1247bc3d16-7318c4d16-11989bc2d17-13777c3d17-11876bcd18+9845c2d18+7240bd19-15850cd19+2831d20 ,
       7358c19+11639c18d+11372c17d2+3964c16d3+1643c15d4-11934c14d5-6581c13d6+9659c12d7-10325c11d8-4362c10d9+8934c9d10-14444c8d11+1325c7d12+bc5d13+15401c6d13+9420bc4d14+1965c5d14+14396bc3d15-9771c4d15+522bc2d16-11848c3d16-4161bcd17+2885c2d17-10477bd18+647cd18-7051d19 ,
       -7124c18+8514c17d-11062c16d2-14927c15d3-11438c14d4-3688c13d5+1130c12d6+10158c11d7+11503c10d8-15922c9d9-11612c8d10+bc6d11+10775c7d11-2845bc5d12+1199c6d12+15265bc4d13+4665c5d13-5875bc3d14-6220c4d14+2963bc2d15-3234c3d15+498bcd16+13045c2d16+10418bd17+10508cd17+435d18 ,
       -15842c17+12c16d-8632c15d2-3285c14d3-9228c13d4+5962c12d5+8775c11d6-14144c10d7+2970c9d8+bc7d9+12503c8d9+8063bc6d10-6653c7d10-11045bc5d11-13915c6d11+1691bc4d12-3946c5d12-11163bc3d13-411c4d13+12513bc2d14+2020c3d14-14376bcd15-2847c2d15+10495bd16-12986cd16+2727d17 ,
       12398c16-4c15d+99c14d2-11259c13d3-3766c12d4+10250c11d5-4076c10d6+bc8d7-2896c9d7+6644bc7d8-3869c8d8-6556bc6d9-9592c7d9+11954bc5d10-9567c6d10+4724bc4d11+3311c5d11+6663bc3d12-7990c4d12-3594bc2d13-2358c3d13-10977bcd14+12752c2d14-12668bd15+11651cd15+426d16 ,
       12213c15+15877c14d-6153c13d2-6868c12d3-1790c11d4+bc9d5+11490c10d5-2552bc8d6+1034c9d6-14943bc7d7-9714c8d7+5148bc6d8-12060c7d8-8934bc5d9-14530c6d9+7458bc4d10-590c5d10+5202bc3d11-6566c4d11+13344bc2d12+13824c3d12+7761bcd13+5043c2d13+14286bd14-13323cd14+3836d15 ,
       11678c14-13583c13d+2407c12d2+bc10d3-15722c11d3-5151bc9d4-6700c10d4+14402bc8d5-11969c9d5+13880bc7d6+8651c8d6-15039bc6d7+15387c7d7-11771bc5d8+5153c6d8+8469bc4d9+7983c5d9+11132bc3d10+10405c4d10-15756bc2d11-7298c3d11+3587bcd12+14804c2d12+11059bd13+3802cd13+7788d14 ,
       12599c13+bc11d-6271c12d+4415bc10d2+749c11d2+3411bc9d3+8566c10d3+15479bc8d4+14050c9d4-15290bc7d5-13632c8d5-1614bc6d6-7809c7d6-10642bc5d7-6992c6d7+5364bc4d8+11005c5d8-3004bc3d9+6658c4d9-4611bc2d10+7601c3d10-9879bcd11+5352c2d11-12677bd12+14738cd12+12409d13 ,
       bc12-1783c13-2779bc11d-4921c12d-5191bc10d2-9398c11d2-1824bc9d3-653c10d3+15881bc8d4-9309c9d4-7224bc7d5-11473c8d5-3599bc6d6+8817c7d6-14293bc5d7+7468c6d7-2604bc4d8+2826c5d8-15028bc3d9+7113c4d9-9285bc2d10-6795c3d10+6278bcd11+13155c2d11-14374bd12+4525cd12+4108d13 ,
       -8063bc11+6982c12+15948bc10d+3019c11d+12859bc9d2+6658c10d2-11198bc8d3+3114c9d3+10748bc7d4-8873c8d4-9095bc6d5-10668c7d5-34bc5d6+10137c6d6+217bc4d7-5639c5d7-12232bc3d8+5279c4d8-8861bc2d9+7281c3d9+b2d10-4820bcd10+8956c2d10+15395bd11-15832cd11+10572d12 ,
       -759bc11-6656c12+11139bc10d-10450c11d+11848bc9d2+3738c10d2+7340bc8d3+1991c9d3-5506bc7d4-6924c8d4+4185bc6d5+9644c7d5-14015bc5d6-10007c6d6+3825bc4d7+5257c5d7-7bc3d8+14215c4d8+b2cd9+5906bc2d9-11487c3d9+7900b2d10+11733bcd10-11242c2d10-5599bd11+7151cd11-9331d12 ,
       13840bc10+10265c11+8348bc9d+2785c10d-12257bc8d2+8756c9d2-11526bc7d3-333c8d3-917bc6d4+14143c7d4+528bc5d5-9301c6d5+7464bc4d6+15650c5d6+b2c2d7+5815bc3d7+8149c4d7-6216b2cd8+6664bc2d8+12003c3d8-15776b2d9-10851bcd9+2784c2d9-4028bd10+5819cd10-9414d11 ,
       -6875bc10+1430c11-669bc9d-7483c10d-11968bc8d2+12749c9d2+14711bc7d3+2131c8d3-15501bc6d4-15444c7d4+14333bc5d5-12257c6d5+b2c3d6-6407bc4d6-13646c5d6+10752b2c2d7-7038bc3d7-11294c4d7-9066b2cd8+6920bc2d8+8176c3d8-11528b2d9+13794bcd9+13881c2d9+116bd10-2066cd10+2184d11 ,
       13201bc9-4794c10-9881bc8d+2542c9d+3325bc7d2-2605c8d2+6064bc6d3+12261c7d3+b2c4d4-5607bc5d4+13905c6d4+4173b2c3d5-6507bc4d5-2069c5d5-11020b2c2d6+6061bc3d6+3749c4d6+15069b2cd7+4248bc2d7+10721c3d7+1625b2d8-5923bcd8+5895c2d8-1833bd9-7558cd9+8134d10 ,
       9428bc9-4844c10-8680bc8d+7085c9d-6186bc7d2-742c8d2+b2c5d3-12613bc6d3-7187c7d3+13678b2c4d4+8863bc5d4-6093c6d4+10628b2c3d5-6772bc4d5+1260c5d5+6716b2c2d6+590bc3d6-10848c4d6+692b2cd7-14307bc2d7-11452c3d7-10098b2d8-308bcd8+8455c2d8+6871bd9+15822cd9+580d10 ,
       -13641bc8+8475c9+b2c6d-15247bc7d+10622c8d-2186b2c5d2+10248bc6d2-9506c7d2-2224b2c4d3+2677bc5d3+14460c6d3+3782b2c3d4-7486bc4d4+3038c5d4+10597b2c2d5-8768bc3d5+7192c4d5+13050b2cd6-4907bc2d6-5311c3d6+13458b2d7+14811bcd7+15190c2d7-4097bd8+13173cd8-783d9 ,
       b2c7-5332bc8-7730c9+5413b2c6d+6342bc7d-890c8d+486b2c5d2+15759bc6d2-10817c7d2-1240b2c4d3+6658bc5d3+2155c6d3+846b2c3d4-6374bc4d4+12671c5d4+1200b2c2d5+3585bc3d5+15404c4d5-4822b2cd6-11933bc2d6+4302c3d6-15489b2d7-8363bcd7-6474c2d7-9270bd8-8240cd8-4848d9 ,
       -15836b2c7-7692bc8-3537c9+6042b2c6d+8057bc7d-8001c8d-6288b2c5d2-7658bc6d2-9691c7d2-8285b2c4d3+7737bc5d3-12242c6d3+8392b2c3d4-585bc4d4+10393c5d4+13254b2c2d5+10230bc3d5+5736c4d5+b3d6+10644b2cd6+7319bc2d6+14380c3d6-9797b2d7+11257bcd7+8768c2d7-3264bd8+5784cd8+10493d9 ,
       4303b2c6-7974bc7+4047c8+4370b2c5d-12258bc6d-219c7d+8704b2c4d2+10815bc5d2+11209c6d2+2542b2c3d3-1795bc4d3-567c5d3+b3cd4-566b2c2d4+3985bc3d4+8850c4d4-8500b3d5-9285b2cd5+7088bc2d5-6584c3d5+10190b2d6+1305bcd6-6518c2d6+6248bd7-13047cd7-1266d8 ,
       9824b2c6-10159bc7+3079c8+9571b2c5d+12941bc6d-3762c7d+77b2c4d2-14752bc5d2-2413c6d2+b3c2d3+3337b2c3d3-6538bc4d3-2472c5d3+7186b3cd4-15301b2c2d4+3983bc3d4-3619c4d4-6528b3d5-9387b2cd5+6975bc2d5+695c3d5-15947b2d6-10097bcd6-8757c2d6+1797bd7+12308cd7-1407d8 ,
       -6107b2c6+11084bc7-7632c8-10515b2c5d+10400bc6d+11870c7d+b3c3d2+15058b2c4d2-8008bc5d2-9287c6d2+12019b3c2d3-4272b2c3d3-692bc4d3+9871c5d3+9815b3cd4-1980b2c2d4-10385bc3d4-12211c4d4-9889b3d5-12727b2cd5-11234bc2d5-6784c3d5-8087b2d6+5159bcd6+6580c2d6+15733bd7+14821cd7+14654d8 ,
       b3c4+12680b2c5-4340bc6-14271c7+8739b3c3d+5964b2c4d-11726bc5d-1888c6d-12154b3c2d2+2878b2c3d2+5938bc4d2+15617c5d2+9862b3cd3-11606b2c2d3+11522bc3d3-6281c4d3-3955b3d4-3908b2cd4-148bc2d4+5224c3d4+2825b2d5-13446bcd5-9706c2d5+9862bd6-12969cd6-1314d7 ,
       7019b3c4-12024b2c5+14134bc6+12518c7+15045b3c3d+2b2c4d+15934bc5d+1841c6d+2941b3c2d2+14657b2c3d2+813bc4d2+13802c5d2+b4d3+9667b3cd3-9429b2c2d3-3134bc3d3+10880c4d3-368b3d4+13747b2cd4+1781bc2d4-6873c3d4-8995b2d5+14503bcd5-8468c2d5-980bd6-15649cd6-5875d7 ,
       -11348b3c4-9681b2c5+15578bc6+1433c7-9084b3c3d-12302b2c4d+9596bc5d-12435c6d+b4cd2+14317b3c2d2+1722b2c3d2-10760bc4d2+12485c5d2+15690b4d3+14141b3cd3-4228b2c2d3+2141bc3d3+1642c4d3+1418b3d4+2118b2cd4-5678bc2d4+13921c3d4+8199b2d5-8063bcd5-10613c2d5-628bd6-11667cd6+15871d7 ,
       -6370b3c4+13642b2c5-9393bc6-912c7+b4c2d-11532b3c3d+1609b2c4d-11102bc5d+9685c6d+8280b4cd2-5438b3c2d2-2917b2c3d2+13811bc4d2-2116c5d2-13058b4d3+9391b3cd3-2656b2c2d3+13352bc3d3-13058c4d3-1572b3d4+6698b2cd4-10835bc2d4-13293c3d4+1279b2d5+6480bcd5-7012c2d5-7727bd6+2233cd6-14375d7 ,
       b4c3+3368b3c4-3093b2c5-3373bc6-12956c7-232b4c2d+2143b3c3d-10021b2c4d-12172bc5d-1806c6d-15204b4cd2-3636b3c2d2-1878b2c3d2-13586bc4d2+12160c5d2+11382b4d3-7427b3cd3-9673b2c2d3-15729bc3d3-4712c4d3-14140b3d4+11375b2cd4-11217bc2d4+2269c3d4+12084b2d5+3013bcd5-8740c2d5-3445bd6-13123cd6-9468d7 ,
       7707b4c2+2743b3c3+4484b2c4-9275bc5-2532c6+b5d-8456b4cd-9673b3c2d-2357b2c3d-10820bc4d-1967c5d+14491b4d2+3621b3cd2+771b2c2d2+12995bc3d2+11676c4d2+6692b3d3-2058b2cd3+13237bc2d3-13172c3d3+3183b2d4-1914bcd4+3853c2d4+5127bd5-9291cd5+2634d6 ,
       b5c+6800b4c2+8851b3c3+7676b2c4+7926bc5+732c6+15595b5d+9776b4cd+3352b3c2d-8810b2c3d-11603bc4d+14852c5d+13111b4d2+9417b3cd2+3383b2c2d2-8698bc3d2+960c4d2+6722b3d3-3226b2cd3-12936bc2d3+225c3d3-426b2d4-3920bcd4+1478c2d4+196bd5-3449cd5-11586d6 ,
       b6+14674b5c-14313b4c2+11016b3c3-1567b2c4-2950bc5-10445c6+15536b5d-1070b4cd+12258b3c2d-7872b2c3d+698bc4d+14476c5d-3527b4d2-8257b3cd2-5918b2c2d2-1750bc3d2+7444c4d2+12615b3d3-3244b2cd3-137bc2d3-5806c3d3-13426b2d4+8355bcd4+1840c2d4+7683bd5-12123cd5-11127d6 ,
       -14683b6-13195b5c-2546b4c2+12217b3c3-3087b2c4+10642bc5-5016c6-7551b5d+11525b4cd-13016b3c2d-10580b2c3d-5944bc4d-4013c5d-5925b4d2-6997b3cd2+8436b2c2d2+8876bc3d2-286c4d2+14166b3d3+6683b2cd3+2439bc2d3-7200c3d3-5948b2d4+2351bcd4+15129c2d4+ad5-4587bd5+5850cd5+782d6 ,
       4732b5c-3072b4c2-11024b3c3+11579b2c4-7724bc5+14788c6+14082b5d+9238b4cd-13773b3c2d+656b2c3d+5569bc4d-2338c5d+2222b4d2+700b3cd2-2299b2c2d2+5353bc3d2-1992c4d2+13180b3d3-14349b2cd3-13394bc2d3-14983c3d3-11396b2d4+acd4-10312bcd4-2154c2d4-3601ad5-6017bd5-3935cd5+14654d6 ,
       11666b5c-15257b4c2-203b3c3-8567b2c4-5018bc5+12844c6+5223b5d-12830b4cd+65b3c2d+11658b2c3d+3921bc4d-12964c5d+14515b4d2-476b3cd2-6970b2c2d2+12291bc3d2-9883c4d2+7169b3d3+9756b2cd3+ac2d3-10588bc2d3+4483c3d3+1117b2d4+14125acd4+14069bcd4-5411c2d4-5924ad5+2164bd5-13444cd5+8675d6 ,
       13960b5c-9641b4c2-12253b3c3-7813b2c4-11879bc5-7597c6-11324b5d-11361b4cd+2703b3c2d-14020b2c3d+12884bc4d+15745c5d-9592b4d2+6086b3cd2-2265b2c2d2+ac3d2+2073bc3d2-1271c4d2-7229b3d3-15840b2cd3-962ac2d3+15187bc2d3-13511c3d3+9652b2d4+83acd4-14290bcd4+12037c2d4-10333ad5-11430bd5+9855cd5-11556d6 ,
       11269b5-14165b4c+125b3c2-3963b2c3+ac4+14404bc4+6323c5-3066b4d-8975b3cd+2933b2c2d-11491ac3d+5832bc3d-6306c4d+881b3d2+12711b2cd2-9498ac2d2+580bc2d2-6540c3d2-7933b2d3-8437acd3+1024bcd3+9127c2d3+1117ad4+6202bd4-6310cd4-5793d5 ,
       11069b5+7277b4c+10225b3c2-2527b2c3+11708ac4-1451bc4-9039c5+10277b4d-5290b3cd+1617b2c2d+7618ac3d+10862bc3d+8083c4d-8817b3d2+2614b2cd2+13113ac2d2-2647bc2d2-12583c3d2+abd3+14772b2d3+2159acd3+10258bcd3-7201c2d3+14532ad4+8861bd4-11544cd4+10827d5 ,
       -14951b5+5071b4c-2048b3c2+6623b2c3-7812ac4+1476bc4+15185c5+11004b4d+7056b3cd+3940b2c2d+15163ac3d+12741bc3d+10224c4d-1114b3d2+abcd2-7434b2cd2-8246ac2d2+5953bc2d2+11198c3d2+15012abd3+12412b2d3+9735acd3-2790bcd3-13489c2d3+8399ad4+10062bd4+6089cd4-4501d5 ,
       -11512b5-1130b4c+1215b3c2-7078b2c3-4312ac4-9824bc4+12811c5+4534b4d+6024b3cd+abc2d+6920b2c2d-8141ac3d+13005bc3d+8526c4d+1412b3d2-15512abcd2-14940b2cd2+8735ac2d2+11988bc2d2+4704c3d2-3495abd3+1975b2d3+13586acd3-4532bcd3+7769c2d3-15951ad4-4713bd4+13541cd4+8580d5 ,
       -8165b5+3347b4c-657b3c2+abc3+13057b2c3-650ac4-8260bc4-5669c5-7036b4d+4503b3cd+3581abc2d-13288b2c2d-15193ac3d+1236bc3d+1414c4d+6058b3d2-862abcd2+6870b2cd2-8409ac2d2+3977bc2d2+2362c3d2-11087abd3-8834b2d3-10289acd3+9404bcd3-1755c2d3+11691ad4+8169bd4-13754cd4-10987d5 ,
       -12135b5+10882b4c+7998b3c2-9553abc3-14273b2c3+13697ac4-513bc4-10249c5+1115b4d+2518b3cd-6775abc2d-4249b2c2d-12272ac3d-5519bc3d+11912c4d+ab2d2+15502b3d2-6208abcd2-5884b2cd2+11557ac2d2+6896bc2d2+13231c3d2+5762abd3+13391b2d3-4360acd3+1763bcd3-4498c2d3+2699ad4-4335bd4+9356cd4+11447d5 ,
       8918b4+ab2c+3324b3c-5938abc2-1269b2c2-714ac3+4194bc3-3909c4+3426ab2d-2298b3d+3692abcd+8740b2cd-11336ac2d+5577bc2d-6747c3d+521abd2-5619b2d2-1315acd2+9854bcd2+3835c2d2+13754ad3-5456bd3+10640cd3+15586d4 ,
       ab3+8614b4-11880ab2c-5345b3c+8392abc2-7389b2c2+3568ac3+14805bc3+10461c4-5786ab2d+15610b3d-12600abcd-15254b2cd+14412ac2d+13915bc2d+4969c3d-7535abd2-12322b2d2+959acd2-6599bcd2+173c2d2-10365ad3-8028bd3-7230cd3-12147d4 ,
       -11347ab3+14644b4+13884ab2c+11384b3c-589abc2+7910b2c2-5304ac3+11727bc3-3264c4+3546ab2d+73b3d-6143abcd+7395b2cd+4247ac2d-2073bc2d-11045c3d+a2d2-5394abd2-15764b2d2+2926acd2+15866bcd2+1841c2d2-9946ad3+1624bd3+209cd3+9366d4 ,
       -4550ab2+9975b3+a2c-8765abc-11257b2c+12705ac2-9665bc2+5132c3-9825a2d+15512abd-8580b2d+11687acd-1066bcd+8006c2d+6810ad2-12535bd2+9630cd2+6727d3 ,
       a2b+2015b3-1648a2c-3910abc-4666b2c+1001ac2-8007bc2+345c3-4404a2d-1027abd-9718b2d+4411acd+11194bcd-3723c2d-13499ad2-4299bd2+159cd2+9012d3 ,
       a3-312b3+1186a2c-14772abc-7b2c-4827ac2+6171bc2+12015c3+11880a2d-4942abd+9929b2d-589acd+10389bcd+6044c2d+5889ad2+7982bd2-14415cd2-8240d3
       ",
       ideal " c2-13428cd+10884d2 , b-12327c+2244d , a-5947c+15464d "
       }
  checkMinimalPrimes(I, C1, "Answer"=>singularList)
  --set1 := C1/(i -> set flatten entries gens gb i)//set;
  --C2 := singularList;
  --set2 := C2/(i -> set flatten entries gens gb i)//set;
  --assert(set1 === set2)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,h]
  I = ideal(a+b+c,a*b+b*c+a*c,a*b*c-h^3)
  C = minprimes I;
  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
  -- TODO: Change the below tests to work with the new
  -- strategies.  Need one test for each of the built-in
  -- strategies.
  -*
  C1 = minprimes( I, "UseColon" => false);
  checkMinimalPrimes(I, C1, "Answer" => decompose)
  C2 = minprimes( I, "SimplifyIdeal" => false);
  checkMinimalPrimes(I, C2, "Answer" => decompose)
  C3 = minprimes( I, "FactorizationSplit" => true );
  checkMinimalPrimes(I, C3, "Answer" => decompose)
  *-
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,h]
  I = ideal(a+b+c+d,a*b+b*c+c*d+d*a,a*b*c+b*c*d+c*d*a+d*a*b,a*b*c*d-h^4)
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d,h]
  I = ideal(a+b+c+d,a*b+b*c+c*d+d*a,a*b*c+b*c*d+c*d*a+d*a*b,a*b*c*d-h^4)
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d]
  I = ideal(a^2-b^2,a*b*c-d^3,b*d^2-a*c^2)
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d]
  I = ideal(a^2-b^2,a*b*c-d^3,b*d^2-a*c^2)
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = ZZ/32003[x,y,z,MonomialOrder=>Lex]
  p = z^2+1
  q = z^4+2
  I = ideal(p^2*q^3, (y-z^3)^3, (x-y*z+z^4)^4)
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  -- ST_S/Y x, except that one is ZZ/32003
  R = QQ[b,s,t,u,v,w,x,y,z];
  I = ideal"su - bv, tv - sw, vx - uy, wy - vz"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = QQ[vars(0..8)];
  I = ideal(b*d+a*e,c*d+a*f,c*e+b*f,b*g+a*h,c*g+a*i,c*h+b*i,e*g+d*h,f*g+d*i,f*h+e*i)
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = QQ[vars(0..8),MonomialOrder=>Lex];
  I = ideal(b*d+a*e,c*d+a*f,c*e+b*f,b*g+a*h,c*g+a*i,c*h+b*i,e*g+d*h,f*g+d*i,f*h+e*i)
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = ZZ/32003[x,y,z];
  I = ideal"
    x2yz + xy2z + xyz2 + xyz + xy + xz + yz,
    x2y2z + xy2z2 + x2yz + xyz + yz + x + z,
    x2y2z2 + x2y2z + xy2z + xyz + xz + z + 1";
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = ZZ/32003[x,y,z,t]
  I = ideal(
    t^10-x,
    t^31-t^6-t-y,
    t^8-z)
  C = minprimes I
  -- decompose fails miserably here.
  -- checkMinimalPrimes(I, C, "CheckPrimality" => true) -- too slow!
  assert (I == first C)
  assert (#C === 1)
  -- it's a bit strange though, since minprimes returns a seemingly
  -- different ideal, but if you check I == first C, you get true
  -- even though they look very different.
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --chemistry: a chemical process in glass melting (DGP set) 9 variables
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[a,b,c,d,e,f,g,h,j];
  I = ideal"
    a+2b+c-d+g,
    f2gh - a,
    efg - c,
    fg2j - b,
    a + b + c + f + g - 1,
    3ad + 3bd + 2cd + df + dg - a - 2b - c - g"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --horrocks (DGP) related to the Horrock bundle on P5 x
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[a,b,c,d,e,f];
  I = ideal"2adef + 3be2f - cef2,         4ad2f + 5bdef + cdf2,         2abdf + 3b2ef - bcf2,                     4a2df + 5abef + acf2,
            4ad2e + 3bde2 + 7cdef,        2acde + 3bce2 - c2ef,         4abde + 3b2e2 - 4acdf + 2bcef - c2f2,     4a2de + 3abe2 + 7acef,
            4acd2 + 5bcde + c2df,         4abd2 + 3b2de + 7bcdf,        16a2d2 - 9b2e2 + 32acdf - 18bcef + 7c2f2, 2abcd + 3b2ce - bc2f,
            4a2cd + 5abce + ac2f,         4a2bd + 3ab2e + 7abcf,        abc2f - cdef2,                            ab2cf - bdef2,
            2a2bcf + 3be2f2 - cef3,       ab3f - 3bdf3,                 2a2b2f - 4adf3 + 3bef3 - cf4,             a3bf + 4aef3,
            3ac3e - cde3,                 3b2c2e - bc3f + 2cd2ef,       abc2e - cde2f,                            6a2c2e - 4ade3 - 3be4 + ce3f,
            3b3ce - b2c2f + 2bd2ef,       2a2bce + 3be3f - ce2f2,       3a3ce + 4ae3f,                            4bc3d + cd3e,
            4ac3d - 3bc3e - 2cd2e2 + c4f, 8b2c2d - 4ad4 - 3bd3e - cd3f, 4b3cd + 3bd3f,                            4ab3d + 3b4e - b3cf - 6bd2f2,
            4a4d + 3a3be + a3cf - 8ae2f2"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --square of a generic 3x3 matrix (DGP, from POSSO)
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[vars(0..8)]
  I = ideal (genericMatrix(R,3,3))^2
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --shimoyama-yokoyama example I8 (DGP)
  needsPackage "MinimalPrimes"
  kk = QQ
  R = kk[b,c,d,e,f,g,h,j,k,l];
  I = ideal(
    (l-k)^9,
    (l-k)^8*(l-b),
    (l-k)^7*(l-c),
    (l-k)^6*(l-d),
    (l-k)^5*(l-e),
    (l-k)^4*(l-f),
    (l-k)^3*(l-g),
    (l-k)^2*(l-h),
    (l-k)*(l-j))
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --riemenschneider (DGP) related to deformations of quotient singularities
  needsPackage "MinimalPrimes"
  kk = QQ
  R = kk[p,q,s,t,u,v,w,x,y,z];
  I = ideal"
    su,
    vx,
    qu,
    xz,
    stx + ux,
    uv3 - uvw + ux,
    -pu2v2 + pu2w + qtx,
    tx2y - uv2z + uwz"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2 x
  --sy-j: shimoyama-yokoyama example J (DGP) 3 variables (J_S/Y) x
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[x,y,z];
  I = ideal"
    xy2z2 - xy2z + xyz2 - xyz,
    xy3z + xy2z,
    xy4 - xy2,
    x2yz2 - x2yz,
    x2y3 - x2y2,
    x4z3 - x4z2 + 2x3z3 - 2x3z2 + x2z3 - x2z2,
    x2y2z,
    x4yz + x3yz,
    2x4y2 + 6x3y2 + 6x2y2 + xy3 + xy2,
    x5z + x4z2 + x4z + 2x3z2 - x3z + x2z2 - x2z,
    x6y + 3x5y + 3x4y + x3y"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --roczen (DGP) related to classification of singularities (Marko) x
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[a,b,c,d,e,f,g,h,k,o];
  I = ideal "o+1,k4+k,hk,h4+h,gk,gh,g3+h3+k3+1,fk,f4+f,eh,ef,f3h3+e3k3+e3+f3+h3+k3+1,e3g+f3g+g,e4+e,dh3+dk3+d,dg,df,de,
             d3+e3+f3+1,e2g2+d2h2+c,f2g2+d2k2+b,f2h2+e2k2+a"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --macaulay (DGP, from an older M2 tutorial)
  needsPackage "MinimalPrimes"
  kk = QQ
  R = kk[a,b,c,d]
  I = ideal"
  b4 - a3d,
  ab3 - a3c,
  bc4 - ac3d - bcd3 + ad4,
  c6 - bc3d2 - c3d3 + bd5,
  ac5 - b2c3d - ac2d3 + b2d4,
  a2c4 - a3d3 + b3d3 - a2cd3,
  b3c3 - a3d3,
  ab2c3 - a3cd2 + b3cd2 - ab2d3,
  a2bc3 - a3c2d + b3c2d - a2bd3,
  a3c3 - a3bd2,
  a4c2 - a3b2d"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --becker-niermann (DGP)
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[x,y,z];
  I = ideal"
    x2+xy2z-2xy+y4+y2+z2,
    -x3y2+xy2z+xyz3-2xy+y4,
    -2x2y+xy4+yz4-3"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
--from ExampleIdeals/DGP.m2
--caprasse4 (DGP, from POSSO)
  needsPackage "MinimalPrimes"
  kk = QQ
  R = kk[x,y,z,t];
  I = ideal"
    y2z+2xyt-2x-z,
    -x3z+4xy2z+4x2yt+2y3t+4x2-10y2+4xz-10yt+2,
    2yzt+xt2-x-2z,
    -xz3+4yz2t+4xzt2+2yt3+4xz+4z2-10yt-10t2+2"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  -- decompose DNF over QQ.  Coeffs probably too nasty
  R = kk[b,c,d,e]
  I = ideal"
    6b4c3 + 21b4c2d + 15b4cd2 + 9b4d3 - 8b2c2e - 28b2cde + 36b2d2e - 144b2c
      - 648b2d - 120,
    9b4c4 + 30b4c3d + 39b4c2d2 + 18b4cd3 - 24b2c3e - 16b2c2de
      + 16b2cd2e + 24b2d3e
      - 432b2c2 - 720b2cd - 432b2d2 + 16c2e2 - 32cde2 + 16d2e2 + 576ce - 576de
      - 240c + 5184,
    -15b2c3e + 15b2c2de - 81b2c2 + 216b2cd - 162b2d2 + 40c2e2 - 80cde2
      + 40d2e2 + 1008ce - 1008de + 5184,
    -4b2c2 + 4b2cd - 3b2d2 + 22ce - 22de + 261"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --moeller (DGP)
  needsPackage "MinimalPrimes"
  kk = QQ
  R = kk[a,b,c,d,u,v,w,x];
  I = ideal"
    a + b + c + d,
    u + v + w + x,
    3ab + 3ac + 3bc + 3ad + 3bd + 3cd + 2,
    bu + cu + du + av + cv + dv + aw + bw + dw + ax + bx + cx,
    bcu + bdu + cdu + acv + adv + cdv + abw + adw + bdw + abx + acx + bcx,
    abc + abd + acd + bcd,
    bcdu + acdv + abdw + abcx"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --buchberger (DGP, from POSSO)
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[a,b,c,d,x,y,z,t];
  I = ideal"
  t - b - d,
  x + y + z + t - a - c - d,
  xz + yz + xt + zt - ac - ad - cd,
  xzt - acd"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --lanconelli (DGP, from POSSO)
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  R = kk[a,b,c,d,e,f,g,h,j,k,l];
  I = ideal"
    a + c + d + e + f + g + h + j - 1,
    -c2k - 2cdk - d2k - cek - dek - cfk - dfk - cgk -
      dgk - egk - fgk - chk - dhk - ehk - fhk + c + d,
    -c2l-cdl-cel-cfl-cgl-dgl-egl-fgl+c2+2cd+d2+cg+dg+ch+dh,
    -b + c + e + g + j"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  --from ExampleIdeals/DGP.m2
  --wang2 (DGP)
  needsPackage "MinimalPrimes"
  kk = QQ
  R = kk[t,x,y,z];
  I = ideal"
  x2 + y2 + z2 - t2,
  xy + z2 - 1,
  xyz - x2 - y2 - z + 1"
  C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

--------------------------
-- from slower-tests.m2 --
--------------------------
SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = QQ[a,b,c,d,e,h]
  I = ideal(
     a+b+c+d+e,
	 d*e+c*d+b*c+a*e+a*b,
	 c*d*e+b*c*d+a*d*e+a*b*e+a*b*c,
	 b*c*d*e+a*c*d*e+a*b*d*e+a*b*c*e+a*b*c*d,
	 a*b*c*d*e-h^5)
   time C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

SIMPLETEST ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,e,h]
  I = ideal(
     a+b+c+d+e,
	 d*e+c*d+b*c+a*e+a*b,
	 c*d*e+b*c*d+a*d*e+a*b*e+a*b*c,
	 b*c*d*e+a*c*d*e+a*b*d*e+a*b*c*e+a*b*c*d,
	 a*b*c*d*e-h^5)
   time C = minprimes(I, Verbosity=>2)
   --time C = minprimes(I, Strategy=>null, Verbosity=>2)

   --time C = splitIdeal(I, Strategy=>Hybrid{defaultStrat,stratEnd}, Verbosity=>2);
   --select(C, c -> (gens I) % (ideal c) != 0)

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

TOODAMNSLOW ///
  --- Now quite fast, no longer TOODAMNSLOW
  needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l,MonomialOrder=>Lex]
    R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l]
  I = ideal "-2hjk + 4ef + bj + ak,
           -2hjl + 4eg + cj + al,
           -4fhj - 4ehk - djk + 2be + 2af,
           -4ghj - 4ehl - djl + 2ce + 2ag,
           -2dfj - 2dek + ab,
           -2dgj - 2del + ac"
   time C = minprimes I
   -- this ideal is radical
   assert(intersect C == I)
   --checkMinimalPrimes(I, C, "CheckPrimality" => true) -- takes WAY too long to use as a test
   --checkMinimalPrimes(I, C, "Answer" => decompose) -- takes too long to use as a test
   assert false -- need to put some actual tests in here
///

SIMPLETEST ///
  needsPackage "MinimalPrimes"
  R = ZZ/32003[x,y,z,t,MonomialOrder=>Lex]
  I = ideal(
     y^2*z+2*x*y*t-2*x-z,
     -x^3*z+4*x*y^2*z+4*x^2*y*t+2*y^3*t+4*x^2-10*y^2+4*x*z-10*y*t+2,
     2*y*z*t+x*t^2-x-2*z,
     -x*z^3+4*y*z^2*t+4*x*z*t^2+2*y*t^3+4*x*z+4*z^2-10*y*t-10*t^2+2)
  time C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

TOODAMNSLOW ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,e,f,h,MonomialOrder=>Lex]
  R = ZZ/32003[a,b,c,d,e,f,h]
  I = ideal(
         a+b+c+d+e+f,
	 a*b+b*c+c*d+d*e+e*f+a*f,
	 a*b*c+b*c*d+c*d*e+d*e*f+e*f*a+f*a*b,
	 a*b*c*d+b*c*d*e+c*d*e*f+d*e*f*a+e*f*a*b+f*a*b*c,
	 a*b*c*d*e+b*c*d*e*f+c*d*e*f*a+d*e*f*a*b+e*f*a*b*c+f*a*b*c*d,
	 a*b*c*d*e*f-h^6)

  time minprimes(I, Verbosity=>2);

  debug MinimalPrimes
  checkMinimalPrimes(I,C)
///

BENCHMARK ///
  debug needsPackage "MinimalPrimes"
  R = ZZ/32003[a,b,c,d,e,f,g,h,j,k,l]
  I = ideal(h*j*l-2*e*g+16001*c*j+16001*a*l,h*j*k-2*e*f+16001*b*j+16001*a*k,h*j^2+2*e^2+16001*a*j,d*j^2+2*a*e,g*h*j+e*h*l+8001*d*j*l+16001*c*e+16001*a*g,f*h*j+e*h*k+8001*d*j*k+16001*b*e+16001*a*f
          ,e*g*j+8001*c*j^2+e^2*l,d*g*j+d*e*l+16001*a*c,e*f*j+8001*b*j^2+e^2*k,d*f*j+d*e*k+16001*a*b,d*e*j-a*h*j-16001*a^2,d*e^2-a*e*h-8001*a*d*j,d*g*k*l-c*h*k*l-d*f*l^2+b*h*l^2-2*c*f*g+2*b*g^2-16001
       	  *c^2*k+16001*b*c*l,d*g*k^2-c*h*k^2-d*f*k*l+b*h*k*l-2*c*f^2+2*b*f*g-16001*b*c*k+16001*b^2*l,d*g^2*k-c*g*h*k-d*f*g*l+c*f*h*l-8001*c*d*k*l+8001*b*d*l^2+16001*c^2*f-16001*b*c*g,d*f*g*k-b*g*h*k-
       	  8001*c*d*k^2-d*f^2*l+b*f*h*l+8001*b*d*k*l+16001*b*c*f-16001*b^2*g,c*f*g*k-b*g^2*k-8001*c^2*k^2-c*f^2*l+b*f*g*l-16001*b*c*k*l-8001*b^2*l^2,e^2*g*k+8001*c*e*j*k-e^2*f*l-8001*b*e*j*l,d*g*h*l^2
       	  -c*h^2*l^2-8001*d^2*l^3+2*d*g^3-2*c*g^2*h+16000*c*d*g*l+c^2*h*l-8001*c^3,d*f*h*l^2-b*h^2*l^2-8001*d^2*k*l^2+2*d*f*g^2-2*b*g^2*h+16001*c*d*g*k+16001*c*d*f*l+16001*b*d*g*l+b*c*h*l-8001*b*c^2,
       	  d*f*h*k*l-b*h^2*k*l-8001*d^2*k^2*l+2*d*f^2*g-2*b*f*g*h+16001*c*d*f*k+16001*b*d*g*k-16001*b*c*h*k+16001*b*d*f*l-16001*b^2*h*l-8001*b^2*c,d*f*h*k^2-b*h^2*k^2-8001*d^2*k^3+2*d*f^3-2*b*f^2*h+
       	  16000*b*d*f*k+b^2*h*k-8001*b^3)
  time C = minprimes I
  assert(#C == 1)
  assert(C#0 == I)

  -- here is an independent check that I is prime:
  assert(I : (j*l) == I)
  I1 = eliminate(I, h)
  assert (codim I1 == 3)
  assert(I : e == I)
  I1 = eliminate(I, {h,b,j})
  assert(codim I1 == 1)
  facs = factors I1_0
  assert (#facs == 1)
  assert (facs#0#0 == 1)
  -- this I1 is irreducible over the field, of codim 1, and is birational to I, therefore I is prime
///

BENCHMARK ///
  --from ExampleIdeals/DGP.m2
  needsPackage "MinimalPrimes"
  kk = ZZ/32003
  --butcher (DGP) (up to a change of coordinates, this appears to be Bu_S/Y (Wang2)) x
  R = kk[a,b,c,d,e,f,g,h];
  I = ideal"
    a + c + d - e - h,
    2df + 2cg + 2eh - 2h2 - h - 1,
    3df2 + 3cg2 - 3eh2 + 3h3 + 3h2 - e + 4h,
    6bdg - 6eh2 + 6h3 - 3eh + 6h2 - e + 4h,
    4df3 + 4cg3 + 4eh3 - 4h4 - 6h3 + 4eh - 10h2 - h - 1,
    8bdfg + 8eh3 - 8h4 + 4eh2 - 12h3 + 4eh - 14h2 - 3h - 1,
    12bdg2 + 12eh3 - 12h4 + 12eh2 - 18h3 + 8eh - 14h2 - h - 1,
    -24eh3 + 24h4 - 24eh2 + 36h3 - 8eh + 26h2 + 7h + 1"
  time C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose) -- decompose is much faster on this one
///

BENCHMARK  ///
  debug needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --gonnet (DGP) (I think this is: Go_S/Y, with change of coordinates) x
  R = kk[a,b,c,d,e,f,g,h,j,k,l,m,n,o,p,q,s];
  I = ideal "
    ag,
    gj + am + np + q,
    bl,
    nq,
    bg + bk + al + lo + lp + b + c,
    ag + ak + jl + bm + bn + go + ko + gp + kp + lq + a + d + f + h + o + p,
    gj + jk + am + an + mo + no + mp + np + gq + kq + e + j + q + s - 1,
    jm + jn + mq + nq,
    jn + mq + 2nq,
    gj + am + 2an + no + np + 2gq + kq + q + s,
    2ag + ak + bn + go + gp + lq + a + d,
    bg + al,
    an + gq,
    2jm + jn + mq,
    gj + jk + am + mo + 2mp + np + e + 2j + q,
    jl + bm + gp + kp + a + f + o + 2p,
    lp + b,
    jn + mq,
    gp + a
    "
  time C = minprimes I -- 1.14 sec
  time C1 = decompose I -- .14 sec

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)

  -- it does simplifyIdeal before the work of minprimes now.
  --time simplifyIdeal I
  --time C2 = minprimes first oo -- .16 sec
///


SIMPLETEST ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --schwarz (DGP) constructing idempotents in group theory x
  R = kk[a,b,c,d,e,h];
  I = ideal"
    -ab - b2 - 2de - 2ch,
    -ac - 2bc - e2 - 2dh,
    -c2 - ad - 2bd - 2eh,
    -2cd - ae - 2be - h2,
    -d2 - 2ce - ah - 2bh
    "
  time C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///


TOODAMNSLOW ///
  debug needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --dejong (DGP) related to the base space of a semi-universal deformation
  -- of a rational quadruple point (same as Theo1, after change of coord) x
  R = kk[a,b,c,d,e,f,g,h,j,k,l]
  I = ideal"-2hjk + 4ef + bj + ak,
    -2hjl + 4eg + cj + al,
    -4fhj - 4ehk - djk + 2be + 2af,
    -4ghj - 4ehl - djl + 2ce + 2ag,
    -2dfj - 2dek + ab,
    -2dgj - 2del + ac"
  time C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose) -- DECOMPOSE TOO SLOW HERE??  but it does work eventually

  C = time minprimes(I,Strategy=>Hybrid{Linear,Birational,Factorization,DecomposeMonomials,Linear,Factorization});    -- 1.25 sec
///


SIMPLETEST ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --gerdt (DGP, from POSSO)
  R = kk[t,u,v,w,x,y,z];
  I = ideal"2tw + 2wy - wz,
    2uw2 - 10vw2 + 20w3 - 7tu + 35tv - 70tw,
    6tw2 + 2w2y - 2w2z - 21t2 - 7ty + 7tz,
    2v3 - 4uvw - 5v2w + 6uw2 + 7vw2 - 15w3 - 42vy,
    6tw + 9wy + 2vz - 3wz - 21x,
    9uw3-45vw3+135w4+14tv2-70tuw+196tvw-602tw2-14v2z+28uwz+
      14vwz - 28w2z + 147ux - 735vx + 2205wx - 294ty + 98tz + 294yz - 98z2,
    36tw3+6w3y-9w3z-168t2w-14v2x+28uwx+14vwx-28w2x-28twy+
      42twz + 588tx + 392xy - 245xz,
    2uvw - 6v2w - uw2 + 13vw2 - 5w3 - 28tw + 14wy,
    u2w - 3uvw + 5uw2 - 28tw + 14wy,
    tuw + tvw - 11tw2 - 2vwy + 8w2y + uwz - 3vwz + 5w2z - 21wx,
    5tuw-17tvw+33tw2-7uwy+22vwy-39w2y-2uwz+6vwz-10w2z+63wx,
    20t2w - 12uwx + 30vwx - 15w2x - 10twy - 8twz + 4wyz,
    4t2w - 6uwx + 12vwx - 6w2x + 2twy - 2wy2 - 2twz + wyz,
    8twx + 8wxy - 4wxz"
  time C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///


SIMPLETEST ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --mikro (DGP) from analyzing analog circuits
  R = kk[a,b,c,d,e,f,g,h]
  I = ideal"
  59ad + 59ah + 59dh - 705d - 1199h,
  330acde + 330aceh + 330cdeh - 407acd - 1642ade - 1410cde
    - 407ach - 407cdh - 1642aeh - 2398ceh - 1642deh,
  -483acd - 483ach - 483cdh + 821ad + 705cd + 821ah + 1199ch + 821dh,
  13926abcde + 13926abceh + 13926bcdeh - 9404abcd - 9239abde
    - 4968acde - 13157bcde - 9404abch - 9404bcdh - 9239abeh
    - 4968aceh - 13025bceh - 9239bdeh - 4968cdeh,
  -cde - 377cdh - ceh - deh,
  -54acf - 54adf + a + d,
  adfg + a + d"
  time C = minprimes I -- slower than decompose

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///


TOODAMNSLOW ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --amrhein (DGP)
  R = kk[a,b,c,d,e,f];
  I = ideal"
  a2 + d2 + 2ce + 2bf + a,
  2ab + 2de + 2cf + b,
  b2 + 2ac + e2 + 2df + c,
  2bc + 2ad + 2ef + d,
  c2 + 2bd + 2ae + f2 + e,
  2cd + 2be + 2af + f"
  time C = minprimes(I,Verbosity=>2)


  debug MinimalPrimes
  time checkMinimalPrimes(I,C);
  --checkMinimalPrimes(I, C, "Answer" => decompose) -- decompose is TOO slow here
  -- TODO: need to be able to check this answer
  assert false
///

TEST ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/5
  --huneke (DGP)
  R = kk[s,t,u,x,y]
  I = ideal"
  s15,
  t15,
  u15,
  u5 - s3tx + s2t2x + s2t2y - st3y"
  time C = minprimes I
  time decompose I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

TEST ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --wang1 (DGP)
  R = kk[a,b,c,d,e,f,g,h,k,l];
  I = ideal"
  f2h-1,
  ek2 - 1,
  g2l - 1,
  2ef2g2hk2 + f2g2h2k2 + 2ef2g2k2l + 2f2g2hk2l + f2g2k2l2 + ck2,
  2e2fg2hk2 +2efg2h2k2 +2e2fg2k2l+4efg2hk2l+2fg2h2k2l+2efg2k2l2
    + 2fg2hk2l2 + 2bfh,
  2e2f2ghk2 +2ef2gh2k2 +2e2f2gk2l+4ef2ghk2l+2f2gh2k2l+2ef2gk2l2
    + 2f2ghk2l2 + 2dgl,
  e2f2g2k2 + 2ef2g2hk2 + 2ef2g2k2l + 2f2g2hk2l + f2g2k2l2 + bf2,
  2e2f2g2hk +2ef2g2h2k +2e2f2g2kl+4ef2g2hkl+2f2g2h2kl+2ef2g2kl2
    + 2f2g2hkl2 + 2cek,
  e2f2g2k2 + 2ef2g2hk2 + f2g2h2k2 + 2ef2g2k2l + 2f2g2hk2l + dg2,
  -e2f2g2hk2-ef2g2h2k2-e2f2g2k2l-2ef2g2hk2l-f2g2h2k2l-ef2g2k2l2
    - f2g2hk2l2 + a2"
  time C = minprimes(I,Verbosity=>2)

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

TEST ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --siebert (DGP)
  R = kk[t,w,x,y,z];
  I = ideal"
  w2xy + w2xz + w2z2,
  tx2y + x2yz + x2z2,
  twy2 + ty2z + y2z2,
  t2wx + t2wz + t2z2"
  time C = minprimes(I, Verbosity => 2)

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

TEST ///
  debug needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/101
  --amrheim2 (DGP)
  R = kk[a,b,c,d,e,f,g];
  I = ideal"
  a2 + 2de + 2cf + 2bg + a,
  2ab + e2 + 2df + 2cg + b,
  b2 + 2ac + 2ef + 2dg + c,
  2bc + 2ad + f2 + 2eg + d,
  c2 + 2bd + 2ae + 2fg + e,
  2cd + 2be + 2af + g2 + f,
  d2 + 2ce + 2bf + 2ag + g"
  time C = minprimes(I,Strategy=>Hybrid{NoBirationalStrat}, Verbosity=>2);
  --time C = splitIdeal(I,Strategy=>Hybrid{defaultStrat,stratEnd}, Verbosity=>2);
  --time C = minprimes(I, Verbosity=>2);   -- the extra time is in the conversion
                                          -- from annotated ideal to ideal caused by
                                          -- the 'Linears' added in calls to Birational
                                          -- Specifically, the GB computation once
                                          -- we throw in the birational elements.
  --time C = minprimes(I,Strategy=>null); -- Note this is the old minprimes call, same
                                          -- time as NoBirationalStrat above.

  checkMinimalPrimes(I,C)
  --checkMinimalPrimes(I, C, "Answer" => decompose) -- decompose is TOO long here.
  --TODO: need to get an answer for this one to check against
///

TEST ///
  needsPackage "MinimalPrimes"
  --from ExampleIdeals/DGP.m2
  kk = ZZ/3
  --huneke2 (not in published DGP) -- over ZZ/3 is real test
  R = kk[x,y,u,s,t];
  I = ideal"
  x27,
  y27,
  u27,
  u5-xy(x-y)(sx-ty)"
  time C = minprimes I

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose) -- immediate
///

TEST ///
  needsPackage "MinimalPrimes"
  -- DGP Wang
  R = QQ[a,b,c,d,f,g,h,k,l,s,t,u,v,w,x,y,z]
  I = ideal"
    -ab-ad+2ah,
    ad-bd-cf-2ah+2bh+2ck,
    ab-ad-2bh+2dh-2ck+2fk+2gl,
    ac-2cs-at+2bt,
    ac-cs-2at+bt,
    -d-3s+4u,
    -f-3t+4v,
    -g+4w,
    -a+2x,
    -b2-c2+2bx+2cy,
    -d2-f2-g2+2dx+2fy+2gz"

  time C = minprimes(I,Verbosity=>2)

  debug MinimalPrimes
  checkMinimalPrimes(I, C, "Answer" => decompose)
///

-- above are from slower-tests.m2 --

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[e_1, e_2, e_3, e_4, g_1, g_2, g_3, g_4, r]
  I = trim ideal(0_R)
  minprimes I == {I}
///

TEST ///
  needsPackage "MinimalPrimes"
  R = QQ[e_1, e_2, e_3, e_4, g_1, g_2, g_3, g_4, r]
  I = ideal(r^2-3,e_2^2-3*e_3^2,e_1^2+6*e_3^2+e_4^2)
  time minprimes I  -- 17.93 sec, Mike's rMBP, 19 Nov 2012
  time decompose I  -- .01 sec!!

  R1 = QQ[support I]
  I1 = sub(I,R1)
  time minprimes I1  -- .27 sec, Mike's rMBP, 19 Nov 2012
///

TEST ///
  needsPackage "MinimalPrimes"

  -- this example is one step from the stewart-gough example
  R = QQ[e_1, e_2, e_3, e_4, g_1, g_2, g_3, g_4, r]
  J = ideal(r^2-3,
       g_3*r+e_1,
       e_1*r+3*g_3,
       e_4*g_3-e_3*g_4,
       g_2^2-3*g_3^2,
       e_3*g_2-e_2*g_3,
       e_2*g_2-3*e_3*g_3,
       e_1*g_1+4*e_3*g_3+e_4*g_4,
       2*e_4^2+9*g_1^2+24*g_3^2+9*g_4^2,
       4*e_3^2-3*g_1^2-6*g_3^2-3*g_4^2,
       4*e_2^2-9*g_1^2-18*g_3^2-9*g_4^2,
       e_1^2-3*g_3^2,
       e_4*g_2*r-e_2*g_4*r,
       g_1^2*r+g_4^2*r+2*e_3*g_1-4*e_1*g_3,
       e_4*g_1*r+2*e_3*e_4+3*g_3*g_4,
       2*e_3*g_1*r+3*g_1^2+12*g_3^2+3*g_4^2,
       2*e_3*e_4*r+3*e_4*g_1-3*e_1*g_4,
       2*e_2*e_3*r+3*e_2*g_1-3*e_1*g_2,
       6*e_3*g_1*g_3-4*e_1*g_3^2+e_4*g_1*g_4-e_1*g_4^2,
       6*e_2*e_3*g_3+3*g_2*g_3^2+e_2*e_4*g_4,
       e_1*e_4*g_2-e_1*e_2*g_4,
       2*e_1*e_3*e_4-3*e_3*g_1*g_4+3*e_1*g_3*g_4,
       2*e_1*e_2*e_4-3*e_2*g_1*g_4+3*e_1*g_2*g_4,
       2*e_1*e_2*e_3-3*e_2*g_1*g_3+3*e_1*g_2*g_3)
  time minprimes(J,Verbosity=>2);
  -*
  C = splitIdeal(J,Strategy=>Hybrid{defaultStrat,(IndependentSet,infinity)}, Verbosity=>2)
  C / isPrime
  J2 = C#2
  splitIdeal(J2,Strategy=>Hybrid{SplitTower}, Verbosity=>2)
  use ring (J2.LexGBOverBase)_0
  use coefficientRing ring (J2.LexGBOverBase)_0
  factorOverTower({(J2.LexGBOverBase)_0},g_1^2-3*g_4^2)
  factorOverTower({(J2.LexGBOverBase)_0},r*(g_1^2-3*g_4^2)) -- r changed rings?
  J2 = C#5
  use ring (J2.LexGBOverBase)_0
  use coefficientRing ring (J2.LexGBOverBase)_0
  lexGBOverBase = J2.LexGBOverBase
  factorOverTower({lexGBOverBase#0}, lexGBOverBase#1)
  factorOverTower(drop(lexGBOverBase,-1), lexGBOverBase#2, Verbosity=>2)
  splitIdeal(J2,Strategy=>Hybrid{SplitTower}, Verbosity=>2)
  *-
  --C = time minprimes(J,Strategy=>Hybrid{Linear,Birational,Factorization,Linear,Birational,Minprimes});
///

TEST ///
  R1 = ZZ/101[a..d]/a^2
  assert(minprimes ideal(a*d) === {ideal(a)})
  R2 = ZZ[a..d]
  assert(minprimes ideal(a*d) == ideal \ {{a}, {d}})
  assert(minprimes monomialIdeal(a*d) == monomialIdeal \ {{a}, {d}})
  R3 = ZZ
  assert(minprimes ideal 0_ZZ == {ideal 0_ZZ})
  assert(minprimes ideal 5_ZZ == {ideal 5_ZZ})
  assert(minprimes ideal 20_ZZ == {ideal 2_ZZ, ideal 5_ZZ})
  R4 = (frac R2)[x,y,z]
  assert try minprimes(ideal(a*x)) else true

  R5 = QQ[a..d]/(a^2+b^2)
  C = minprimes ideal(0_R5)
  assert (numgens C#0 === 0)
///

TEST ///
  -- what about over GF?
  kk = GF(9, Variable=>a)
  R = kk[x,y,z,w]
  I = ideal(x^2-x-a)
  minprimes I
///

-- factorizationSplit test
///
  -- TODO : Turn this into a test.
  debug needsPackage "MinimalPrimes"
  R1 = QQ[d, f, j, k, m, r, t, A, D, G, I, K];
  I1 = ideal ( I*K-K^2, r*G-G^2, A*D-D^2, j^2-j*t, d*f-f^2, d*f*j*k - m*r, A*D - G*I*K);
  facGB0(I1, set {})
  facGB I1
  J = ideal(K,G,D^2,A*D,j*m-m*t,d*m-f*m,j^2-j*t,d*f-f^2,f^2*j*k-m*r)
  facGB0(J, set {})
  facGB0(J, set {}, "UseColon"=>false)
  p1  = time factorizationSplit(I1)

  -- simplifyIdeal takes too long!
  -- Takes about .8 seconds on Frank's Machine 12/4/2012
  time p1 / simplifyIdeal;
  time p1 / simplifyIdeal2;

  p1  = time factorizationSplit(I1, "UseColon"=>false)
  p1' = time minprimes I1
  (set p1') - (set p1)
  (set p1) === (set p1')

  checkMinimalPrimes(I1,p1')
  -- uhoh!
  checkMinimalPrimes(I1,p1)
  p1 = sort apply(p1, P -> flatten entries gens gb P );
  D1 = time decompose I1;
  D1 = sort apply(D1, i -> flatten entries gens gb i );
  assert(p1 === D1)
///

TOODAMNSLOW ///
  -- Comes from newGTZ/siphon-eg.m2
  -- Large Naive Franzi example
  debug needsPackage "MinimalPrimes"
  R1 = QQ[a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,M,MonomialSize => 8]
  I1 = ideal ( I*K-K^2, r*G-G^2, A*D-D^2, k*z-z^2, m*w-w^2, j^2-j*t, d*f-f^2, p*y*M-p^2, k*w*M-w^2, j*r*M-j*t, I*J*L-J^2, B*H*L-H^2, A*E*L-E^2, b*C*L-C^2, b*B*L-b^2, t*x*L-x^2, a*u*L-a^2, m*q*L-q^2, d*g*L-g^2, o*J*K-J^2, k*s*K-K^2, i*B*H-H^2, l*F*G-G^2, v*D*F-D^2, e*z*F-z^2, r*y*F-r^2, f*s*F-f^2, j*p*F-j*t, o*D*E-E^2, r*s*D-D^2, b*i*C-C^2, s*v*y-v^2, i*j*x-x^2, i*q*w-q^2, a*o*u-a^2, i*k*r-r^2, f*g*o-g^2, h*i*n-h^2, e*i*l-l^2, c*h*i-h^2, y^2*M^2-p^2, r^2*M^2-j*t, k^2*M^2-w^2, I^2*L^2-J^2, B^2*L^2-b^2, A^2*L^2-E^2, t^2*L^2-x^2, m^2*L^2-q^2, d^2*L^2-g^2, y^2*F^2-r^2, v^2*F^2-D^2, s^2*F^2-f^2, p^2*F^2-j*t, l^2*F^2-G^2, e^2*F^2-z^2, i^2*B^2-H^2, s^2*y^2-v^2, o^2*u^2-a^2, r^2*s^2-D^2, k^2*s^2-K^2, i^2*k^2-r^2, e^2*i^2-l^2, c^2*i^2-h^2, b^2*i^2-C^2)

  time C = minprimes(I1, Verbosity=>2);
  -- This took 2 mins on Frank's machine on 12/4/2012'
  time p1 = factorizationSplit(I1, "UseColon"=>false)
  -- TODO: Play with factorization depth in this example?

  C = time minprimes(I1,Strategy=>Hybrid{Linear,Factorization,Linear,Factorization,Linear,Factorization});

  C = time minprimes(I1,Strategy=>Hybrid{Linear,Factorization,DecomposeMonomials,Linear,Factorization,DecomposeMonomials,Linear,Factorization,DecomposeMonomials}, Verbosity=>1);
    -- 190 sec
  time D = for c in C list (t := timing ideal c; if t#0 > 5. then << "ideal: " << c << endl; t#1);
  time Dmin = selectMinimalIdeals D;

  C = apply(#C, i -> (C#i,i));
  -- this is very slow because of a saturate at around ~1750,
  -- and again at ~2100, and again at ~2200 (probably the same one since we are not being
  -- careful with redundancy)
  D = C / (c -> (if c#1 % 50 == 0 then << "Computing Ideal number " << c#1 << endl; ideal c#0));

  time Dmin = selectMinimalIdeals D;
///

TOODAMNSLOW ///
   -- Comes from newGTZ/siphon-eg.m2
   -- Large Non-Naive Franzi example
  restart
  debug needsPackage "MinimalPrimes"
  needsPackage "UnitTestsPD"
  R = QQ[x3283, x3096, x2909, x1952, x319, x1683, x2321, x2921, x2855, x1370, x622, x331, x1904, x2933, x2867, x1382, x2273, x634, x343, x1916, x3319, x1647, x1394, x2285, x646, x421, x1928, x3331, x3188, x1659, x2297, x295, x433, x3271, x1940, x2309, x1671, x2254, x307];

  I = ideal(x1940*x1671-x1671^2,-x622*x343+x1671,-x1940*x2254+x2309,x2867*x2309*x1671-x2309^2,x3331*x3271*x2254-x3271^2,-x2855*x3331+x3271,x634*x433-x433^2,-x331*x295+x433,-x1928*x2254+x2297,x2867*x1659*x2297-x2297^2,x1928*x1659-x1659 ^2,-x1647*x295+x1659,-x634*x343+x1659,x3096*x3188*x2254-x3188^2,-x3096*x2855+x3188,x622*x421-x421^2,-x319*x295+x421,-x1916*x2254+x2285,x2855*x1370*x2285-x2285^2,x1904*x1394-x1394^2,-x622*x307+x1394,-x343*x646+x1647,-x1370^2+ x1370*x1916,-x646*x295+x634,-x2855*x622+x634,-x1904*x2254+x2273,x2855*x2273*x1394-x2273^2,-x646*x307+x1382,-x319*x2855+x331,-x634*x307+x1370,-x1382*x295+x1370,x2921*x2855*x2933-x2921^2,-x2909*x2855+x2921,-x1952*x2254+x2321,x1683 *x2321*x2867-x2321^2,x1952*x1683-x1683^2,-x343*x295+x1683,-x3331*x2254+x3096,x3283*x3319*x2254-x3283^2,-x2867*x3319+x3283)
  C = time minprimes(I,Strategy=>Hybrid{Linear,Factorization,Linear,Factorization});

  C = time minprimes(I,Strategy=>Hybrid{Linear,Factorization,Linear,Factorization});
  C = time minprimes(I,Strategy=>Hybrid{Linear,Factorization,DecomposeMonomials,Linear,Factorization});
  C = time minprimes(I,Strategy=>Hybrid{Linear,Factorization,DecomposeMonomials,Linear,Factorization});
///

TEST ///
  debug needsPackage "MinimalPrimes"
  kk = ZZ/7
  R = kk[x,y,t]
  I = ideal {x^7-t^2,y^7-t^2}

  T = tally for i from 0 to 30 list (
      C := minprimes I;
      #C == 1 and C_0 == ideal(y-x, x^7-t^2))
  assert(not T#?false)
///

TEST ///
  debug needsPackage "MinimalPrimes"
  R = QQ[x,y,z,w]
  I = ideal {x^2+y^2,z^2+w^2}
  assert(not detectMembership(x,I))
  assert(detectMembership(x^2+y^2,I) === null)
  gb I
  assert(detectMembership(x^2+y^2,I))
  S = QQ[a,b,c,d,Degrees=>{4:{1,1}}]
  J = ideal {a^2+b^2,c^2+d^2}
  assert(detectMembership(a,J) === null)
  gb J
  assert(not detectMembership(a,J))
  assert(detectMembership(a^2+b^2+c^2+d^2,J))
  I' = ideal {0_R}
  J' = ideal {1_R}
  assert(not detectMembership(x,I'))
  assert(detectMembership(x,J') === null)
  assert(detectMembership(0_R,I'))
  assert(detectMembership(0_R,J'))
  assert(not detectMembership(1_R,I'))
  assert(detectMembership(1_R,J') === null)
///

TEST ///
  -- this test occurs as a crash in github issue #190.
  -- seems to work after recent changes to factory.
  -- it might be too long for a test...
 needsPackage "MinimalPrimes"
  R = QQ[x,r,v,u,b, MonomialOrder=>{Lex=>5}]
  I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
  time C1 = minprimes(I, Verbosity=>2)
  assert(#oo == 2)

  R = QQ[b][u][x,r,v, MonomialOrder=>{Lex=>3}]
  I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
  time C2 = minprimes(I, Verbosity=>2)
  assert(#C2 == 2)
  C1' = for i in C1 list sub(i,R);
  assert(C1'_0 == C2_0 or C1'_0 == C2_1);
  assert(C1'_1 == C2_0 or C1'_1 == C2_1);
///

TEST ///
  R = QQ[h,l,s,z,y,x] -- notice: variables are not in order!
  J = ideal(h*l-l^2-4*l*s+h*y,6*h*s^2-6*s^2*x+l*s+l*z,3*h^2*s+3*l*s^2+3*s^2*y-3*h*s*x-2*h*s-2*h*z,3*l^3+27*l^2*s+36*l*s^2-3*h^2*y-3*l^2*y-12*h*s*y-9*l*s*y-12*s^2*y+3*h*y^2-3*l^2*x-12*l*s*x+3*h*y*x+8*h*s+8*h*z,h^3+l^2*s-h^2*x,12*l^2*s*y+60*l*s^2*y+3*h^2*y^2-12*h*s*y^2+3*l*s*y^2+12*s^2*y^2+3*s*y^3-12*l*s*y*x-3*h*y^2*x-8*h*s*y-8*l*s*y-8*h*z*y-8*l*z*y)
  P = ideal(s,l,h)
  -- TODO: exactly which part of this should be false?
  assert(not (isSubset(J, P) and codim J == codim P and all((J, P), isPrime)))
///

TEST ///
  -- from an example of PlaneCurveSingularities in ReesAlgebra
  R = ZZ/32003[x,y]
  cusp = ideal(x^2-y^3)
  mm = radical ideal singularLocus cusp
  B = first flattenRing reesAlgebra(mm)
  proj = map(B,R,{x,y})
  totalTransform = proj cusp
  D = decompose totalTransform
  assert(length oo == 2)
///

TEST ///
  -- Example by Justin Chen
  -- see https://github.com/Macaulay2/M2/issues/1025
  d = 4;
  for k in {QQ, ZZ/101, GF(81)} do (
      S = k[x_0..x_d] ** k[y_0..y_d];
      I = minors(2, genericMatrix(S, d+1, 2));
      C = minimalPrimes I;
      assert(#C == 1 and C#0 == I))
///

--- These used to be in Macaulay2Doc/functions/decompose-doc.m2

TEST ///
  R = ZZ[x,y,z]
  f = (x - 2^32 * y) * (x + 2^33 * z - 77 * y)
  d = factor f
  assert( #d == 2 and value d == f )
///

TEST ///
  R = ZZ/32003[a..h]
  I = ideal(
      -b*d^2*f*h^2+a*b*d*g*h^2,
      -a*b*d^2*e^2+c^2*g^2*h^2,
      -d^2*f*g^3+a*b*d*e^2*h)
  dec = minimalPrimes I
  E = {
      ideal(c,d),
      ideal(h,d),
      ideal(g,d),
      ideal(h,f,a),
      ideal(c,f,a),
      ideal(g,f,a),
      ideal(h,g,a),
      ideal(c,f,b),
      ideal(g,b),
      ideal(h,f,b),
      ideal(h,e,f),
      ideal(g,e,f),
      ideal(h,g,e),
      ideal(-d*f+a*g,
	  g^4-b*e^2*h,
	  -d^3*f*g+c^2*h^3,
	  -a^3*b*e^2+c^2*f^2*h^2,
      -a^2*b*d*e^2+c^2*f*g*h^2,
      -a*b*d^2*e^2+c^2*g^2*h^2)
      }
      Ds = set apply(dec, I -> gens gb I)
      Es = set apply(E, I -> gens gb I)
      assert(Ds === Es)
///

TEST ///
  -- permanents!
  R = ZZ/32003[r,s,t,u,v,w,x,y,z]
  I = ideal(
      r*v+s*u, r*w+t*u, s*w+t*v, r*y+s*x, r*z+t*x, s*z+t*y,
      u*y+v*x, u*z+w*x, v*z+w*y)
  time D = minimalPrimes I
			-- used 130.74 seconds
			-- used 127.85 seconds
			-- used 102.09 seconds
			-- used 43.06 seconds (Messollen speed up!)
			-- used 41.93 seconds
			-- used 6.87 seconds, cygnus32
			-- used 5.19 seconds, linux
			-- 82 seconds in Singular
  E = {
      ideal(u,r,y,x,z,t*v+s*w),
      ideal(z,x,y,w,u,v),
      ideal(v,u,s,w,y,t*x+r*z),
      ideal(v,s,y,x,z,t*u+r*w),
      ideal(x,y,r,s,u,v),
      ideal(v,u,r,w,x,t*y+s*z),
      ideal(v,s,r,t,y,w*x+u*z),
      ideal(z,x,y,t,r,s),
      ideal(u,s,r,t,x,w*y+v*z),
      ideal(z,x,w,t,r,u),
      ideal(s,r,t,w,z,v*x+u*y),
      ideal(v,u,t,w,z,s*x+r*y),
      ideal(z,y,w,t,s,v),
      ideal(w,t,r,s,u,v),
      ideal(t,w,y,x,z,s*u+r*v)
      }
  Ds = set apply(D, I -> gens gb I)
  Es = set apply(E, I -> gens gb I)
  assert(Ds === Es)
///

TEST ///
  R = ZZ/31991[x,y,z];
  ivd = minimalPrimes ideal (x^3-y^2,x^3-z^2,y^3-z^2);
  assert( #ivd ===  5 )
///

TEST ///
  R = ZZ/31991[x,y,z]
  I = ideal (x,y)
  J = ideal (y-1,z-1)
  K = intersect(I,J)
  ivd = minimalPrimes K
  assert( #ivd == 2 )
///

TEST ///
  -- from Wang's paper, example 8.1
  R = ZZ/5[x_1 .. x_4]
  I = ideal (
      -3*x_3*x_4 + x_2^2 - 2*x_1 + 2,
      -3*x_1^2*x_4 - 4*x_2*x_3 - 6*x_1*x_3 + 2*x_2^2 + 3*x_1*x_2,
      -3*x_3^2*x_4 - x_1*x_4 + x_2^2*x_3 + x_2)
  ivd = minimalPrimes I
  assert( #ivd === 2 )
///

TEST ///
  -- This is a case where P1 factors.
  R = ZZ/109[x,y,z]
  I = ideal ((x-1)^2-(x-1)-3)
  J = ideal (y-1,z^2-z-3)
  P1 = ideal (x^2-x-3,y^2-y-3,z-13)
  P2 = ideal (x-13,y-55,z-12)
  K = intersect(I,J,P1,P2)
  ivd = minimalPrimes K
///

TEST ///
  R = ZZ/31991[x,y]
  assert( (x^2-10748*y*x+y^2)*(y^2+x^2)*(x^2+10748*y*x+y^2) == x^6 + y^6 )
  assert ( # factor (x^6 + y^6) == 3 )
///

TEST /// -- test of makeFiberRings
  debug needsPackage "MinimalPrimes"
  R = QQ[x,r,v,u,b, MonomialOrder=>{Lex=>5}]
  I = ideal(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b,x^2+(b-2)*x*r+r^2+b^2-4*b)
  (S, SF) = makeFiberRings({v,u}, R)
  describe S
  describe SF

  use R
  G = resultant(b^3-7*b^2+14*b-7,r^2-u*r+(-2*b^2+9*b-5)*u^2+b^2-4*b, b)
  H = resultant(G, x^2+(b-2)*x*r+r^2+b^2-4*b, x)
  factor H
///

TEST /// -- radical test
  R = ZZ/32003[a..f]
  F = map(R,R,symmetricPower(2,matrix{{a,b,c}}))
  I = ker F
  J = I^2
  G = I_0
  radicalContainment(G,J)
  radicalContainment(G-a^2,J)
///

TEST ///
  -- used to be in tests/normal/radical2.m2
  kk = ZZ/101
  S = kk[vars(0..11)]
  I = ideal"-be+af,-de+cf,-dg+ch,-bi+aj"
  time assert(radical I == ideal {b*i-a*j, d*g-c*h, d*e-c*f, b*e-a*f}) -- fast now

  J = ideal"-de+cf,-bg+ah,-fg+eh,-bi+aj,-di+cj"
  time assert(radical J == ideal {d*i-c*j, b*i-a*j, f*g-e*h, b*g-a*h, d*e-c*f}) -- was a problem, switched algorithm to intersect decompose
///

TEST /// -- radicalContainment test: sharp bound example
  d = (4,5,6,7)
  n = #d
  k = ZZ/101
  R = k[x_0..x_n]
  I = ideal homogenize(matrix{{x_1^(d#0)} | apply(toList(1..n-2), i -> x_i - x_(i+1)^(d#i)) | {x_(n-1) - x_0^(d#-1)}}, x_n)
  D = product(I_*/degree/sum)
  assert(x_0^(D-1) % I != 0 and x_0^D % I == 0)
  elapsedTime assert(radicalContainment(x_0, I, Strategy => "Kollar"))
  elapsedTime assert(radicalContainment(x_0, I))
  f = random(1,R)
  elapsedTime assert(not radicalContainment(f, I, Strategy => "Kollar")) -- ~3s
  elapsedTime assert(not radicalContainment(f, I)) -- ~1s
///

TOODAMNSLOW ///
  d = (4,5,6,7)
  n = #d
  k = ZZ/101
  R = k[x_0..x_n]
  I = ideal homogenize(matrix{{x_1^(d#0)} | apply(toList(1..n-2), i -> x_i - x_(i+1)^(d#i)) | {x_(n-1) - x_0^(d#-1)}}, x_n)
  A = random(R^(n+1), R^(n+1))
  J = sub(I, vars R * A);
  f = ((vars R)*A)_{0} _(0,0)
  elapsedTime assert(radicalContainment(f, J, Strategy => "Kollar")) -- ~15s
  elapsedTime assert(radicalContainment(f, J)) -- ~75 s
  elapsedTime assert(not radicalContainment(x_0, J, Strategy => "Kollar")) -- ~7s
  elapsedTime assert(not radicalContainment(x_0, J)) -- ~22s
///

end--

-- UHOH problem with finite fields
restart
needsPackage "MinimalPrimes"
kk = GF(7)
kk = ZZ/7
R = kk[x,y,t]
I = ideal {x^7-t^2,y^7-t^2}
I' = sub(I, {x => (random kk)*y + (random kk)*x, y => (random kk)*x + (random kk)*y})
minprimes(I, Verbosity=>2)

---
restart
--installPackage "MinimalPrimes"
needsPackage "UnitTestsPD"
wallTiming (() -> check "UnitTestsPD")
--- On Frank's office machine, 11/27/2012 : 95 seconds
--- on Mike's rMBP, 12/7/2012: 70 seconds
---  r15790, 72 tests, 1/3/2013: 81 seconds
