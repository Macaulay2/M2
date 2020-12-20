TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"

  restart
  check "AssociativeAlgebras"

*-
  --- generators test
  debug Core -- for generatorSymbols
  R = QQ{a,b,c}; assert(R#generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R
    
  R = QQ{a,b,c}; assert(R#generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R

  R = QQ{a,b, x_1..x_3, c, y_1..y_4}
  assert(numgens R == 10)
  debugLevel = 1
  isWellDefined R

  R = QQ{{a,b,c},{d,e}}; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),{d,e}}; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{(a,b,c),(d,e)}; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ{b..f}; assert(R#generatorSymbols == splice {vars(1,2,3,4,5)})
  R = QQ{a,b,c}; assert(R#generatorSymbols == splice {vars(0,1,2)})
  R = QQ{x_1..x_100, y_1..y_100}; assert(numgens R == 200)
  debugLevel = 1
  isWellDefined R
///

TEST ///
  -- toExternalString
  -- toString
  -- expression
  -- net
  -- describe
  R = QQ{a,b, x_1..x_3, c, y_1..y_4}
  isWellDefined R
  unstack describe R === {"QQ {a, b, x , x , x , c, y , y , y , y }", 
                            "           1   2   3      1   2   3   4"}
  assert(net R === "R")
  assert(net R == net expression R)
  assert((depth describe R, height describe R, width describe R) == (1,1,40))
  assert(toString R === "R")
  assert(toExternalString R === "QQ {a, b, x_1, x_2, x_3, c, y_1, y_2, y_3, y_4}")
///

TEST ///
  --- equality
  R = QQ{a,b,c}
  assert(a != b)
  assert(a == a)
  assert(b*a + a*b + b*a == 2*b*a + a*b)
  assert(R_0 == a)
  f = a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1
  g = (a*a*b+a+1)*(a*a*b+a+1)
  assert(f == g)
  assert(f - g == 0)
///

TEST ///
  -- printing tests
  R = QQ{a,b,c}
  f = a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1
  g = (a*a*b+a+1)*(a*a*b+a+1)
  assert(toExternalString(f - g) == "0")
  assert(f == g)
  assert(f-g == 0)
  assert(net f == net expression f)
  assert(toString f === "a^2*b*a^2*b+a^3*b+a^2*b*a+2*a^2*b+a^2+2*a+1")
  assert(toString f === toExternalString f)
-*
  for i from 0 to 10 list (elapsedTime size (h = g^i));
  for i from 0 to 8 list (elapsedTime size (h = g^i))
  g1 = g;
  for i from 0 to 7 do elapsedTime (h = g1*g; g1 = h; print size h) 
  
  g1 = g;  
  for i from 0 to 7 do elapsedTime (h = g*g1; g1 = h; print size h)   
  apply(11, i -> print size elapsedTime(h = g^i));
*-
///

///
i3 :   g = (a*a*b+a+1)*(a*a*b+a+1)

      2   2     3     2        2     2
o3 = a b*a b + a b + a b*a + 2a b + a  + 2a + 1
  
31
     -- 0.000237961 seconds elapsed
127
     -- 0.000494012 seconds elapsed
511
     -- 0.00139882 seconds elapsed
2047
     -- 0.00770198 seconds elapsed
8191
     -- 0.0549404 seconds elapsed
32767
     -- 0.188868 seconds elapsed
131071
     -- 0.583566 seconds elapsed
524287
     -- 2.66607 seconds elapsed
///

BENCHMARK ///
-- this takes currently about 2 GB, so can't be run as a test
  R = QQ{a,b,c,d}
  g = a+b+c+d
  elapsedTime for i from 0 to 11 list (elapsedTime size (h = g^i))
  g3 = g^3
  g5 = g^5
  g8 = g^8;
  assert(g3*g5 == g8)
///

TEST ///
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2
  h2 = g2*g3
  assert((g1*g2)*g3 == g1*(g2*g3))
  --g1*g2*g3*g2*g1*g3*g1*g3*g2*g1*g2*g1*g2;
  h1 = g1*g2*g3*g2*g1*g3;
  h2 = g1*g3*g2*g1*g2*g1; -- *g2;

  --elapsedTime for i from 0 to 11 list (elapsedTime size (h = g^i))
  g3 = g1^3
  g5 = g1^5
  g8 = g1^8;
  assert(g3*g5 == g8)
///

TEST ///
  needsPackage "AssociativeAlgebras"
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2*g3*g2*g1;
  h2 = g1*g3*g2*g1*g2;
  size(h1)
  size(h2)
  h3 = elapsedTime(h1*h2); -- memory usage: elapsedTime: 2.59 sec
  assert(size h3 == 164025)
///

BENCHMARK ///
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2*g3*g2*g1*g2;
  h2 = g1*g3*g2*g1*g2;
  size(h1)
  size(h2)
  h3 = elapsedTime(h1*h2); -- memory usage: 670 MB elapsedTime: 10.2 sec
  assert(size h3 == (size h1) * (size h2))
  assert(size h3 == 492075)
///

BENCHMARK ///
-- This one uses too much memory (about 1.7 GB)
  R = QQ{a,b,c,d}
  g1 = a^2-b*c+c^3
  g2 = a*b*a+c*d*b+3*a*b*c*d
  g3 = a*b+b*c+c*d+d*a-1
  h1 = g1*g2*g3*g2*g1*g2;
  h2 = g1*g3*g2*g1*g2*g1;
  size(h1)
  size(h2)
  h3 = elapsedTime(h1*h2); -- memory usage: 2.4 GB elapsedTime:  90.4 sec
  assert(size h3 == (size h1) * (size h2))
  assert(size h3 == 1476225)
///

BENCHMARK ///
-- this examples uses 2.2 GB
  R = QQ{a,b,c,d,e,f,g}
  G = a+b+c+d+e+f+g
  elapsedTime for i from 0 to 8 list (elapsedTime size (H = G^i))
///

--- question: why is engine code slower for this computation than NCAlgebra?
-- apply(11, i -> time(h = g^i));  -- SIGSEGV?

TEST ///
  --- promote/lift tests
  R = QQ{a,b,c}
  3_R
  assert(promote(3,R) == 3_R)
  assert(promote(23423/324,R) == (23423/324)_R)
  
  debug Core
  A = ZZ/101[s,t]
  B = A{x,y,z}
  promote(s,B)
  f = (s + x + y)^2
  (coeff, monoms) = rawPairs(raw A, raw f)
  peek first monoms

  A = ZZ/101[t]/t^2
  B = A{x,y,z}
  promote(t,B)
  t_B
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  debug Core
  -- basic arithmetic
  A = ZZ/101[t]/t^2
  B = A{x,y,z}
  f = 0_A * x
  raw f 
  
  f = (t*x + t*y)^2
  assert(toString raw f == "0")
  (x + t*y)^2 == x^2 + t*x*y + t*y*x 
  
  f = (t*x + t*y)^2
  f = (t*x + t*y)*(t*x+t*y)
  assert(size f == 0)
///

TEST /// 
  R = QQ{b,c,d}
  f = 3*b^2*c*b + 2*b^4
  assert(size (b+c) == 2)
  terms f == {2*b^4, 3*b^2*c*b}
  assert(# terms f == 2)
  assert(sum terms f == f)
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{b,c,d}
  assert instance(R, FreeAlgebra)

  f = 3*b^2*c*b + 2*b^4
  assert(leadTerm f == 2*b^4)
  assert(leadCoefficient f == 2)
  assert(degree f == {4})
  assert(someTerms(f,0,2) == f)
  assert(leadMonomial f == b^4)
  assert(isHomogeneous f)

  g = b*c*b-b
  assert not isHomogeneous g
  
  A = QQ[a]
  B = A{b,c,d}
  f = 3*(a^2-a-1)*b^2*c*b + 2*(a^3-a-1)*b^4
  g = (a+2*b+3*c)^3
  assert(leadCoefficient f == 2*(a^3-a-1))
  assert(leadTerm f == 2*(a^3-a-1)*b^4)
  assert(someTerms(g,2,3) == 12*b*c*b + 18*b*c^2 + 12*c*b^2)
  assert(size g == 15)

  A = frac(QQ[a])
  B = A{b,c,d}
  f = 3/(a^2-a-1)*b^2*c*b + 2/(a^3-a-1)*b^4
  assert(leadCoefficient f == 2/(a^3-a-1))
  assert(leadTerm f == 2/(a^3-a-1)*b^4)
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{b,c,d, Degrees=>{2,3,4}}
  degree b
  degree c
  degree d
  assert(degree(b*d*c) == {9})
  assert isHomogeneous(b^2-d)
  assert not isHomogeneous(b^2-c)

  R = QQ{b,c,d, Degrees=>{{1,0},{0,1},{3,-4}}, Heft=>{2,1}}
  degree b
  degree c
  degree d
  assert(degree(b*d*c) == {4,-3})
  assert isHomogeneous(c^4*d-b^3)
  assert(degree(c^4*d-b^3) == {3,0})
  assert not isHomogeneous(b^2-c)
  
  F = b^3 + c^4*d
  assert(leadTerm F == c^4*d);  -- default order is heft-graded, then word length, then lexicographic
  I = ideal"b3-c2dc2"
  assert isHomogeneous I
  NCGB(I, 10)
///

TEST ///
  R = QQ{a,b,c,d}
  {b*c}
  e = {{b,c,d,b*c,c*b,b^2,a*c-1}}
  M = matrix for j from 1 to 10 list for i from 1 to 10 list a*b-i*a-j*b
  M_(1,1)
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C - matrix {{c*b}} == 0)
  D = matrix {{b,c}}
  assert(D * transpose D - matrix {{b^2 + c^2}} == 0)
  assert(transpose D * D - matrix {{b^2,c*b},{b*c,c^2}} == 0)
///

TEST ///
  R = QQ{b,c,d}
  M = R^2
  B = matrix {{b}}
  C = matrix {{c}}
  assert(B*C - matrix {{c*b}} == 0)
  N = mutableMatrix(R,2,3);
  N = mutableMatrix(R,2,3)
  N = mutableMatrix(R,100,200);
  N_(1,1)
  D = matrix {{b,c}}
  assert(D * transpose D - matrix {{b^2 + c^2}} == 0)
  assert((transpose D * D) - matrix {{b^2,c*b},{b*c,c^2}} == 0)
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-

  A = QQ[s,t]
  R = QQ{b,c,d}
  F = map(R,A,{b*c,d*c})
  G = map(A,R,{s,t,s*t})
  assert(G b == s)
  assert(G 3 == 3)
  F s
  assert(F (s*t) == d*c*b*c) -- Do we want to allow this? F is not well-defined. kind of a BUG!!
          
  F1 = map(R,R,{c,b,d})
  F1 (b*c*d + b*b*d*c*d*b)

  use R  
  F2 = map(R,R,{c+b,c,d})  
  F2(b+c+d)
  g = 3*b*c + b*c*b -2* b*d*b
  assert(F2 g == 3 * (b+c)*c + (b+c)*c*(b+c) -2* (b+c)*d*(b+c))
  
  B = QQ[b,c,d]
  H1 = map(R,B)
  H2 = map(B,R)
  use R
  H2 (b*c)
  
  R = QQ{b,c,d}
  a1 = (3/4)_R
  lift(a1,QQ) -- ok
  a2 = 3_R
  lift(a2,ZZ) -- ok
  promote(3/4, R)  -- ok
  
  A = ZZ/32003[t]/t^2
  B = A{x,y,z}
  promote(t_A, B) == t_B
  promote(3, B)
  assert(coefficientRing B === A)
  lift(t_B, A)
  
  kk = QQ
  A = kk[a]
  B = A[b]
  C = B{c,d}
  assert(lift(a_C, B) == a_B)
  assert(lift(a_C, A) == a_A)
  assert(try (lift(a_C, kk); false) else true)
///

TEST ///
  RingMap @@ RingMap := (f,g) -> (
      if target g =!= source f then error "Expected composable maps.";
      map(target f, source g, apply(gens source g, x -> f g x))
      )

  R = QQ{b,c,d}
  F1 = map(R,R,{c,b,d})
  F2 = map(R,R,{c+b,c,d})  
  F3 = map(R,R,{b*d-1, c*c-c, b-d})
  G = F1 @@ F2
  G2 = F2 @@ F3
  use R  

  F1 (b*c*d + b*b*d*c*d*b)  
  F2 (b*c*d + b*b*d*c*d*b)  
  F3 (b*c*d + b*b*d*c*d*b)  
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c,d}
  M = matrix{{a*b*c-2*a*a*b*a}}
  assert(monomials M == matrix{{a^2*b*a, a*b*c}})
  coefficients M
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  A = QQ[a..d]
  M = matrix{{a,b},{c,d}}
  monomials(M*M*M)
  R = QQ{a,b,c,d}
  M = matrix{{a*b+b*a, a*b+c*d, a*a+b*a}}
  monomials M
  coefficients M

  M = matrix{{a,b},{b,d}}
  M3 = M*M*M
  M6 = M3 | M3
  assert(monomials M6 == monomials M3)
  mons = monomials M6
  (mon,cf) = coefficients M6
  assert(mon*cf == M6)
  assert(mons == monomials mons)

  M = matrix{{a,b},{b,3*d-1}}
  M3 = M*M*M
  M6 = M3 | M3
  assert(monomials M6 == monomials M3)
  mons = monomials M6
  (mon,cf) = coefficients M6
  assert(mon*cf == M6)
  assert(mons == monomials mons)
///

TEST ///
  -- noncommutative reduction test
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a..d}
  I = ideal(a*b*a-a*c*b)
  I2 = ideal(a*b*a-a*c*b, d*a*c*b)
  I3 = ideal(a*b - b*a, a*c - c*a, a*d - d*a, b*c - c*b, b*d - d*b, c*d - d*c)
  J = ideal(a*b*a)
  K = ideal(a*c*b)
  L = ideal(a*b*d*c*a*d*b*c*a*b*d*c*c*c*d*b*a)
  debug Core
  map(R, rawNCReductionTwoSided(raw gens I, raw gens I))
  map(R, rawNCReductionTwoSided(raw gens I, raw gens J))
  map(R, rawNCReductionTwoSided(raw gens I, raw gens K))
  map(R, rawNCReductionTwoSided(raw gens I2, raw gens K))

f = a*a-b*c-a
g = NCReduction2Sided(a*f-f*a, ideal(f))
g = -a*b*c+b*c*a
h = a*g + f*b*c
-- TODO: Fix this!
NCReduction2Sided(h, ideal(f,g)) -- never never land
///

TEST ///
  -- noncommutative reduction test
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b}
  I = ideal(a^2 - b^2)
  gbTrace=3
  debug Core
  NCGB(I, 387)
///

TEST ///
-- test of free algebra quotient rings
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b}
  I = ideal(a^2 - b^2)
  A = R/I

  NCGB(I, 1000) 
  J = gens ideal NCGB(I, 1000)
  A1 = R/I
  assert(A1 =!= A)
  assert(I.cache.NCGB#0 == 1000)

-- i16 : coefficients(a^3)
-- stdio:16:1:(3): error: expected polynomial ring

-- i17 : lift(a^3, R)
-- stdio:17:1:(3): error: cannot lift given ring element

-- basis(4, A) -- error: can't handle this kind of ring.  
  use R
  terms(a^3)
  f = a^3
  use A
  terms(a^3)
  assert(promote(f, A) == b^2*a)
  assert(a == A_0)
  assert(b == A_1)
  assert(5 == # unique for e in (0,0,0,0)..(1,1,1,1) list product for i in e list A_i)
  assert(6 == # unique for e in (0,0,0,0,0)..(1,1,1,1,1) list product for i in e list A_i)

  R = QQ{a,b,c}
  I = ideal"aba-bab, ac-ca, ab+ba"
  J = gens ideal NCGB(I, 10)  

  -- 'monomials' seems to be working:
  assert(monomials matrix"ab-ba,ab+ba" == matrix"ab,ba")
  assert(numcols monomials J == 131)

  -- coefficients works over free algebras
  coefficients(a^3)
  elapsedTime (monoms, cfs) = coefficients(J, Monomials => monomials J);
  assert(monoms * cfs == J)  
  elapsedTime (monoms, cfs) = coefficients(J);
  assert(monoms * cfs == J)  

  -- monomials, coefficients, over quotients of free algebras.
  A = R/I  

  -- 'monomials' seems to be working:
  assert(monomials matrix"ab-ba,ab+ba" == matrix"ba")
  coefficients(a^3)
  assert(sub(J, A) == 0)
  M = matrix"ab-ba,ac-ba,aca-bab-a3"
  monomials M 
  elapsedTime (monoms, cfs) = coefficients(M, Monomials => monomials M);
  assert(monoms * cfs == M)  
  elapsedTime (monoms, cfs) = coefficients M;
  assert(monoms * cfs == M)  

  sub(M, R) -- TODO: should lift monomials as in the commutative case
  map(R,A) -- gives the 0 map
  map(A,R) -- ok
  sub(M, vars R) -- ok
  
  lift(M,R)
  phi = map(R,A,vars R)
  phi M
  
-- TODO
-*
  . raw A -- display quotient elements, for debugging purposes.
  . coefficients, monomials DONE (might need some more refactoring in c++ code).
  . basis
  . random
  . terms DONE (changed makeTerm to use M2FreeAlgebraOrQuotient)  
*-
///

TEST ///
-- test of basis of a quotient ring
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b}
  I = ideal(a^2 - b^2)
  NCGB(I, 1000)
  A = R/I
  assert(numcols ncBasis({10}, {10}, A) == 11) 
  ncBasis({500},{500},A); -- Duplicate large block deallocation?
  elapsedTime assert(numcols ncBasis({1000},{1000},A) == 1001)

  S = QQ{u,v,Degrees=>{2,3}}
  I = ideal(u*v + v*u)
  T = S/I
  assert(ncBasis({15},{15},T) == matrix{{v*u^6, v^3*u^3, v^5}})
///

TEST ///
-*
-- XXX
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c}
  R = (ZZ/32003){a,b,c}
  I = ideal(2*a*b + 3*b*a + 5*c^2,
             2*b*c + 3*c*b + 5*a^2,
             2*c*a + 3*a*c + 5*b^2)
  elapsedTime NCGB(I, 4);

  elapsedTime NCGB(I, 10);
  A = R/I
  assert(numcols ncBasis(0,A) == 1)
  assert(numcols ncBasis(1,A) == 3)
  assert(numcols ncBasis(2,A) == 6)
  assert(numcols ncBasis(3,A) == 10)
  assert(numcols ncBasis(4,A) == 15)
  assert(numcols ncBasis(5,A) == 21)
  assert(numcols ncBasis(6,A) == 28)
  assert(numcols ncBasis(10,A) == 66)

  -*  
  -- Did these in order, in same session, right after defining I (reason for speedup: almost certainly skype)
  elapsedTime NCGB(I, 20); -- best time so far: Map.  5.9 sec, at home it is 4.2 sec (same computer)... 
    -- 27/12/2019, Mike MBP: now 2.7 sec

  elapsedTime NCGB(I, 21); -- 9.8 sec, 6.9 sec at home, same computer, Map.
    -- 27/12/2019, Mike MBP: 4.4 sec

  elapsedTime NCGB(I, 22); -- 16.23 sec, 11.7 sec at home, same computer, Map.
    -- 27/12/2019, Mike MBP: 7.3 sec

  elapsedTime NCGB(I, 23); 
    -- 27/12/2019, Mike MBP: 12.2 sec
  *-
///

///
  -- magma code
  kk := Rationals();
  kk := FiniteField(32003);
  F<a,b,c> := FreeAlgebra(kk,3);
  B := [2*a*b + 3*b*a + 5*c^2,
             2*b*c + 3*c*b + 5*a^2,
             2*c*a + 3*a*c + 5*b^2];
  I := ideal<F | B>;
  # GroebnerBasis(B,20);
  
  kk := Rationals();
  kk := FiniteField(32003);
  F<x,y,z,w> := FreeAlgebra(kk,4);
  B := [x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x];
  I := ideal<F | B>; 
  Igb := GroebnerBasis(B,10); 
///

TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c, Degrees=>{{1,0,0},{0,1,0},{0,0,1}}}
  assert(degree a == {1,0,0})
  assert(degree (a^2 + b^2 + c^2) == {2,2,2})
  assert not isHomogeneous (a^2 + b^2 + c^2)
  assert isHomogeneous a^2
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  -- note that variables in the base of a FreeAlgebra commute
  -- with the variables adjoined.  I.e. QQ{x}{y} is the same as QQ[x,y]
  R = QQ[x,y]/ideal{x^2,x*y,y^2}
  S = R{a,b}
  T = S{c,d}
  assert(a*c == c*a)
  assert(x*c == c*x)
  f = x*c + y*d
  assert(f^2 == 0)
  assert(x*f == 0)
  assert(numcols ncBasis(2,S) == 4)
  assert(numcols ncBasis(2,T) == 4)
  assert(numcols ncBasis(0,S) == 1)
  assert(ncBasis(-1,S) == 0)
  g = (a*c + b*d)^2
  assert(#(terms g) == 4)
///  

--- bugs 2/20/2020


TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
  R = QQ{a,b,c,t, Weights=>{{1,1,1,0}}}
  I = ideal {a*b - c*t, b*c - a*t, c*a - b*t, a*t - t*a, b*t - t*b, c*t - t*c}
  J2 = NCGB(I,2) 
  J3 = NCGB(I,3)
  J4 = NCGB(I,4)
  I2 = ideal J2 + ideal {a^2-c^2,b^2-c^2,c^2*b - t*a*c, a*c^2 - t*c*b, b*a^2-t*a*c, c^3 - t*b*a, c*b^2 - t*b*a}
  J4 = NCGB(I2,4)
  I3 = ideal J4_(toList(0..10)) + ideal {a*c*b - b*a*c, b*a*c - c*b*a}
  J4 = NCGB(I3,4)
  J5 = NCGB(I3,5)
  J6 = NCGB(I3,6)
  compress sub(J6, {t => 1}) -- looks like it is working :)
  
  R = QQ{a,b,Degrees=>{2,3}}
  assert(leadTerm (a+b) == b)  -- should be b
  assert(leadTerm (a^3 + b^2) == a^3)-- should be a^3 (which it is)

  R = QQ{a,b,Degrees=>{2,3}, Weights=>{{1,0},{0,1}}}
  -- The following two ring definitions are supposed to give errors.
  assert try (R = QQ{a,b,Degrees=>{2,3}, Weights=>{{1,0},{0,1,1}}}; false) else true
  assert try (R = QQ{a,b,Degrees=>{2,3}, Weights=>{{-1,0},{0,-1}}}; false) else true
///

TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
R = QQ{a,b,c,x,y, Degrees => {3,3,2,1,1}, Weights => {{0,0,0,1,1}} }
I = ideal{x*y - c, x*y*x-a, y*x*y-b}
isHomogeneous I
assert(degrees source gens I === {{2},{3},{3}})
M1 = gens I
J = NCGB(I,3) 
J = NCGB(I,20)
M2 = I.cache.NCGB#1
J1 = ideal (ideal M1)_*
J2 = ideal (ideal M2)_*
assert(NCGB(J1, 20) == NCGB(J2, 20)) -- note: NCGB J2 seems correct.

J = NCGB(I, 6)
assert isHomogeneous J
assert(NCReduction2Sided(x*y*x*y*x, ideal J) == c*a)
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ{b,c}
  I = ideal"bc"
  assert(NCGB(I, 10) == matrix{{b*c}})
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
R = QQ{x,y}
I = ideal {x^2-y^2}
S = R/I
gbS = NCGB(ideal S)
debug Core
rawNCBasis(raw gbS,{500},{500},-1);
rawNCBasis(raw gbS,{1000},{1000},-1);
///

TEST ///
-*
restart
needsPackage "AssociativeAlgebras"
*-
A = QQ[x,y]
R = A{b,c,d}
f = 3*x*y*b^2*c*b + 2*b^4
assert(leadMonomial f == b^4)
assert(ring leadCoefficient f === A)
assert(leadCoefficient f == 2_A)
assert(leadTerm f == 2*b^4)
g = f - 2*b^4
assert(leadMonomial g == b^2*c*b)
assert(ring leadCoefficient g === A)
assert(leadCoefficient g == 3*x*y)
assert(leadTerm g == 3*x*y*b^2*c*b)
assert(leadMonomial 0_R == 0_R)
///

BUG ///
--- things to get fixed:
1) basis rather than ncBasis
2) Check that the type "Ring" inputs
either FreeAlgebra or FreeAlgebraQuotient

--- bringing over ring constructions
restart
needsPackage "AssociativeAlgebras"
--needsPackage "NCAlgebra"
kk = ZZ/32003
A = kk[x,y]
R = kk{a,b,c}
promote(kk^3, R)
promote(kk^3, A)

restart
needsPackage "AssociativeAlgebras"
kk = ZZ/32003
R = kk{a,b,c}
I = ideal(2*a*b + 3*b*a + 5*c^2,
             2*b*c + 3*c*b + 5*a^2,
             2*c*a + 3*a*c + 5*b^2)
gbTrace=2
deg = 17
I = ideal I_*; time J1 = ideal NCGB(I, deg, Strategy=>16); -- this gives wrong answer every n times, for n = ??
numgens J1 == 78 -- this fails here and there... (run this and the the line before it over and over).

gbTrace=2
I = ideal I_*; time NCGB(I, 23, Strategy=>16); 

I = ideal I_*; time NCGB(I, 20, Strategy=>16); 
I = ideal I_*; time NCGB(I, 20, Strategy=>0);

--- this is the matrix in degree 3 for the above computation after column sort, if
--- one would like to manipulate it for checking purposes.
M = map (kk^21, kk^27, { (0,1) => 1, (0,3) => 3/2, (0,8) => 5/2,
	                  (1,0) => 1, (1,5) => 2/5, (1,7) => 3/5,
			  (2,2) => 1, (2,4) => 5/3, (2,6) => 2/3,
			  (3,1) => 1, (3,3) => 3/2, (3,8) => 5/2,
			  (4,3) => 1, (4,9) => 3/2, (4,24) => 5/2,
			  (5,8) => 1, (5,14) => 5/3, (5,20) => 2/3,
			  (6,0) => 1, (6,15) => 2/5, (6,21) => 3/5,
			  (7,5) => 1, (7,11) => 3/2, (7,26) => 5/2,
			  (8,7) => 1, (8,13) => 5/3, (8,19) => 2/3,
			  (9,2) => 1, (9,17) => 2/5, (9,23) => 3/5,
			  (10,4) => 1, (10,10) => 3/2, (10,25) => 5/2,
			  (11,6) => 1, (11,12) => 5/3, (11,18) => 2/3,
			  (12,9) => 1, (12,14) => 2/5, (12,16) => 3/5,
			  (13,20) => 1, (13,22) => 5/3, (13,24) => 2/3,
			  (14,11) => 1, (14,13) => 5/3, (14,15) => 2/3,
			  (15,19) => 1, (15,21) => 3/2, (15,26) => 5/2,
			  (16,10) => 1, (16,12) => 3/2, (16,17) => 5/2,
			  (17,18) => 1, (17,23) => 2/5, (17,25) => 3/5,
			  (18,1) => 1, (18,16) => 2/5, (18,22) => 3/5,
			  (19,0) => 1, (19,15) => 2/5, (19,21) => 3/5,
			  (20,2) => 1, (20,17) => 2/5, (20,23) => 3/5})
M = mutableMatrix M

I = ideal I_*; NCGB(I, 10, Strategy=>0);
--I = ideal I_*; NCGB(I, 10, Strategy=>1); -- crash
I = ideal I_*; NCGB(I, 10, Strategy=>2); -- works
--I = ideal I_*; NCGB(I, 10, Strategy=>3); -- infinite loop
I = ideal I_*; NCGB(I, 10, Strategy=>4); -- works
I = ideal I_*; NCGB(I, 10, Strategy=>5); -- works
I = ideal I_*; NCGB(I, 10, Strategy=>6); -- works -- hmm, doesn't seem to work well

restart
needsPackage "AssociativeAlgebras"
R = QQ{a,b,c, Weights=>{{1,0,0},{0,1,0},{0,0,1}}}
I = ideal {a^2 - 1, b^2 - 1, c^2 - 1, a*b*a - b*a*b, b*c*b - c*b*c, a*c - c*a}
NCGB(I,10)

restart
needsPackage "AssociativeAlgebras"
R = QQ{a,b,c,d}
J = ideal{ a*c - 1, b*d - 1, a^2 - b^3, a^3 - b^5, a*c - c*a, b*d - d*b }
J' = ideal flatten entries NCGB(J,50)

S = R/I
centralElements(S,3)
T = skewPolynomialRing(ZZ/32003,-1,{x,y,z})
T = threeDimSklyanin(QQ,{x,y,z}, DegreeLimit => 10)
time T = fourDimSklyanin(QQ,{x,y,z,w},DegreeLimit => 8)
T = fourDimSklyanin(QQ,{x,y,z,w},DegreeLimit => 4)
T = fourDimSklyanin(ZZ/32003,{x,y,z,w},DegreeLimit => 4)
T = fourDimSklyanin(ZZ/32003,{x,y,z,w})

restart
needsPackage "AssociativeAlgebras"
gbTrace = 2
kk = QQ
kk = ZZ/32003
R = kk{x,y,z,w}
I = ideal {x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x}
time Igb = NCGB(I, 11, Strategy=>16); -- 12.0 seconds (with std::vector<int>)
time Igb = NCGB(I, 10); -- 12.3 seconds (with std::vector<int>)


time Igb = NCGB(I, 20, Strategy=>16);
time Igb = NCGB(I, 10);
S = R/I
#(flatten entries ncBasis(8,S)) == binomial(8+3,3)
flatten entries Igb / degree

-- the following seems wrong
T = fourDimSklyanin(ZZ/32003,{x,y,z,w}, DegreeLimit => 20);
ideal T
--- playing with ore extensions
R = QQ {x,Degrees=>{1}}
f = map(R,R,{-x})
S = oreExtension(R,f,y,Degree=>{2})
g = map(S,S,{-x,-y})
T = oreExtension(S,g,z,Degree=>{3})
-- free products
R1 = QQ {x,Degrees=>{1}}
R2 = QQ {y,Degrees=>{2}}
S = freeProduct(R1,R2)
T = qTensorProduct(R1,R2,-1)
--- homog dual
R1 = QQ[x,y]
S1 = homogDual R1
ideal S1
R3 = QQ[x,y]/ideal{x^2,x*y,y^2}
S3 = homogDual R3
ideal S3
--- getting promote to work
kk = ZZ/32003
A = kk[x,y]
B = A{z,w, DegreeRank=>2}
promote(A^{1,2,3}, B) -- this is not an optimal situation.  We need to allow DegreeMap...
assert(ring promote(kk^3, B) === B) -- fails at the moment.
///

BUG ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ[u,v]
  S = R{x,y,z}
  -- TODO: need to incorporate the degree information of base when creating such a ring.
  f = u*x*y^2 + v^2*x*y*x
  NCGB(ideal f, 5) -- BUG!
  isHomogeneous f -- BUG!
  
  f = c*m*w1 + lot
  g = d*n*w2 + lot
  
  -- overlap s-pair
  w = pos; w1 = po; w2 = os
  m = m1g
  n = n1g
  the s-pair corresponding to this overlap is:
  d*n1*f*s - c*m1*p*g

  -- same monomial lead term s-pair  
  f = f1*w + lot
  g = g1*w + lot  
///

