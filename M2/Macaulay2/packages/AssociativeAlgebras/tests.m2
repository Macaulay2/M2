TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"

  restart
  check "AssociativeAlgebras"

*-
  --- generators test
  debug Core -- for generatorSymbols
  R = QQ<|a,b,c|>; assert(R#generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R
    
  R = QQ<|a,b,c|>; assert(R#generatorSymbols == splice {vars(0,1,2)})
  assert isWellDefined R

  R = QQ<|a,b, x_1..x_3, c, y_1..y_4|>
  assert(numgens R == 10)
  debugLevel = 1
  isWellDefined R

  R = QQ<|{a,b,c},{d,e}|>; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ<|(a,b,c),{d,e}|>; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ<|(a,b,c),(d,e)|>; assert(R#generatorSymbols == splice {vars(0,1,2,3,4)})
  R = QQ<|b..f|>; assert(R#generatorSymbols == splice {vars(1,2,3,4,5)})
  R = QQ<|a,b,c|>; assert(R#generatorSymbols == splice {vars(0,1,2)})
  R = QQ<|x_1..x_100, y_1..y_100|>; assert(numgens R == 200)
  debugLevel = 1
  isWellDefined R
///

TEST ///
  -- toExternalString
  -- toString
  -- expression
  -- net
  -- describe
  R = QQ<|a,b, x_1..x_3, c, y_1..y_4|>
  isWellDefined R
  unstack describe R === {"QQ<|a, b, x , x , x , c, y , y , y , y |>", 
                            "           1   2   3      1   2   3   4"}
  assert(net R === "R")
  assert(net R == net expression R)
  assert((depth describe R, height describe R, width describe R) == (1,1,41))
  assert(toString R === "R")
  assert(toExternalString R === "QQ<|a, b, x_1, x_2, x_3, c, y_1, y_2, y_3, y_4|>")
///

TEST ///
  --- equality
  R = QQ<|a,b,c|>
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
  R = QQ<|a,b,c|>
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
  R = QQ<|a,b,c,d|>
  g = a+b+c+d
  elapsedTime for i from 0 to 11 list (elapsedTime size (h = g^i))
  g3 = g^3
  g5 = g^5
  g8 = g^8;
  assert(g3*g5 == g8)
///

TEST ///
  R = QQ<|a,b,c,d|>
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
  R = QQ<|a,b,c,d|>
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
  R = QQ<|a,b,c,d|>
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
  R = QQ<|a,b,c,d|>
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
  R = QQ<|a,b,c,d,e,f,g|>
  G = a+b+c+d+e+f+g
  elapsedTime for i from 0 to 8 list (elapsedTime size (H = G^i))
///

--- question: why is engine code slower for this computation than NCAlgebra?
-- apply(11, i -> time(h = g^i));  -- SIGSEGV?

TEST ///
  --- promote/lift tests
  R = QQ<|a,b,c|>
  3_R
  assert(promote(3,R) == 3_R)
  assert(promote(23423/324,R) == (23423/324)_R)
  
  debug Core
  A = ZZ/101[s,t]
  B = A<|x,y,z|>
  promote(s,B)
  f = (s + x + y)^2
  (coeff, monoms) = rawPairs(raw A, raw f)
  peek first monoms

  A = ZZ/101[t]/t^2
  B = A<|x,y,z|>
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
  B = A<|x,y,z|>
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
  R = QQ<|b,c,d|>
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
  R = QQ<|b,c,d|>
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
  B = A<|b,c,d|>
  f = 3*(a^2-a-1)*b^2*c*b + 2*(a^3-a-1)*b^4
  g = (a+2*b+3*c)^3
  assert(leadCoefficient f == 2*(a^3-a-1))
  assert(leadTerm f == 2*(a^3-a-1)*b^4)
  assert(someTerms(g,2,3) == 12*b*c*b + 18*b*c^2 + 12*c*b^2)
  assert(size g == 15)

  A = frac(QQ[a])
  B = A<|b,c,d|>
  f = 3/(a^2-a-1)*b^2*c*b + 2/(a^3-a-1)*b^4
  assert(leadCoefficient f == 2/(a^3-a-1))
  assert(leadTerm f == 2/(a^3-a-1)*b^4)
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ<|b,c,d, Degrees=>{2,3,4}|>
  degree b
  degree c
  degree d
  assert(degree(b*d*c) == {9})
  assert isHomogeneous(b^2-d)
  assert not isHomogeneous(b^2-c)

  R = QQ<|b,c,d, Degrees=>{{1,0},{0,1},{3,-4}}, Heft=>{2,1}|>
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
  R = QQ<|a,b,c,d|>
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
  R = QQ<|b,c,d|>
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
  R = QQ<|b,c,d|>
  F = map(R,A,{b*c,d*c})
  G = map(A,R,{s,t,s*t})
  assert(G b == s)
  assert(G 3 == 3)
  F s
  assert(F(s*t) == F(s) * F(t))  -- note that the map F is not well-defined, but
                                 -- M2 still allows such ring maps.
  
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
  
  R = QQ<|b,c,d|>
  a1 = (3/4)_R
  lift(a1,QQ) -- ok
  a2 = 3_R
  lift(a2,ZZ) -- ok
  promote(3/4, R)  -- ok
  
  A = ZZ/32003[t]/t^2
  B = A<|x,y,z|>
  promote(t_A, B) == t_B
  promote(3, B)
  assert(coefficientRing B === A)
  lift(t_B, A)
  
  kk = QQ
  A = kk[a]
  B = A[b]
  C = B<|c,d|>
  assert(lift(a_C, B) == a_B)
  assert(lift(a_C, A) == a_A)
  assert(try (lift(a_C, kk); false) else true)
///

TEST ///
  RingMap @@ RingMap := (f,g) -> (
      if target g =!= source f then error "Expected composable maps.";
      map(target f, source g, apply(gens source g, x -> f g x))
      )

  R = QQ<|b,c,d|>
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
  R = QQ<|a,b,c,d|>
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
  R = QQ<|a,b,c,d|>
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
  debug needsPackage "AssociativeAlgebras"
*-
  R = QQ<|a..d|>
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
g = NCReductionTwoSided(a*f-f*a, ideal(f))
g = -a*b*c+b*c*a
h = a*g + f*b*c
-- TODO: Fix this!
NCReductionTwoSided(h, ideal(f,g)) -- never never land
///

TEST ///
  -- noncommutative reduction test
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ<|a,b|>
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
  R = QQ<|a,b|>
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

  R = QQ<|a,b,c|>
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
  R = QQ<|a,b|>
  I = ideal(a^2 - b^2)
  NCGB(I, 1000)
  A = R/I
  assert(numcols ncBasis({10}, {10}, A) == 11) 
  ncBasis({500},{500},A); -- Duplicate large block deallocation?
  elapsedTime assert(numcols ncBasis({1000},{1000},A) == 1001)

  S = QQ<|u,v,Degrees=>{2,3}|>
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
  R = QQ<|a,b,c|>
  R = ZZ/32003<|a,b,c|>
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

  I = ideal I_*; elapsedTime NCGB(I, 21); -- 9.8 sec, 6.9 sec at home, same computer, Map.
    -- 27/12/2019, Mike MBP: 4.4 sec

  I = ideal I_*; elapsedTime NCGB(I, 22); -- 16.23 sec, 11.7 sec at home, same computer, Map.
    -- 27/12/2019, Mike MBP: 7.3 sec

  I = ideal I_*; elapsedTime NCGB(I, 23); 
    -- 27/12/2019, Mike MBP: 12.2 sec
    -- 6/1/2021, Frank MBP: 2.1 sec
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
  GroebnerBasis(B,5);
  time Igb := GroebnerBasis(B,15);
  -- 8.73 secs d = 25, kk = ZZ/32003
  -- 0.14s  d = 15, kk = ZZ/32003
  -- 10.990 secs d = 15, kk = QQ
  
  kk := Rationals();
  kk := FiniteField(32003);
  F<x,y,z,w> := FreeAlgebra(kk,4);
  B := [x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x];
  I := ideal<F | B>;
  time Igb := GroebnerBasis(B,10);
  -- at least 959 secs, d = 15, kk = ZZ/32003, 11gb ram!
  -- 258 secs, d = 14, kk = ZZ/32003 up to 2.7gb ram
  -- 105 secs, d = 12, kk = QQ
  -- 12.6s, d = 12, kk = ZZ/32003
///

TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
  R = QQ<|a,b,c, Degrees=>{{1,0,0},{0,1,0},{0,0,1}}|>
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
  S = R<|a,b|>
  T = S<|c,d|> -- TODO: should disallow GBs over such a ring.
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
  R = QQ<|a,b,c,t, Weights=>{{1,1,1,0}}|>
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
  
  R = QQ<|a,b,Degrees=>{2,3}|>
  assert(leadTerm (a+b) == b)  -- should be b
  assert(leadTerm (a^3 + b^2) == a^3)-- should be a^3 (which it is)

  R = QQ<|a,b,Degrees=>{2,3}, Weights=>{{1,0},{0,1}}|>
  -- The following two ring definitions are supposed to give errors.
  assert try (R = QQ<|a,b,Degrees=>{2,3}, Weights=>{{1,0},{0,1,1}}|>; false) else true
  -- TODO REINSTATE THIS TEST: it should give an error, but it doesn't.
  -- assert try (R = QQ<|a,b,Degrees=>{2,3}, Weights=>{{-1,0},{0,-1}}|>; false) else true
///

TEST ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
--- test of an "elimination" order for kernels
R = QQ<|a,b,c,x,y, Degrees => {3,3,2,1,1}, Weights => {{0,0,0,1,1}} |>
I = ideal{x*y - c, x*y*x-a, y*x*y-b}
isHomogeneous I
assert(degrees source gens I === {{2},{3},{3}})
M1 = gens I
J = NCGB(I,3) 
J = NCGB(I,20)
J = NCGB(I,20,Strategy=>"F4")
M2 = I.cache.NCGB#1
J1 = ideal (ideal M1)_*
J2 = ideal (ideal M2)_*
assert(NCGB(J1, 20) == NCGB(J2, 20)) -- note: NCGB J2 seems correct.

J = NCGB(I, 6)
assert isHomogeneous J
assert(NCReductionTwoSided(x*y*x*y*x, ideal J) == c*a)
///

TEST /// 
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ<|b,c|>
  I = ideal"bc"
  assert(NCGB(I, 10) == matrix{{b*c}})
///

TEST ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
R = QQ<|x,y|>
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
R = A<|b,c,d|>
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

FAILINGTEST ///
-*
   restart
   needsPackage "AssociativeAlgebras"
*-
-- BUG 12.22.2020
-- ring homs defined on algebras over fraction fields were
-- ignoring denominators of coefficients at some point
kk = frac(QQ[x])
A = kk <|y|>
phi = map(A,A,{y^2})
assert(phi (x*y) == x*y^2)
assert(phi ((1/x)*y) == (1/x)*y^2) -- fails
///

TEST ///
kk = ZZ/32003
R = kk<|x,y,z,w|>
I = ideal {x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x}

I = ideal I_*; Igb = NCGB(I, 11, Strategy=>"F4Parallel");
assert(numcols Igb == 99)
I = ideal I_*; Igb = NCGB(I, 12, Strategy=>"F4Parallel");
assert(numcols Igb == 122)
I = ideal I_*; Igb = NCGB(I, 11, Strategy=>"F4Parallel");
assert(numcols Igb == 99)
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
R = kk<|a,b,c|>
promote(kk^3, R)
promote(kk^3, A)

restart
needsPackage "AssociativeAlgebras"
kk = QQ
kk = ZZ/32003
R = kk<|a,b,c|>
I = ideal(2*a*b + 3*b*a + 5*c^2,
             2*b*c + 3*c*b + 5*a^2,
             2*c*a + 3*a*c + 5*b^2)
gbTrace=2
deg = 30
I = ideal I_*; elapsedTime J1 = ideal NCGB(I, deg, Strategy=>"F4"); -- this gives wrong answer every n times, for n = ??
--- 53 sec d = 30, kk = ZZ/32003, sequential
--- 45 sec d = 30, kk = ZZ/32003, parallel
--- 5.37sec, d = 25, kk = ZZ/32003
--- 60 sec, d = 15, kk = QQ
numgens J1 == 78 -- this fails here and there... (run this and the line before it over and over).

gbTrace=2
I = ideal I_*; time NCGB(I, 23, Strategy=>"F4"); 
I = ideal I_*; time NCGB(I, 20, Strategy=>"F4"); 
I = ideal I_*; time NCGB(I, 20, Strategy=>"Naive");

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

I = ideal I_*; NCGB(I, 10, Strategy=>"Naive");
-- Haven't named the below strategies yet
--I = ideal I_*; NCGB(I, 10, Strategy=>1); -- crash
--I = ideal I_*; NCGB(I, 10, Strategy=>2); -- works
--I = ideal I_*; NCGB(I, 10, Strategy=>3); -- infinite loop
--I = ideal I_*; NCGB(I, 10, Strategy=>4); -- works
--I = ideal I_*; NCGB(I, 10, Strategy=>5); -- works
--I = ideal I_*; NCGB(I, 10, Strategy=>6); -- works -- hmm, doesn't seem to work well

restart
needsPackage "AssociativeAlgebras"
R = QQ<|a,b,c,d|>
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

Row 30;3: [9,1] [30,3] [42,4] 
Row 31;3: [10,1] [31,3] [43,4] 
Row 32;3: [11,1] [32,3] [44,4] 
Row 33;3: [0,1] [5,3] [8,4] 
Row 34;3: [2,1] [3,-4571] [7,4572] 
Row 35;3: [1,1] [4,-3429] [6,-3430] 

-- XXX
restart
needsPackage "AssociativeAlgebras"
debug Core
--gbTrace = 2
kk = QQ
kk = ZZ/32003
kk = ZZp(32003, Strategy=>"Aring")
R = kk<|x,y,z,w|>
I = ideal {x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x};
elapsedTime Igb = NCGB(I, 8, Strategy=> "F4Parallel");
-- Should be 10 gens, 108 rows in last matrix, 35, 61 new gb elts
-- I = ideal I_*; elapsedTime Igb = NCGB(I, 11, Strategy=> "Naive");
count = 0
while (true) do (
    I = ideal I_*;
    elapsedTime Igb = NCGB(I, 8, Strategy=> "F4Parallel");
    assert(#(flatten entries Igb) == 42);
    Igb = null;
    count = count + 1;
    << "Count: " << count << endl;    
    collectGarbage();
    collectGarbage();
    collectGarbage();
    collectGarbage();    
    if count >= 2000 then break;
)

gbTrace = 50; I = ideal I_*; elapsedTime Igb = NCGB(I, 3, Strategy=> "F4Parallel");
I = ideal I_*; elapsedTime Igb2 = NCGB(I, 6, Strategy => "Naive"); -- (with autoreduction) 5.2 sec
I = ideal I_*; elapsedTime Igb = NCGB(I, 11, Strategy => "Naive"); -- (with autoreduction) 19.9 sec
I = ideal I_*; elapsedTime Igb = NCGB(I, 12, Strategy => "Naive"); -- (with autoreduction) 101 sec

I = ideal I_*; elapsedTime Igb = NCGB(I, 14, Strategy=>"F4");    -- 2220 seconds, I think? (now 380 sec on FMs machine)
I = ideal I_*; elapsedTime Igb = NCGB(I, 14, Strategy=>"Naive"); -- 

gbTrace = 50; I = ideal I_*; elapsedTime Igb = NCGB(I, 4, Strategy=> "F4Parallel"); -- (with autoreduction) .9 sec
I = ideal I_*; elapsedTime Igb = NCGB(I, 11); -- (with autoreduction) 3.5 sec
I = ideal I_*; elapsedTime Igb = NCGB(I, 12); -- (with autoreduction) 17.7 sec                 --- 8 secs
I = ideal I_*; elapsedTime Igb = NCGB(I, 12, Strategy => "F4"); 
I = ideal I_*; elapsedTime Igb = NCGB(I, 10, Strategy => "F4Parallel"); 
I = ideal I_*; elapsedTime Igb = NCGB(I, 13, Strategy => "F4"); -- (with autoreduction) 79 sec (153 gens in GB)  --- 29 secs
I = ideal I_*; elapsedTime Igb = NCGB(I, 13, Strategy => "F4Parallel"); -- 18.84s
I = ideal I_*; elapsedTime Igb = NCGB(I, 14, Strategy => "F4"); -- (with autoreduction) 352 sec (177 gens in GB) --- 110 secs after previous F4 changes, about 2.5gb
                                              -- 102s after lazy 2nd criterion change.
I = ideal I_*; elapsedTime Igb = NCGB(I, 14, Strategy => "F4Parallel"); -- 61s 
I = ideal I_*; elapsedTime Igb = NCGB(I, 15, Strategy => "F4"); -- 381 sec (354s after VA changes) 7.37gb
I = ideal I_*; elapsedTime Igb = NCGB(I, 15, Strategy => "F4Parallel"); -- 220 sec (195s after VA changes) 8.21gb (121s on x86 M1! 103s native!)
I = ideal I_*; elapsedTime Igb = NCGB(I, 16, Strategy => "F4"); --
I = ideal I_*; elapsedTime Igb = NCGB(I, 16, Strategy => "F4Parallel"); -- 




-- Testing: tbb2020 vs tbb2021 attempts on Mike's M1 max (14 Jan 2022)
restart
needsPackage "AssociativeAlgebras"
kk = ZZ/32003
R = kk<|x,y,z,w|>
I = ideal {x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x};

gbTrace=2
I = ideal I_*; elapsedTime Igb = NCGB(I, 13, Strategy => "F4"); 
  -- timings
  -- zoom, tbb2020: 16.7 sec
  -- zoom, tbb2021, gbTrace=2: 17.2 sec
I = ideal I_*; elapsedTime Igb = NCGB(I, 13, Strategy => "F4Parallel"); 
  -- timings
  -- zoom, tbb2020: 7.6 sec
  -- zoom, tbb2020, build matrix in parallel: CRASH in tbb::concurrent_vector code
  -- zoom, tbb2021, gbTrace=2: 7.9 sec
I = ideal I_*; elapsedTime Igb = NCGB(I, 14, Strategy => "F4"); 
  -- timings
  -- zoom, tbb2020:  60.4 sec
I = ideal I_*; elapsedTime Igb = NCGB(I, 14, Strategy => "F4Parallel"); 
  -- timings
  -- zoom, tbb2020: 27.0 sec
  -- zoom, tbb2021: 25.5 sec

I = ideal I_*; elapsedTime Igb = NCGB(I, 15, Strategy => "F4"); 
I = ideal I_*; elapsedTime Igb = NCGB(I, 15, Strategy => "F4Parallel"); 



-- CRT tests
restart
needsPackage "AssociativeAlgebras"
debug Core
--gbTrace = 2
kk = QQ
S = kk<|x,y,z,w|>
I = ideal {x*y-y*x-7*z*w-7*w*z, 3*x*z-4*y*w-3*z*x-4*w*y, 31*x*w+25*y*z+25*z*y-31*w*x, x*y+y*x-z*w+w*z, x*z+y*w+z*x-w*y, x*w-y*z+z*y+w*x};
R = ZZ[]
RQ = QQ[]
prod = 1
d = 10

p = nextPrime (10000000000)
kk = ZZ/p
Sp = freeAlgebra(kk,gens S)
Ip = sub(I,vars Sp)
Ipgb = NCGB(Ip,d, Strategy => "F4Parallel");
numTimes = 1
-- leadTerm broken for matrices with nc entries!
(mons,coeffs) = coefficients Ipgb;
MN = sub(coeffs,R);
prod = prod*p;
coeffsQ = map(RQ,rawMatrixRatConversion(raw MN, prod, raw RQ));
coeffsQ' = 0;

while (numTimes == 1 or coeffsQ != coeffsQ') do (
  numDiffs = #select(flatten entries (coeffsQ - coeffsQ'), f -> f != 0);
  << "Prime                    : " << p << endl;
  << "Number of entries changed: " << numDiffs << endl;
  coeffsQ = coeffsQ';
  p = nextPrime(p+1);
  kk = ZZ/p;
  Sp = freeAlgebra(kk,gens S);
  Ip = sub(I,vars Sp);
  elapsedTime Ipgb = NCGB(Ip,d, Strategy => "F4Parallel");
  (mons',coeffs') = coefficients Ipgb;  -- need to handle when mons are different
  N = sub(coeffs',R);
  if (numrows MN != numrows N or numcols MN != numcols N) then (
    coeffQ' = 0;
    continue;
  );
  numTimes = numTimes + 1;
  elapsedTime MN = map(R,rawMatrixCRA(raw MN, raw N, prod, p));
  prod = prod*p;
  elapsedTime coeffsQ' = map(RQ,rawMatrixRatConversion(raw MN, prod, raw RQ));
)
candGB = (sub(mons,vars ring I)) * sub(coeffsQ,QQ);

candGB = Igb;
compressGBCoeff = compress flatten last coefficients candGB;
compressGBCoeff = apply(flatten entries compressGBCoeff, c -> lift(c,QQ));
bigNum = max (compressGBCoeff / numerator)
bigDen = max (compressGBCoeff / denominator)

restart
needsPackage "AssociativeAlgebras"
S = QQ<|x,y,e,f_1..f_4|>
I = ideal {x*y - y*x, e*x^3, e*y^3}
Igb = NCGB(I,10)
fs = drop(flatten entries Igb, 1)
I2 = ideal {f_1*y, f_3*y, f_4*y, x*y-y*x}
I2gb = NCGB(I2,10)

--f_1.y |-> e.x^3y ~> e.yx^3 = f_3.1 so the syzygy is f_1.y - f_3.1
--which one is the lead term? e.x^3y > e.yx^3
--if there was a tie (which may not happen in the nc case?) go up one step and compare again.

--- FM + MES example 3/15/2021
restart
needsPackage "AssociativeAlgebras"
Q = QQ<|x,y,z,e|>
I = ideal (x*y+y*x, x*z+z*x, y*z+z*y, e*(x - y), e*z)
Igb = NCGB(I,10)

restart
debug Core
R = ZZ[]
RQ = QQ[]
origM = matrix {{4/3,22/7}}
M = sub(sub(matrix {{35,32}},ZZ/101),R)
N = sub(sub(matrix {{10669,4575}},ZZ/32003),R)
MN = map(R,rawMatrixCRA(raw M, raw N, 101, 32003))
map(RQ,rawMatrixRatConversion(raw MN, 101*32003, raw RQ)) -- cool!

time Igb = NCGB(I, 20, Strategy=>"F4");
time Igb = NCGB(I, 10, Strategy=>"Naive");
S = R/I;
#(flatten entries ncBasis(12,S)) == binomial(12+3,3)
flatten entries Igb / degree
all(13, i -> #(flatten entries ncBasis(i, S)) == binomial(i + 3,3))
apply(11, i -> #(flatten entries ncBasis(i, S)))

getMons = f -> terms f / leadMonomial
leadTerms = M -> (flatten entries M) / leadMonomial // ideal
leadTerms Igb
gbI = ideal Igb;
inI = leadTerms Igb;
-- check that elements are interreduced.
all(gbI_*,f -> f - NCReductionTwoSided(f, inI) == leadMonomial f)

monsOneDegUp = f -> (flatten apply(gens R, x -> {x*(f - leadMonomial f), (f - leadMonomial f)*x})) / getMons // flatten
stdMonsSoFar = mons -> partition(m -> NCReductionTwoSided(m,inI) != 0,mons)
fPart = stdMonsSoFar monsOneDegUp(gbI_71);
NCReductionTwoSided(matrix {fPart#false},gbI)

J = ideal Igb;
J10 = select(J_*, f -> sum degree f == 10)

-- the following seems wrong (20 is too big)
T = fourDimSklyanin(ZZ/32003,{x,y,z,w}, DegreeLimit => 20);
ideal T
--- playing with ore extensions
R = QQ <|x,Degrees=>{1}|>
f = map(R,R,{-x})
S = oreExtension(R,f,y,Degree=>{2})
g = map(S,S,{-x,-y})
T = oreExtension(S,g,z,Degree=>{3})
-- free products
R1 = QQ <|x,Degrees=>{1}|>
R2 = QQ <|y,Degrees=>{2}|>
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
B = A<|z,w, DegreeRank=>2|>
promote(A^{1,2,3}, B) -- this is not an optimal situation.  We need to allow DegreeMap...
assert(ring promote(kk^3, B) === B) -- fails at the moment.
///

BUG ///
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
  R = QQ[u,v]
  S = R<|x,y,z|>
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

TEST ///
-- testing kernels
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
--- more robust test of an "elimination" order for kernels
kk = toField(QQ[x]/(x^2+x+1))   -- these examples fail if you use frac instead of toField
R = kk<|y_1,y_2,y_3|>
S = skewPolynomialRing(kk,(-1)_kk,{z_1,z_2,z_3})
f_1 = z_1 + z_2 + z_3
f_2 = z_1 + x^2*z_2 + x*z_3
f_3 = z_1 + x*z_2 + x^2*z_3
phi = map(S,R,{f_1,f_2,f_3})
K = ncKernel phi
assert(ring y_1 === R)
assert(phi(y_1*y_3 - 2*y_2^2 + y_3*y_1) == 0)
assert(phi(y_2*y_3 - 2*y_1^2 + y_3*y_2) == 0)
assert(phi(y_1*y_2 - 2*y_3^2 + y_2*y_1) == 0)
T = R/K
hsT = apply(15, i -> numgens source ncBasis(i,T))
binT = apply(15, i -> binomial(i+2,2))
assert(hsT == binT)
psi = map(S,T,{f_1,f_2,f_3})
assert(ncKernel psi == ideal 0_T)

graphPhi = ncGraphIdeal phi
use ring graphPhi
graphPhiGB = NCGB(graphPhi,10,Strategy=>"F4")
-- note: if I make the power of z_3 in the next monomial much higher
--       e.g. 5, then the reduction takes a long time.  can we speed up reduction
--       algorithm a bit?
preim = NCReductionTwoSided(z_1^2*z_2^3*z_3, ideal graphPhiGB)
alpha = map(S,ring graphPhi,gens S | {f_1,f_2,f_3})
assert(alpha(preim) == alpha(z_1^2*z_2^3*z_3))
///

TEST ///
-- testing kernels
-*
  restart
  needsPackage "AssociativeAlgebras"
*-
--- a simpler example
A = QQ<|a,b,c,Degrees=>{3,3,2}|>
A' = QQ<|a,b,c,Degrees=>{2,2,2}|>
B = QQ<|x,y|>
phi = map(B,A,{x*y*x,y*x*y,x*y})
phi' = map(B,A',{x^2,x*y,y^2})
assert(ncKernel phi' == 0)
graphK = ncGraphIdeal phi
K = ncKernel phi
C = A/K
hsC = apply(15, i -> numgens source ncBasis(i,C))
-- not sure what else to test here yet.
///

DEVELOPMENT ///
-*
  restart
  debug needsPackage "AssociativeAlgebras"
*-
--- more robust test of an "elimination" order for kernels
---xxx---
kk = toField(QQ[x]/(x^2+x+1))   -- these examples fail if you use frac instead of toField
R = kk<|y_1,y_2,y_3|>
S = skewPolynomialRing(kk,(-1)_kk,{z_1,z_2,z_3})
f_1 = z_1 + z_2 + z_3
f_2 = z_1 + x^2*z_2 + x*z_3
f_3 = z_1 + x*z_2 + x^2*z_3
phi = map(S,R,{f_1,f_2,f_3})
K = ncKernel phi
use R -- should not be necessary but a ring from ncKernel is leaking to front end.

-- strictly speaking, this GB calculation is not necessary, as the return value of ncKernel is a GB of K
-- not sure how to "force" that for the return value of ncKernel
Kgb = NCGB(K, Strategy=>"F4") -- this seems to be caught in some kind of infinite loop when over frac field instead of toField
Kgb = NCGB(K, Strategy=>"Naive") -- same.  
T = R/K  -- crashing because the NCGB is not completing if frac above
apply(15, i -> numgens source ncBasis(i,T))
apply(15, i -> binomial(i+2,2))

--- the below calculations compute a GB of the kernel of the above
--- ring map.  
kk = toField(QQ[x]/(x^2+x+1))
A = kk<|y_1,y_2,y_3|>
B = kk<|z_1,z_2,z_3|>
J = ideal apply(subsets(gens B, 2), p -> product p + product reverse p)
C = kk<|z_1,z_2,z_3,y_1,y_2,y_3,Weights=>{1,1,1,0,0,0}|>
-- BUG!!! sub(J,C) does not work, so K's definition below is more complicated
f1 = z_1 + z_2 + z_3
f2 = z_1 + x^2*z_2 + x*z_3
f3 = z_1 + x*z_2 + x^2*z_3
K = ideal apply(subsets(take(gens C,3), 2), p -> product p + product reverse p) + 
    ideal (y_1 - f1, y_2 - f2, y_3 - f3)
isHomogeneous K
Kgb = NCGB(K,20,Strategy=>"F4")
netList flatten entries Kgb
-- The elements of Kgb that are in y_i are a GB of the kernel.

--- a simpler example
A = QQ<|a,b,c,Degrees=>{3,3,2}|>
B = QQ<|x,y|>
phi = map(B,A,{x*y*x,y*x*y,x*y})
K = ncKernel phi
C = A/K

R = QQ<|x,y,a,b,c, Degrees => {1,1,3,3,2}, Weights => {{1,1,0,0,0}} |>
I = ideal{x*y - c, x*y*x-a, y*x*y-b}
isHomogeneous I
assert(degrees source gens I === {{2},{3},{3}})
M1 = gens I
J = NCGB(I,3) 
J = NCGB(I,20)
J = NCGB(I,20,Strategy=>"F4")
M2 = I.cache.NCGB#1
J1 = ideal (ideal M1)_*
J2 = ideal (ideal M2)_*
assert(NCGB(J1, 20) == NCGB(J2, 20)) -- note: NCGB J2 seems correct.

J = NCGB(I, 6)
assert isHomogeneous J
assert(NCReductionTwoSided(x*y*x*y*x, ideal J) == c*a)

f = z_1^2 + z_1*z_2
   A = ring f
   (rawCoeff, rawMons) := rawPairs(raw coefficientRing ring f, raw f)
   reverse sort unique flatten apply(rawMons, rm -> apply(rawSparseListFormMonomial rm, p -> A_(p#0)))
///

BUG ///
restart
needsPackage "AssociativeAlgebras"
kk = frac( ZZ/32003[a,b,c] )
A = kk <|x,y,z|>
I = ideal {c*x^2+a*y*z+b*z*y, b*x*z+c*y^2+a*z*x, a*x*y+b*y*x+c*z^2}
Igb = elapsedTime NCGB(I, 3, Strategy=>"F4");
Igb = elapsedTime NCGB(I, 4, Strategy=>"F4");
Igb = elapsedTime NCGB(I, 5, Strategy=>"F4"); -- .21s
Igb = elapsedTime NCGB(I, 6, Strategy=>"F4"); -- 5.5s
Igb = elapsedTime NCGB(I, 7, Strategy=>"F4"); -- 187s  (my suspicion is that the lack of interreduction is killing us here.)
Igb = elapsedTime NCGB(I, 8, Strategy=>"F4"); -- ??
Igb = elapsedTime NCGB(I, 9, Strategy=>"F4"); -- ??

Igb = elapsedTime NCGB(I, 3);
Igb = elapsedTime NCGB(I, 4);
Igb = elapsedTime NCGB(I, 5);
Igb = elapsedTime NCGB(I, 6); -- crashes often.
///

TEST ///
-- some inhomogeneous GB tests
-*
restart
needsPackage "AssociativeAlgebras"
*-
-- S_4
R = QQ<|a,b,c|>
I = ideal {a^2 - 1, b^2 - 1, c^2 - 1, a*b*a - b*a*b, b*c*b - c*b*c, a*c - c*a}
NCGB(I,20)
S = R/I
assert(sum apply(20, i -> numgens source ncBasis(i,S)) == 4!)

-- S_5
R = QQ<|a,b,c,d|>
I = ideal {a^2 - 1, b^2 - 1, c^2 - 1, d^2 - 1, a*b*a - b*a*b, b*c*b - c*b*c, a*c - c*a, a*d - d*a, b*d - d*b, c*d*c - d*c*d}
NCGB(I,20)
S = R/I
assert(sum apply(20, i -> numgens source ncBasis(i,S)) == 5!)

-- S_6
R = QQ<|a,b,c,d,e|>
I = ideal {a^2 - 1, b^2 - 1, c^2 - 1, d^2 - 1, e^2 - 1, a*b*a - b*a*b, b*c*b - c*b*c, c*d*c - d*c*d, d*e*d - e*d*e, a*c - c*a, a*d - d*a, a*e - e*a, b*d - d*b, b*e - e*b, c*e - e*c}
NCGB(I,20)
S = R/I
assert(sum apply(20, i -> numgens source ncBasis(i,S)) == 6!)

-- S_7
R = QQ<|a,b,c,d,e,f|>
I = ideal {a^2 - 1, b^2 - 1, c^2 - 1, d^2 - 1, e^2 - 1, f^2 - 1, a*b*a - b*a*b, b*c*b - c*b*c, c*d*c - d*c*d, d*e*d - e*d*e, e*f*e - f*e*f, a*c - c*a, a*d - d*a, a*e - e*a, a*f - f*a, b*d - d*b, b*e - e*b, b*f - f*b, c*e - e*c, c*f - f*c, d*f - f*d}
NCGB(I,30)
S = R/I
assert(sum apply(30, i -> numgens source ncBasis(i,S)) == 7!)

-- S_8
R = QQ<|a,b,c,d,e,f,g|>
I = ideal {a^2 - 1, b^2 - 1, c^2 - 1, d^2 - 1, e^2 - 1, f^2 - 1, g^2 - 1, a*b*a - b*a*b, b*c*b - c*b*c, c*d*c - d*c*d, d*e*d - e*d*e, e*f*e - f*e*f, f*g*f - g*f*g,
           a*c - c*a, a*d - d*a, a*e - e*a, a*f - f*a, a*g - g*a, b*d - d*b, b*e - e*b, b*f - f*b, b*g - g*b, c*e - e*c, c*f - f*c, c*g - g*c, d*f - f*d, d*g - g*d, e*g - g*e}
NCGB(I,30)
S = R/I
assert(sum apply(30, i -> numgens source ncBasis(i,S)) == 8!)
///

TEST ///
-- support tests
-*
restart
needsPackage "AssociativeAlgebras"
*-
R = QQ<|x,y,z|>
assert(support (0_R) == {})
assert(support (x) == {x})
assert(support (y) == {y})
assert(support (z) == {z})
assert(support (x*y) == {x,y})
assert(support (x*y*z) == {x,y,z})
assert(support (x + y + z) == {x,y,z})
assert(support (x*y + z) == {x,y,z})
assert(support (x^2 + z) == {x,z})
assert(support (x^2 + x) == {x})

M = matrix {{x,y,z},{x^2,0,y},{0,x^2,x},{0,0,0},{x + y,z,0},{0,0,z^2}}
assert(support M == {x,y,z})
assert(support M^{0} == {x,y,z})
assert(support M^{2} == {x})
assert(support M^{3} == {})
assert(support M^{4} == {x,y,z})
assert(support M^{5} == {z})
assert(support M_{0} == {x,y})
assert(support M_{0,1} == {x,y,z})
assert(support M_{2} == {x,y,z})
///

DEVELOPMENT ///
-- turn this into a test.  Crash using VectorArithmetic when using GCed rings under F4
restart
debug needsPackage "AssociativeAlgebras"
kk = QQ[a]
A = kk<|x_4,x_1,x_2,x_3,ee|>
eps = 1_A
I1 = ideal {x_3^2 - x_1*x_2,
            x_4^2 - eps*x_2*x_1,    
	    x_1*x_3 - x_2*x_4,
	    x_3*x_1 - x_2*x_3,
	    x_1*x_4 - eps*x_4*x_2,
	    x_4*x_1 - x_3*x_2}
--I1gb = NCGB(I1,10)
I2 = I1 + ideal {ee*(x_4 + 2*x_1 + x_2 + 2*x_3), ee*(-2*x_4 - x_1 + 2*x_2 + x_3), x_1*ee,x_2*ee,x_3*ee,x_4*ee,ee^2}
I2gb = NCGB(I2,17,Strategy=>"F4") -- crashes (sometimes)
///

--- (FM) List of bugs found when working on kernel code:
--- 1. fraction field and ring maps, line 801.  I suspect the constructor for RingMap
---    is at fault.
--- 2. sub(ideal,Ring) doesn't seem to work right
---    in the noncommutative case. (zeroing out generators)
--- 3. support is broken (but I wrote ncSupport for the time being)
---      (FM: support has been fixed, I think)  Q: do we need newarray_atomic or is std::vector ok?
--- 4. both F4 and regular GB computation seemed to hang on
---    the example marked by ---xxx---.  Change toField to frac to see the behavior.
--- 5. The ring created for the computation in the kernel code
---    is leaking to the front end.  Not sure how to avoid this.
--- 6. Should implement ncGraphIdeal, as it is useful for preimages
---    as well.

DEVELOPMENT ///
restart
needsPackage "AssociativeAlgebras"
A = QQ<|x,y,z,w|>
I = ideal {z*w*z*w^2*z*w-303600/972977*z*w^2*z^4, z*w*z*w^2*z*w-303600/972977*z*w^2*z^4}
NCGB(I,8)
///

DEVELOPMENT ///
  restart
  needsPackage "AssociativeAlgebras"
  debug Core
  createIdeal = (kk) -> (
    A = kk<|x,y,z|>;
    setRandomSeed(34782734);
    alpha = random(kk);
    beta = random(kk);
    gamma = random(kk);
    p = alpha*y*z + beta*z*y + gamma*x^2;
    q = alpha*z*x + beta*x*z + gamma*y^2;
    r = alpha*x*y + beta*y*x + gamma*z^2;
    ideal{p,q,r}
    )
  runIdeal = (I, deg, strategy) -> (
    << "---- strategy: " << strategy << " deg: " << deg << "---------------" << endl;
    I = ideal I_*;
    elapsedTime Igb = NCGB(I,deg,Strategy=>strategy);
    if numcols Igb == 0 then (<< "***error*** obtained zero gb generators..." << endl; return Igb;);
    B = A/I; 
    vals := apply(deg+1, i -> numcols ncBasis(i, B));
    ans := apply(deg+1, i -> binomial(i+2,2));
    if vals != ans then (
        << "***ERROR*** expected: " << ans << " but got " << vals << endl;
        );
    Igb
    )
  runGBs = (I) -> (
      runIdeal(I, 10, "Naive");
      runIdeal(I, 10, "F4");
      runIdeal(I, 10, "F4Parallel");
      runIdeal(I, 12, "Naive");
      runIdeal(I, 12, "F4");
      runIdeal(I, 12, "F4Parallel");
      runIdeal(I, 14, "Naive");
      runIdeal(I, 14, "F4");
      runIdeal(I, 14, "F4Parallel");
      runIdeal(I, 16, "Naive");
      runIdeal(I, 16, "F4");
      runIdeal(I, 16, "F4Parallel");
      runIdeal(I, 20, "Naive");
      runIdeal(I, 20, "F4");
      runIdeal(I, 20, "F4Parallel");
      )

  runGBs(I = createIdeal (ZZ/32003)) -- OK

  runGBs(I = createIdeal GF(27)) -- OK

  I = createIdeal GF(3^10) -- FlintBig
  runGBs I -- sometimes gives wrong number in Naive, different wrong numbers.

  I = createIdeal GF(7^5) -- FlintBig
  runGBs I -- Naive gives wrong number of generators sometimes

  I = createIdeal GF(3^10, SizeLimit => 60000) -- FlintZech
  runGBs I -- OK

  I = createIdeal GF(7^5, SizeLimit => 60000) -- FlintZech
  runGBs I -- OK

  I = createIdeal GF(27, Strategy => "Flint") -- OK
  runGBs I -- OK

  I = createIdeal ZZp(32003,Strategy=>"Ffpack")
  runGBs I -- OK

  I = createIdeal ZZp(32003,Strategy=>"Aring")
  runGBs I -- OK

  I = createIdeal GF(27, Strategy => "New")
  runGBs I -- OK

  I = createIdeal GF(7^5, Strategy => "New", SizeLimit => 60000)
  runGBs I -- OK

  kk = ZZ/34359738421 -- 2^35 + 53
  runGBs(I = createIdeal kk) -- OK

  kk = ZZ/4611686018427388039 -- 2^62 + 135
  runGBs(I = createIdeal kk) -- OK

  kk = ZZ/9223372036854775837 -- 2^63 + 29
  runGBs(I = createIdeal kk) -- OK

  kk = ZZ/18446744073709551521 -- 2^64 - 95
  runGBs(I = createIdeal kk) -- OK

  kk = ZZ/18446744073709551557 -- 2^64 - 59 -- largest prime less than 2^64.
  runGBs(I = createIdeal kk) -- OK
  
  -- What about ffpack integers?  How well do they work?
  kk = ZZp(32749, Strategy => "Ffpack") -- largest prime with ffpack.
  elapsedTime runGBs(I = createIdeal kk) -- OK  

  kk = ZZp(32749, Strategy => "Aring")
  elapsedTime runGBs(I = createIdeal kk) -- OK  

  kk = ZZp(32749, Strategy => "Flint")
  elapsedTime runGBs(I = createIdeal kk) -- OK  

  I = createIdeal QQ
    runIdeal(I, 10, "Naive"); -- 33 gens OK
    runIdeal(I, 10, "F4"); -- 33 gens -- OK
    runIdeal(I, 10, "F4Parallel"); -- 33 gens OK
    runIdeal(I, 12, "Naive"); -- 47 gens OK
    runIdeal(I, 14, "F4"); -- hmmm, very long...! 87 sec! 60 gens Mikes MBP
    runIdeal(I, 14, "F4Parallel"); -- 19.83 sec Mikes MBP
    
  kk = GF(27, Strategy => "New")    
  S = kk[x,y,z]
  I = ideal random(S^1, S^{-2,-2,-2})
  gens gb I
///

DEVELOPMENT ///
restart
needsPackage "AssociativeAlgebras"
debug Core
kk = GF(27)
kk = GF(3^10)
kk = GF(7^5)
kk = QQ
kk = GF(13^2,Strategy=>"New")
kk = GF(27,Strategy=>"Flint")
kk = ZZp(32003,Strategy=>"Ffpack")
kk = ZZp(32003,Strategy=>"Aring")
A = kk<|x,y,z|>
setRandomSeed(34782734)
alpha = random(kk)
beta = random(kk)
gamma = random(kk)
p = alpha*y*z + beta*z*y + gamma*x^2
q = alpha*z*x + beta*x*z + gamma*y^2
r = alpha*x*y + beta*y*x + gamma*z^2
I = ideal{p,q,r}
gbTrace = 100
elapsedTime Igb = NCGB(I,10,Strategy=>"Naive");
elapsedTime Igb = NCGB(I,12,Strategy=>"F4");
I = ideal I_*; elapsedTime Igb = NCGB(I,15,Strategy=>"F4Parallel");
B = A/I; all(13, i -> #(flatten entries ncBasis(i, B)) == binomial(i + 2,2))
apply(16, i -> #(flatten entries ncBasis(i, B)))
apply(16, i -> binomial(i+2,2))

restart
needsPackage "AssociativeAlgebras"
kk = QQ
A = QQ<|x,y,z|>
p = y*z + z*y - 2*x^2
q = z*x + x*z - 2*y^2
r = x*y + y*x - 2*z^2
I = ideal{p,q,r}
Igb = NCGB(I, 10, Strategy=>"Naive")
I = ideal I_*; Igb = NCGB(I, 10, Strategy=>"F4")
I = ideal I_*; Igb = NCGB(I, 10, Strategy=>"F4Parallel")
B = A/I
ncHilbertSeries(B, Order => 15)

kk = GF(7^5) -- FlintBig
kk = GF(27, Strategy => "New")
kk = ZZ/101; a = 4

restart
needsPackage "AssociativeAlgebras"
kk = GF(27, Strategy => "New")
A = kk<|x,y,z|>
p = a*y*z + z*y - 2*x^2
q = a*z*x + x*z - 2*y^2
r = a*x*y + y*x - 2*z^2
I = ideal{p,q,r}
elapsedTime Igb = NCGB(I, 15, Strategy=>"F4Parallel");
///

DEVELOPMENT ///
restart
needsPackage "AssociativeAlgebras"

B = QQ[X,Y,Z]
resB = res coker vars B
resB.dd

-- playing with resolutions...
A = ZZ/32003<|x,y,z,a,b,c,d,e,f,g,h, Degrees=>{{1},{1},{1},{1},{2},{2},{2},{3},{3},{3},{4}}|>
I = ideal {x*y + y*x - 2*z^2,
    	    y*z + z*y - 2*x^2,
	    z*x + x*z - 2*y^2}
I1 = I + ideal {b - a*x,
                c - a*y,
	    	d - a*z}
d0 = matrix {{x,y,z}};
I1gb = NCGB(I1,10)
I1new = ideal I1gb_{8..12}
I1gbSub = I + I1new*x + I1new*y + I1new*z
I1gbSubGB = NCGB(I1gbSub,10)
NCReductionTwoSided(I1gb,I1gbSubGB)
-- the elements in this list in only b,c,d,x,y,z are the minimal
-- elements
d1 = matrix {{y, x, z}, {x, 16001*z, -2*y}, {-2*z, 16001*y, x}};
net d0 || " " || net d1
-- how to find a minimal generating set?
NCReductionTwoSided(ncMatrixMult(d0,d1),I) -- should be zero
(2/5)_A  ----- BUG: in promote?  Throwing away denominators over a finite field
sub(1/2,A)
I2 = I + ideal {e - (b*y + c*x - 2*d*z),
                f - (b*x + 16001*c*z + 16001*d*y),
		g - (b*z - 2*c*y + d*x)}
NCGB(I2,10)
d2 = matrix {{z},{-2*x},{y}}
NCReductionTwoSided(ncMatrixMult(d1,d2),I)
net d0 || " " || net d1 || " " || net d2

--- trying to do this in general:
--- works on a small example
restart
debug needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(ZZ/32003,{3,5,7},{x,y,z})
d0 = matrix {{x,y,z}}
d1 = rightKernel d0
d2 = rightKernel d1
d3 = rightKernel d2
use B
g = first flatten entries centralElements(B,3)
isCentral g -- central
rightKernel matrix {{g}} -- regular element
-- hypersurface example
A = ambient B
C = A/(ideal B + ideal lift(g,A))
use C
normalElements(C,2,X)
d0' = matrix {{x,y,z}}
d1' = rightKernel(d0', DegreeLimit => 10)
d2' = rightKernel(d1', DegreeLimit => 11)
d3' = rightKernel(d2', DegreeLimit => 12)
d4' = rightKernel(d3', DegreeLimit => 13)
d5' = rightKernel(d4', DegreeLimit => 14)
d6' = rightKernel(d5', DegreeLimit => 15)
d7' = rightKernel(d6', DegreeLimit => 16)
d8' = rightKernel(d7', DegreeLimit => 17)
d9' = rightKernel(d8', DegreeLimit => 18)
d10' = rightKernel(d9', DegreeLimit => 19)
assert(0 == transpose((transpose d1')*(transpose d0')))
assert(0 == transpose((transpose d2')*(transpose d1')))
assert(0 == transpose((transpose d3')*(transpose d2')))
assert(0 == transpose((transpose d4')*(transpose d3')))
assert(0 == transpose((transpose d5')*(transpose d4')))
assert(0 == transpose((transpose d6')*(transpose d5')))
assert(0 == transpose((transpose d7')*(transpose d6')))
assert(0 == transpose((transpose d8')*(transpose d7')))
assert(0 == transpose((transpose d9')*(transpose d8')))
assert(0 == transpose((transpose d10')*(transpose d9')))

-------------------------------
-- BAD BUG!
-- ../../m2/matrix2.m2:323:71-349:40: --source code:
-- addHook((quotient, Matrix, Matrix), Strategy => Default, (opts, f, g) -> (
-- under isQuotient(ZZ,ring target f), solve(g,f) returns null and is not handled
-- correctly.
restart
errorDepth = 0
M = sub(matrix {{0},{1}},ZZ/32003)
N = sub(matrix {{1},{0}},ZZ/32003)
m = mutableMatrix(M,Dense=>true)
n = mutableMatrix(N,Dense=>true)
solve(m,n)
debug Core
rawLinAlgSolve(raw m, raw n)
M // N
M = sub(matrix {{0},{1}},ZZ)
N = sub(matrix {{1},{0}},ZZ)
M // N
M = sub(matrix {{0},{1}},QQ)
N = sub(matrix {{1},{0}},QQ)
M // N
-------------------------------

-- cool example
restart
debug needsPackage "AssociativeAlgebras"
kk = ZZ/32003
R = kk<|x,y,z,w|>
I = ideal {x*y-y*x-7*z*w-7*w*z,
           3*x*z-4*y*w-3*z*x-4*w*y,
	   31*x*w+25*y*z+25*z*y-31*w*x,
	   x*y+y*x-z*w+w*z,
	   x*z+y*w+z*x-w*y,
	   x*w-y*z+z*y+w*x}
S = R/I
d0 = matrix {{x,y,z,w}}
d1 = rightKernel d0
d2 = rightKernel d1
d3 = rightKernel d2
d4 = rightKernel d3

-- Koszul dual of S
T = homogDual S
NCGB(ideal T, 10) -- finite GB
d0 = matrix {{x,y,z,w}}
d1 = rightKernel d0
d2 = rightKernel d1
d3 = rightKernel d2
d4 = rightKernel d3
d5 = rightKernel d4

assert(0 == transpose((transpose d1)*(transpose d0)))
assert(0 == transpose((transpose d2)*(transpose d1)))
assert(0 == transpose((transpose d3)*(transpose d2)))
assert(d4 == 0)
///

DEVELOPMENT ///
restart
needsPackage "AssociativeAlgebras"
R = QQ[x]
A = R<|a,b|>
I = ideal {a}
A = R<|c,d|>
I   --- BUG: why are these parenthesis here?
-- how can we get describe to give the 
///

DEVELOPMENT ///
restart
gbTrace = 2
needsPackage "AssociativeAlgebras"
S = threeDimSklyanin (frac(QQ[a,b,c]),{a,b,c},{x,y,z}, DegreeLimit => 2)
S = threeDimSklyanin (QQ,{x,y,z}, DegreeLimit => 2)
I = ideal S
elapsedTime Igb = NCGB(I, 5, Strategy=>"F4Parallel")
R = (ambient S)/I; all(7, i -> binomial(i+2,2) == numcols ncBasis(i,R))
///

DEVELOPMENT ///
restart
debug needsPackage "AssociativeAlgebras"
kk = QQ; w=2
kk = frac(QQ[w]/ideal(w^4+1)) -- computations crash if this is used
kk = frac(QQ[w])
--kk = toField(QQ[w]/ideal(w^4+1))
A = kk <|x,y|>

--- BUG!!!
sub(1/w,A)
methods sub
code 24
methods map
code 30 -- need to fix this, but it's long...

B = A/ideal (x*y-w*y*x,x^3,y^3)
d0 = matrix {{x,y}}
d1 = rightKernel(d0, DegreeLimit => 10)
d2 = rightKernel(d1, DegreeLimit => 12)
d3 = rightKernel(d2, DegreeLimit => 14)
d4 = rightKernel(d3, DegreeLimit => 16)
d5 = rightKernel(d4, DegreeLimit => 18)
d6 = rightKernel(d5, DegreeLimit => 20)
d7 = rightKernel(d6, DegreeLimit => 22) 
d8 = rightKernel(d7, DegreeLimit => 22) -- crashes or hangs with frac kk above.  Probably a GC problem somewhere.
d9 = rightKernel(d8, DegreeLimit => 22) -- does *not* crash with toField() rather than frac()
d10 = rightKernel(d9, DegreeLimit => 22); k = 0; d = d10
d' = rightKernel(d, DegreeLimit => 22+k); k = k + 2; d = d'

A = kk <|x,y,z|>
B = A/ideal (x*y-w*y*x,x*z-w^2*z*x,y*z+w*z*y,x^3,y^3,z^3)
d0 = matrix {{x,y,z}}
d1 = rightKernel(d0, DegreeLimit => 10)
d2 = rightKernel(d1, DegreeLimit => 12)
d3 = rightKernel(d2, DegreeLimit => 14)
d4 = rightKernel(d3, DegreeLimit => 16) -- crashes or hangs with frac kk above, but not toField()
k = 0; d = d4;
d' = rightKernel(d, DegreeLimit => 13+k); k = k + 2; d = d'
///

DEVELOPMENT ///
restart
needsPackage "AssociativeAlgebras"
B = threeDimSklyanin(ZZ/32003,{3,5,7},{x,y,z})
M = coker matrix {{x,y,z}}
errorDepth = 0
C = res(M,Strategy=>10)
///

DEVELOPMENT ///
restart
needsPackage "AssociativeAlgebras"
A = QQ<|a,b,c,e,f_1,f_2|>
I1 = ideal {a*b - b*a, a*c - c*a, b*c - c*b, e*(a - b), e*c}
I1gb = NCGB(I1, 10)
-- ea, ec lead terms
-- f_1*b <--> e*(a-b)*b - e*(a*b + b*a) = -eb^2 - eba
-- f_1*c <--> e*(a-b)*c - e*(ac + ca) = -ebc - eca -> ecb - eca -> 0
-- f_3*b <--> e(ba + b^2)*b - e*b*(ab + ba) = eb^3 - eb^2a
-- f_3*c <--> e(ba + b^2)*c - e*b*(ac + ca) = eb^2c - ebca ~> ecb^2 + ecba -> ec(b^2+ba) -> 0
-- must remember when we use module elements in the reduction.
-- so the fact that f_1*c reduces to 0 gives a syzygy
-- the corresponding syzygies are:
-- f_1*c - f_2*(a+b)
-- f_1*b + f_3
-- f_3*b + f_4
-- f_3*c - f_2*(b^2+ba)
I2 = ideal {a*b + b*a, a*c + c*a, b*c + c*b, e*(a - b), e*c, f_1}
I2gb = NCGB(I2, 10)

    0     1     2     3        0    1    2    3
0   a     .     .     .                  pb   pc (or *during*)
1   .     b     c     d
2   .     c     d
3   .     d

-- e's: level 0 (just the basis of the target of the first matrix) -- a
-- f_1 |-> e*(a-b), f_2 |-> e*c (in degree 1), etc -- (wherever the gens of the gb are)
--    we are assuming we have a gb of the columns of the input matrix out to a given degree
-- from this, we can get a gb of the modules in each level (up to the degree that we computed the original gb).

-- g_1 |-> f_1*c - f_2*(a+b) lead term label e.a.c (or (e.a).c)
-- g_2 |-> f_1*b + f_3
-- g_3 |-> f_3*b + f_4
-- g_4 |-> f_3*c - f_2*(b^2+ba)

-- g_2*c <--> (f_1*b + f_3)*c - f_1(bc + cb) = f_1*cb + f_3*c - (f_1*c - f_2(a+b))*b
--                                           = f_3*c + f_2*(a+b)*b
--                                           = -f_2*ba + f_2*b^2 + f_3*c
--                                           = -f_2*ba + f_2*b^2 + f_2*(b^2+ba) = 0
-- corresponding syzygy: g_2*c - g_1*b + g_4

-- TODO: About to start the programming now.  We will begin by creating the frame (no reductions yet).
--       Then allow us to view the matrices of the frame in Macaulay2.

restart
debug needsPackage "AssociativeAlgebras"
A = QQ<|a,b,c|>
I = ideal {a*b + b*a, a*c + c*a, b*c + c*b}
B = A/I
M = matrix {{a-b,c}}
d1 = rightKernel(M, DegreeLimit => 10)
d2 = rightKernel(d1, DegreeLimit => 10)
d3 = rightKernel(d2, DegreeLimit => 10)

restart
needsPackage "AssociativeAlgebras"
A = QQ[e,a,b,c]
I1 = ideal {e*(a - b), e*c}
gens gb I1
e*(a-b)*c - e*c*a = -e*b*c = -e*c*b = 0
///

DEVELOPMENT ///
-- Examples involving Fomin-Kirillov algebras
restart
needsPackage "AssociativeAlgebras"
n = 6
indexing = p -> position(subsets(n,2), t -> t == p)
subsets2 = subsets(n,2)
subsets3 = subsets(n,3)
disjSubsets2 = select(subsets(subsets(n,2),2), p -> #( (set p#0) * (set p#1) ) == 0)
A = ZZ/32003 <|apply(subsets2, p -> x_(indexing p))|>
I = ideal (apply(gens A,       x -> x^2) | 
             apply(subsets3,     p -> x_(indexing {p#0,p#1})*x_(indexing {p#1,p#2}) - x_(indexing {p#1,p#2})*x_(indexing {p#0,p#2}) - x_(indexing {p#0,p#2})*x_(indexing {p#0,p#1})) |
             apply(subsets3,     p -> x_(indexing {p#1,p#2})*x_(indexing {p#0,p#1}) - x_(indexing {p#0,p#2})*x_(indexing {p#1,p#2}) - x_(indexing {p#0,p#1})*x_(indexing {p#0,p#2})) |
	     apply(disjSubsets2, p -> x_(indexing {p#0#0,p#0#1})*x_(indexing {p#1#0,p#1#1}) - x_(indexing {p#1#0,p#1#1})*x_(indexing {p#0#0,p#0#1})))
elapsedTime Igb = NCGB(I, 7); -- crashes (e.g. is killed, out of memory?) on the degree 11 computation.
B = A/I
elapsedTime apply(13, i -> numcols ncBasis(i,B)) -- correct for E_4
elapsedTime apply(41, i -> numcols ncBasis(i,B)) -- correct for E_5
elapsedTime apply(7, i -> numcols ncBasis(i,B)) -- correct for E_6 to degree 10
sum oo
///

DEVELOPMENT ///
-- Examples involving Fomin-Kirillov algebras
restart
debug needsPackage "AssociativeAlgebras"
n = 6
-- variables x_1..x_(n-1)
-- for each 1 \leq i < j \leq n-1
-- D_{ij}(ab) = D_{ij}(a)b + s_{ij}(a)D(b)
-- need to implement twisted derivations (either in top level, or in the engine)
kk = ZZ/32003
A = kk <| x_1..x_(n-1) |>
sigmas = hashTable apply(subsets(toList(1..n-1),2), p -> (p,map(A,A,apply(numgens A, i -> if i+1 == p#0 then A_(p#1-1) else if i+1 == p#1 then A_(p#0-1) else A_i))))
ders = hashTable apply(subsets(toList(1..n-1),2), p -> (p,derivation(A,apply(numgens A, i -> if i+1 == p#0 then -A_(p#0-1)*A_(p#1-1) else if i+1 == p#1 then A_(p#1-1)*A_(p#0-1) else 0),sigmas#p)))

deg = 10;
degprev = apply(gens A, v -> v^2)
gensSoFar = degprev
for i from 3 to deg do (
   << "Working on degree: " << i << endl;
   elapsedTime (newmons,newdeg) := coefficients(compress matrix {flatten apply(degprev, f -> apply(values ders, der -> der f))});
   degprev = flatten entries (newmons * (mingens image sub(newdeg,kk)));
   gensSoFar = gensSoFar | degprev;
   -- now remove the polynomial combinations
   elapsedTime Igb = NCGB(ideal gensSoFar, i);
   << #gensSoFar << " : " << #(flatten entries Igb) << endl;
   gensSoFar = flatten entries Igb;
   degprev = select(gensSoFar, f -> first degree f == i);
   if degprev == {} then break;
);
I = ideal gensSoFar;
Igb = NCGB(I, 10);
B = A/I
C = ZZ[T]
elapsedTime sum apply(11, i -> (numcols ncBasis(i,B))*T^i)
F5 = T^28+4*T^27+12*T^26+30*T^25+64*T^24+122*T^23+211*T^22+336*T^21+496*T^20+684*T^19+885*T^18+1078*T^17+1239*T^16+1346*T^15+1384*T^14+1346*T^13+1239*T^12+1078*T^11+885*T^10+684*T^9+496*T^8+336*T^7+211*T^6+122*T^5+64*T^4+30*T^3+12*T^2+4*T+1
F4 = T^8+3*T^7+6*T^6+9*T^5+10*T^4+9*T^3+6*T^2+3*T+1
F3 = T^3+2*T^2+2*T+1
F2 = T+1
FF = F2*F3*F4*F5
///

-- Hilbert series example. Use r >= 11 for a reasonably long test
DEVELOPMENT ///
restart
needsPackage "AssociativeAlgebras"
n = 6
r = 11
kk = ZZ/32003
R = kk <|x_1..x_n|>
I = ideal (ncBasis(2,R) * random(kk^(n^2),kk^r))
elapsedTime Igb = NCGB(I,8, Strategy=>"F4Parallel");
(flatten entries Igb) / degree // tally
///

DEVELOPMENT ///
-- this is some top-level code for testing Lundqvist's approach to NCGBs
-- 3dim sklyanin example
  restart
  needsPackage "AssociativeAlgebras"
  Lambda = (ZZ/11) <|a,b,c|>
  f1 = a*b + 2*b*a + c^2
  f2 = b*c + 2*c*b + a^2
  f3 = c*a + 2*a*c + b^2
  I = ideal (f1,f2,f3)
  Igb = elapsedTime NCGB(I,24);  
  (flatten entries Igb) / degree // tally

  restart
  gbTrace = 2
  debug needsPackage "AssociativeAlgebras"
  kk = ZZ/11
  Lambda = kk <|a,b,c|>
  f1fac = {{a,b},{b,2*a},{c,c}}
  f2fac = {{b,c},{c,2*b},{a,a}}
  f3fac = {{c,a},{a,2*c},{b,b}}
  facfs = {f1fac,f2fac,f3fac};
  I = ideal apply(facfs, facf -> facf / product // sum)
  makeMonic = f -> f*(leadCoefficient f)^(-1)

  Idm1 = I
  d = 3
  N = 10
  
  -- all the time consuming work is in reducing x.w where w is a reduced word of degree d-2 or d-1
  -- and x is a generator.  if we can do this fast, then we are in good shape.

  -- let w be a reduced word.  write w as v.y for a variable y.  can we use the fact that we can save
  -- how to reduce x.v as a sum to help here?

  -- define quotient so far for reductions
  tempA = freeAlgebraQuotient(Lambda,Idm1,gens Idm1);
  elapsedTime for d from 3 to N do (
     -- maps to get back and forth to/from free algebra
     phi = map(Lambda,tempA,gens Lambda);
     psi = map(tempA,Lambda,gens tempA);
     -- basis of A_(d-1)
     elapsedTime Adm1 = flatten entries forceNCBasis(d-1,tempA);

     -- basis of A_(d-1) \otimes A_1 considered as words in the tensor algebra     
     elapsedTime Adm11mat = ncMatrixMult(transpose matrix {Adm1 / phi}, vars Lambda);
     elapsedTime Adm11 = reverse flatten entries Adm11mat;

     -- basis of the complementary degree.  In this case, degree d-2 since all relations are quadratic
     elapsedTime Acomp = flatten entries forceNCBasis(d-2,tempA);
     
     -- lots of time is spent here
     -- compute the multiplication maps from degree d-2 to d-1 by the variables
     -- this is a nested hash table
     if d == 3 then (
        elapsedTime multMapsTemp = entries ncMatrixMult(transpose matrix {Acomp},vars tempA);
        elapsedTime multMaps = hashTable apply(#Acomp, w -> (
        	     (Acomp#w,hashTable apply(numgens tempA, v -> (tempA_v,multMapsTemp#w#v)))));
     	-- use this information to compute a basis of Nd
        elapsedTime Nd = flatten for m in Acomp list (
             apply(facfs, facf -> apply(facf, p -> {multMaps#m#(psi p#0), psi p#1}))
        );
     ) else (
        elapsedTime multMaps = hashTable apply(Acomp, w -> (
        	     (w,hashTable apply(gens tempA, v -> (v,forNextDeg#((phi w)*(phi v)))))));
        elapsedTime Nd = flatten for m in Acomp list (
             apply(facfs, facf -> apply(facf, p -> {alpha forNextDeg#((phi m)*(p#0)),psi p#1}))
        );        
     );
     
     -- lift this information to the free algebra and take the product, then take
     -- coefficients of these elements in A_(d-1) \otimes A_1 (thought of as words in Lambda)
     elapsedTime Ndmat = matrix {apply(Nd, p -> apply(p, q -> q / phi // product)) / sum};
     elapsedTime Ndcoeffs = sub(last coefficients(Ndmat, Monomials=>Adm11),kk);

     -- basis of A_d (so far, anyway)
     elapsedTime Ad = flatten entries forceNCBasis(d,tempA);

     -- most time is spent here
     -- matrix representation of the projection map from A_(d-1) \otimes A_1 --> A_d
     -- would like to tell reduction system to only look for suffixes    
     elapsedTime imAdm11 = flatten entries psi matrix {Adm11};
     
     -- save these calculations for next degree
     elapsedTime forNextDeg = hashTable apply(#Adm11, i -> (Adm11#i,imAdm11#i));
     elapsedTime projMap = sub(last coefficients(matrix {imAdm11}, Monomials => Ad),kk);
     -- matrix representation of the inclusion map from A_d --> A_(d-1) \otimes A_1
     elapsedTime inclMap = sub(last coefficients(phi matrix {Ad}, Monomials => Adm11),kk);
     -- project, lift back up and take mingens to autoreduce
     elapsedTime downAndBack = mingens image (inclMap * projMap * Ndcoeffs);
     
     -- lift these new elements to the free algebra
     elapsedTime newElts = flatten entries ((matrix {Adm11}) * downAndBack);

     -- enlarge ideal and move on to the next degree
     Idm1 = Idm1 + ideal newElts;
     
     -- update the ring for the new relations
     prevTempA = tempA;
     tempA = freeAlgebraQuotient(Lambda,Idm1,gens Idm1);

     -- I don't know how to put an element in a quotient without rewriting at top level
     -- so I am using this ring map to do the work.
     alpha = map(tempA,prevTempA,gens tempA); 
    
     << "Completed degree " << d << "." << endl;
  )
-- only about 3.5x slower than engine code at the moment.
///

DEVELOPMENT ///
-- this is some top-level code for testing Lundqvist's approach to NCGBs
-- 4dim sklyanin example
  restart
  needsPackage "AssociativeAlgebras"
  Lambda = (ZZ/11) <|x,y,z,w|>
  f1 = x*y-y*x-7*z*w-7*w*z
  f2 = 3*x*z-4*y*w-3*z*x-4*w*y
  f3 = 31*x*w+25*y*z+25*z*y-31*w*x
  f4 = x*y+y*x-z*w+w*z
  f5 = x*z+y*w+z*x-w*y
  f6 = x*w-y*z+z*y+w*x
  I = ideal (f1,f2,f3,f4,f5,f6)
  Igb = elapsedTime NCGB(I,12);  
  (flatten entries Igb) / degree // tally

  restart
  debug needsPackage "AssociativeAlgebras"
  kk = ZZ/11
  Lambda = kk <|x,y,z,w|>
  -- need to interreduce these input polys so the input is a GB so far
  f1fac = {{x,y},{z,-4*w},{w,-3*z}}
  f2fac = {{x,z},{z,-3*x},{w,2*y}}
  f3fac = {{x,w},{z,-5*y},{w,5*x}}
  f4fac = {{y,x},{z,3*w},{w,4*z}}
  f5fac = {{y,w},{z,4*x},{w,-3*y}}
  f6fac = {{y,z},{z,5*y},{w,4*x}}
  facfs = {f1fac,f2fac,f3fac,f4fac,f5fac,f6fac};
  I = ideal apply(facfs, facf -> facf / product // sum)
  
  makeMonic = f -> f*(leadCoefficient f)^(-1)

  Idm1 = I
  d = 3
  N = 12
  
  -- all the time consuming work is in reducing x.w where w is a
  -- reduced word of degree d-2 or d-1 and x is a generator.  if we
  -- can do this fast, then we are in good shape.

  -- let w be a reduced word.  write w as v.y for a variable y.  can
  -- we use the fact that we can save how to reduce x.v as a sum to
  -- help here?

  -- w deg d-1
  -- x.w = x.w'y = \sum c_i w_i.y
  
  -- A_1 ** A_(d-2) ** A_1 --> A_1 ** A_(d-1) --> A_d
  -- A_1 ** A_(d-2) ** A_1 --> A_(d-1) ** A_1 --> A_d
  
  -- define quotient so far for reductions
  tempA = freeAlgebraQuotient(Lambda,Idm1,gens Idm1);
  elapsedTime for d from 3 to N do (
     -- maps to get back and forth to/from free algebra
     phi = map(Lambda,tempA,gens Lambda);
     psi = map(tempA,Lambda,gens tempA);
     -- basis of A_(d-1)
     elapsedTime Adm1 = flatten entries forceNCBasis(d-1,tempA);

     -- basis of A_(d-1) \otimes A_1 considered as words in the tensor algebra     
     elapsedTime Adm11mat = ncMatrixMult(transpose matrix {Adm1 / phi}, vars Lambda);
     elapsedTime Adm11 = reverse flatten entries Adm11mat;

     -- basis of the complementary degree.  In this case, degree d-2 since all relations are quadratic
     elapsedTime Acomp = flatten entries forceNCBasis(d-2,tempA);
     
     -- lots of time is spent here
     -- compute the multiplication maps from degree d-2 to d-1 by the variables
     -- this is a nested hash table
     if d == 3 then (
        elapsedTime multMapsTemp = entries ncMatrixMult(transpose matrix {Acomp},vars tempA);
        elapsedTime multMaps = hashTable apply(#Acomp, w -> (
        	     (Acomp#w,hashTable apply(numgens tempA, v -> (tempA_v,multMapsTemp#w#v)))));
     	-- use this information to compute a basis of Nd
        elapsedTime Nd = flatten for m in Acomp list (
             apply(facfs, facf -> apply(facf, p -> {multMaps#m#(psi p#0), psi p#1}))
        );
     ) else (
        elapsedTime multMaps = hashTable apply(Acomp, w -> (
        	     (w,hashTable apply(gens tempA, v -> (v,forNextDeg#((phi w)*(phi v)))))));
        elapsedTime Nd = flatten for m in Acomp list (
             apply(facfs, facf -> apply(facf, p -> {alpha forNextDeg#((phi m)*(p#0)),psi p#1}))
        );        
     );
     
     -- lift this information to the free algebra and take the product, then take
     -- coefficients of these elements in A_(d-1) \otimes A_1 (thought of as words in Lambda)
     elapsedTime Ndcoeffs = last coefficients(matrix {apply(Nd, p -> apply(p, q -> q / phi // product)) / sum}, Monomials=>Adm11);
     Ndcoeffs = sub(Ndcoeffs, kk);

     -- basis of A_d (so far, anyway)
     elapsedTime Ad = flatten entries forceNCBasis(d,tempA);

     -- most time is spent here
     -- matrix representation of the projection map from A_(d-1) \otimes A_1 --> A_d
     -- would like to tell system to only look for suffixes

     -- need to save the calculations of psi Adm11 below in a hash table for next iteration's
     -- Acomp * (vars tempA) in multMapsTemp
     elapsedTime imAdm11 = flatten entries psi matrix {Adm11};
     elapsedTime forNextDeg = hashTable apply(#Adm11, i -> (Adm11#i,imAdm11#i));
     elapsedTime projMap = sub(last coefficients(matrix {imAdm11}, Monomials => Ad),kk);

     -- matrix representation of the inclusion map from A_d --> A_(d-1) \otimes A_1
     elapsedTime inclMap = sub(last coefficients(phi matrix {Ad}, Monomials => Adm11),kk);
     -- project, lift back up and take mingens to autoreduce
     elapsedTime downAndBack = mingens image (inclMap * projMap * Ndcoeffs);
     
     -- lift these new elements to the free algebra
     elapsedTime newElts = flatten entries ((matrix {Adm11}) * downAndBack);

     -- enlarge ideal and move on to the next degree
     Idm1 = Idm1 + ideal newElts;
     
     -- update the ring for the new relations
     prevTempA = tempA;
     tempA = freeAlgebraQuotient(Lambda,Idm1,gens Idm1);

     -- I don't know how to put an element in a quotient without rewriting at top level
     -- so I am using this ring map to do the work.
     alpha = map(tempA,prevTempA,gens tempA); 
    
     << "Completed degree " << d << "." << endl;
  )

-- now about 16x slower than engine code at the moment.
///

DEVELOPMENT ///
R = QQ[x,y,z]
f = x^2 - y*z
g = x*y
-- A = R/f
d = 3
-- C_3^*
(x ** y - y ** x)*x = x ** xy - y ** x^2 = -y ** yz           ~> -y^2z
(x ** y - y ** x)*y = x ** y^2 - y ** xy = x ** y^2           ~> 0
(x ** y - y ** x)*z = x ** yz - y ** xz  = x ** yz - y ** xz  ~> 0

(x ** z - z ** x)*x = x ** xz - z ** x^2 = x ** xz - z ** yz  ~> 0
(x ** z - z ** x)*y = x ** yz - z ** xy  = x ** yz            ~> 0
(x ** z - z ** x)*z = x ** z^2 - z ** xz = x ** z^2 - z ** xz ~> 0

(y ** z - z ** y)*x = y ** xz - z ** xy  = y ** xz            ~> 0
(y ** z - z ** y)*y = y ** yz - z ** y^2 = y ** yz - z ** y^2 ~> 0
(y ** z - z ** y)*z = y ** z^2 - z ** yz = y ** z^2 - z ** yz ~> 0
-- N_3^* = 0

R = QQ[x,y,z]
f = x^2 - y*z
g = x*y
-- A = R/f
d = 3
-- C_3
(x ** y - y ** x)*x = x ** oxy - x * xy   = 0
(x ** y - y ** x)*y = x ** y^2 - y ** xy = x ** y^2           ~> 0
(x ** y - y ** x)*z = x ** yz - y ** xz  = x ** yz - y ** xz  ~> 0

(x ** z - z ** x)*x = x ** xz - z ** x^2 = x ** xz - z ** yz  ~> 0
(x ** z - z ** x)*y = x ** yz - z ** xy  = x ** yz            ~> 0
(x ** z - z ** x)*z = x ** z^2 - z ** xz = x ** z^2 - z ** xz ~> 0

(y ** z - z ** y)*x = y ** xz - z ** xy  = y ** xz            ~> 0
(y ** z - z ** y)*y = y ** yz - z ** y^2 = y ** yz - z ** y^2 ~> 0
(y ** z - z ** y)*z = y ** z^2 - z ** yz = y ** z^2 - z ** yz ~> 0
-- N_3^* = 0

A_3 <--> dim 4
A_1 ** A_2 <--> dim 3*4 = 12

d = 4
-- C_d
(x ** y - y ** x)*x^2 = 0
(x ** y - y ** x)*xy = x ** xy^2
(x ** y - y ** x)*y^2 = x ** y^3 - y ** xy^2
-- N_d = 0

A_1 ** A_3 <--> 4
C_4 <--> 2
A_4 <--> 2 diml
///
