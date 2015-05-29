-- -*- coding: utf-8 -*-
-- test of matrix routines
testMatOpR = (R,m,n) -> (
  assert(m + n == n + m);
  assert(- (-m) == m);
  assert((m+n)^2 == m * m + 2 * m * n + n * n);
  assert(R_0 * m == matrix(R,{R_0}) ** m);
  assert(transpose transpose m == m);
  assert(m - n == m + (-n));
  assert(numgens flatten m == (numgens target m)(numgens source m));
  assert(m ++ n == map(target m ++ target n, source m ++ source n,
        (i,j) -> if i<numgens target m
                   then if j<numgens source m
                     then m_(i,j)
                     else 0
                   else if j<numgens source m
                     then 0
                     else n_(i-numgens target m, j - numgens source m)))
  )

constructTest = () -> (
  -- matrix construction(genericMatrix, genericSkewMatrix, id, zeroMatrix)
  R = ZZ/101[vars(0..14)];

  -- first we test the various versions of the matrix command
  -- we assume that the version 'matrix(R, {{...},...,{...}})' is working.
  assert(matrix {{a,b},{c,d},{e,f}} == matrix(R, {{a,b},{c,d},{e,f}}));

  -- assert(matrix(R, {a-b^2, c*d - e*f}) == matrix(R, {{a-b^2, c*d - e*f}}))

  assert(matrix table(1, 4, (j,i) -> a^(i+1) + b^(i+1)) ==
         matrix {{a+b, a^2 + b^2, a^3 + b^3, a^4 + b^4}});

--  assert(map(R^2, {{a,b,4},{d,0,f}}) == matrix(R, {{a,b,4},{d,0,f}}));
--  assert(map(R^2, {{a,b,c},{d,e,f}}) == matrix(R, {{a,b,c},{d,e,f}}));
--  assert(map(R^{-1,-1}, R^{-2,-2,-2}, {{a,b,c},{d,e,f}})
--           == matrix(R, {{a,b,c},{d,e,f}}));

  -- matrix m -- should give back the same vectors (if possible) as m.
  
  m1 = matrix {{a,b},{c,d}};
  m2 = matrix {{e},{f}};
  m3 = id_(R^3);
  mm = m1 | m2 || m3;
  assert(mm == matrix(R, {{a,b,e},
                          {c,d,f},
                          {1,0,0},
                          {0,1,0},
                          {0,0,1}}));

  assert(genericMatrix(R,a,3,5) == 
         matrix {{a,d,g,j,m}, 
                 {b,e,h,k,n},
                 {c,f,i,l,o}});
  assert(genericSkewMatrix(R,a,5) == 
         matrix {{ 0, a, b, c, d},
                 {-a, 0, e, f, g},
                 {-b,-e, 0, h, i}, 
                 {-c,-f,-h, 0, j}, 
                 {-d,-g,-i,-j, 0}});
  F = R^{0,1,2};
  m = id_F;
  assert(id_F == map(F, F, {{1,0,0}, {0,1,0}, {0,0,1}}));
  assert(target m == F);
  assert(source m == F);

  n1 = m - m;
  n2 = map(F,F,0);
  assert(n1 == n2);
  assert(source n1 == source n2);
  assert(target n1 == target n2);
  )

arithTest = () -> (
  -- tests some arithmetic, genericMatrix, pfaffians, minors, transpose
  x = symbol x;
  y = symbol y;
  R = ZZ/101[x_1 .. x_9, y_1 .. y_9];
  m1 = genericMatrix(R,x_1,3,3);
  m2 = genericMatrix(R,y_1,3,3);
  assert(m1 - m1 == 0);
  assert((m1 + m2)*m1 == m1*m1 + m2*m1);
  assert((-m1) + m1 == 0);
  m = genericSkewMatrix(R,x_1,6);
  assert(m + transpose m == 0);
  assert(pfaffians(5,m) == 0);

  -- matrix arithmetic
  --m = matrix table (3, 3, (i,j)->(R_(i+3*j))^2)  -- incorrect at moment
  )

submatTest = () -> (
  -- test submatrices and selection
  R = ZZ/5[vars(0..11)];
  m = genericMatrix(R,a,3,4);
  n = submatrix(m,{0..2},{2,3});
  assert(n == submatrix(m, {2,3}));
  --assert(submatrix(m, {7}, {0,1,2}) == 0);
  assert(submatrix(m, {0..2}, {2}) == matrix {{g},{h},{i}});

  R = ZZ/5[vars(0..9)];
  m = matrix table(3, 3, (i,j)->(R_(i+3*j))^2);
  m = m*m;
  assert(m_(1,2) == b^2*g^2 + e^2*h^2 + h^2*i^2);
  assert(m_(1,2) == m_2_1);
  )

detTest = () -> (
  -- pfaffians and minors
  R = ZZ/101[vars(0..14)];
  assert(generators pfaffians(4, genericSkewMatrix(R,a,4))
       == matrix {{c*d - b*e + a*f}});
  m = genericSkewMatrix(R,a,6); 
  m1 = generators pfaffians(6, m); 
  assert (ideal(m1*m1) == minors(6,m)))

degTest = () -> (
  -- This is a test of degrees, homogeneity checking
  -- First usual degrees
  R = ZZ/101[symbol a..symbol f];
  F = R^{-4, -2};
  m = map(F,, {{a, b^2}, {c^3, d^4}});
  assert(isHomogeneous m);
  assert(isHomogeneous transpose m);

  R = ZZ/101[symbol a..symbol f, Degrees => {3,4,5,7,9,10}];
  F = R^{-5,-8};
  m = map(F,, {{f - c^2, e}, {d, a^2}});
  assert(isHomogeneous m);
  assert(isHomogeneous transpose m);

  R = ZZ/101[symbol a..symbol f, Degrees => {{1, 1}, {1, 2}, {1, 3}, {1, -1}, {1, -2}, {1, -3}}];
  F = R^{{-1,-2}};
  m = map(F, R^0, 0);
  --n = basis({5,2}, m);

  -- homogeneity and degrees
  R = ZZ/7[vars(0..8)];
  m = genericMatrix(R, a, 3, 3);
  m2 = m * m;
  assert isHomogeneous m2;
  assert (target m2 == target m);
  assert (source m2 == R^{-2,-2,-2});
  assert (degree m2_(0,0) == {2});
  assert (degree m2_0 == {2});
  )

randomTest = () -> (
  -- random matrices 
  A = ZZ/31991[a..d];
  m = random(A^{0,1}, A^{1,0,-1,-2});
  assert(m_(0,0) == 0);
  assert(degree m_(0,1) == {0}  and isHomogeneous m_(0,1));
  assert(degree m_(0,2) == {1}  and isHomogeneous m_(0,2));
  assert(degree m_(0,3) == {2}  and isHomogeneous m_(0,3));
  assert(degree m_(1,0) == {0}  and isHomogeneous m_(1,0));
  assert(degree m_(1,1) == {1}  and isHomogeneous m_(1,1));
  assert(degree m_(1,2) == {2}  and isHomogeneous m_(1,2));
  assert(degree m_(1,3) == {3}  and isHomogeneous m_(1,3));
    --MES: also check bihomogeneous random matrix
  )

diffTest = () -> (
  -- tests diff, contract, Jacobian
  )

opsTest = () -> (
  -- tests **, ++, koszul, symmetricPower

  -- koszul
  R = ZZ/107[vars(0..4)];
  m1 = koszul(4, vars R);
  v = matrix {{a,b,c,d}};
  m2 = matrix {{koszul(4,v), -e * id_(R^4)}, 
               {map(R^6, R^1, 0), koszul(3,v)}};
  assert(m1 - m2 == 0);

  -- reshape(implicitly), adjoint, adjoint', flatten, flip
  R = ZZ/101[vars(0..23)];
  m1 = genericMatrix(R,a,4,6);
  m2 = adjoint(m1, R^{-1,-1}, R^3);
  m3 = adjoint'(m2, R^3, R^4);
  assert(m1 == m3);
  assert(flatten m1 == flatten m2);

  flip1 = flip(R^{-1,-1}, R^3);
  flip2 = flip(R^3, R^{-1,-1});
  assert(flip1 * flip2 == id_(R^{-1,-1} ** R^3));

  -- test transpose, adjoints
  -- start with m : F --> G ** H
  F = R^{-1,-1,-1,-1};
  G = R^3;
  HH = R^2;
  m = genericMatrix(R,a,6,4);
  m1 = transpose reshape(HH ** dual F, dual G, transpose m);
  m2a = reshape(HH, F ** dual G, m);
  m2 = reshape(G, dual HH ** F, transpose m2a);
  assert(m1 == m2);
  assert(source m1 == source m2);
  assert(target m1 == target m2);

  -- test transpose, adjoint, flip
  -- idea: given m : F --> G, madj : F ** dual G --> R,
  -- then the transpose is:  dual G ** F --> F ** dual G --> R,
  -- take adjoint:
  -- use the 'm' above.
  fliptranspose := m -> (
    F = source m; G = target m;
    reshape(dual F, dual G, (flatten m) (flip(F, dual G))));
  assert(transpose m == fliptranspose m);
  )

hilbTest = () -> (
  -- test of Hilbert functions (these will compute Gröbner bases)

  -- first a very simple test
  R = ZZ/101[symbol a..symbol c];
  m = matrix {{a^2, a * b, b^2}};
  F = poincare cokernel m;
  use ring F;  -- this allows T to be used
  assert(F == 1 - 3 * T^2 + 2 * T^3);

  -- now for the 2 by 2's of a generic 3 by 3.
  R = ZZ/101[vars(0..8)];
  m = genericMatrix(R, a, 3, 3);
  j = generators minors(2, m);
  assert(poincare cokernel j == 1 - 9 * T^2 + 16 * T^3 - 9 * T^4 + T^6);

  -- now for the 3 by 3 commuting matrices
  needs "../examples/commuting.m2";
  assert(poincare cokernel commuting 3 == 1 - 8 * T^2 + 2 * T^3 + 31 * T^4 - 32 * T^5 - 25 * T^6
                         + 58 * T^7 - 32 * T^8 + 4 * T^9 + T^10);

  -- now for quasi homogeneous
  R = ZZ/101[symbol a..symbol d, Degrees=>{1,2,3,4}];
  m = matrix {{a^3, a*b*c, a*d^2, c^4}};
  assert(poincare cokernel m == 1-T^3-T^6+T^8-T^9+T^11-T^12+T^14+2*T^15
                         -T^16-T^17+T^21-2*T^23+T^25);

  -- now for multi-degrees
  R = ZZ/101[symbol a..symbol d, Degrees=>{{1,1,0,0,0},{1,0,1,0,0},{1,0,0,1,0},{1,0,0,0,1}}];
  m = matrix{{a^3, a*b*c, a*d^2, c^4}};
  F = poincare cokernel m;
  assert(F == poincare resolution m);
  T = ring F;
  assert(poincare cokernel m == 1-T_0^3*T_1*T_4^2-T_0^3*T_1*T_2*T_3-T_0^3*T_1^3
                   -T_0^4*T_3^4+T_0^5*T_1*T_2*T_3*T_4^2+T_0^5*T_1^3*T_4^2
                   +T_0^5*T_1^3*T_2*T_3+T_0^6*T_1*T_2*T_3^4
                   +T_0^7*T_1*T_3^4*T_4^2+T_0^7*T_1^3*T_3^4
                   -T_0^7*T_1^3*T_2*T_3*T_4^2-T_0^8*T_1*T_2*T_3^4*T_4^2
                   -T_0^8*T_1^3*T_2*T_3^4-T_0^9*T_1^3*T_3^4*T_4^2
                   +T_0^10*T_1^3*T_2*T_3^4*T_4^2);

  )

testMatrix = () -> (
  -- some things are tested implicitly all over: 
  -- m == n, m == 0, ring m, source m, target m, matrix
  << "matrix tests...";
  constructTest();
  arithTest();
  submatTest();
  detTest();
  opsTest();
  degTest();
  randomTest();
  hilbTest();

  -- not yet tested: matrix * enginevector,
  -- concatenate (will be placed into 'matrix'
  << "passed\n";
  )

randomElement2 = (m) -> (
     R := ring m;
     p := char R;
     (m * matrix table(rank ambient source m, 1, (i,j) -> i -> (random p)_R))_(0,0))

randomMatrix2 = (F,G) -> (
    kb := new MutableHashTable;
    dF := degrees F;
    dG := degrees G;
    zeromat := matrix((ring F)^1, (ring F)^0, 0);
    scan(dF, d -> scan(dG, e -> (
        deg := apply(# d, i -> e_i - d_i);
        if not kb#?deg then
          kb#deg = basis(deg, zeromat);
        )));
    matrix table (rank ambient F, rank ambient G, (i,j) -> (
        d := dF_i;
        e := dG_j;
        deg := apply(# d, i -> e_i - d_i);
        randomElement2 kb_deg)))


R = QQ[x]
assert ( 0 == map(R^1,R^1,0) )
assert ( 0 == map(R^1,R^1,0,Degree => 5) )
assert ( {0} == degree map(R^1,R^1,0) )
assert ( {5} == degree map(R^1,R^1,0,Degree => 5) )
assert isHomogeneous map(R^{5},R^1,{{1}},Degree => -5)
assert isHomogeneous map(R^{5},R^1,1,Degree => -5)
assert ( {-5} == degree map(R^{5},R^1,{{1}},Degree => -5))
assert ( {-5} == degree map(R^{5},R^1,1,Degree => -5))
assert ( {5} == degree map(R^1,R^1,1,Degree => 5) )
assert ( {5} == degree map(R^1,R^1,2,Degree => 5) )
f = map(R^1, R^1, {{x^2}}, Degree => 2)
g = map(R^1, R^1, {{x^1}}, Degree => 1)
assert isHomogeneous f
assert isHomogeneous g
isHomogeneous(f|g)
isHomogeneous(g|f)
degree(f|g)
degree(g|f)
h = modulo(f,g)
assert isHomogeneous h
h = modulo(g,f)
assert isHomogeneous h

-- testMatrix()


end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test matrix.out"
-- End:
