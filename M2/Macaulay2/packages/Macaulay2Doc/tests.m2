-- these tests were in random places among the documentation
-- TODO: move them somewhere appropriate
TEST ///
     k = ZZ/101
     R = k[a,b,c,d]/(a^4+b^4+c^4+d^4)
     X = Proj R
     result = table(3,3,(p,q) -> timing ((p,q) => rank HH^q(cotangentSheaf(p,X))))
     assert( {{1, 0, 1}, {0, 20, 0}, {1, 0, 1}} === applyTable(result,last@@last) )
     print new MatrixExpression from result
     ///

-- Example 4.1: the bounds can be sharp.
TEST ///
     S = QQ[w,x,y,z];
     X = Proj S;
     I = monomialCurveIdeal(S,{1,3,4})
     N = S^1/I;
     assert(Ext^1(OO_X,N~(>= 0)) == prune truncate(0,Ext^1(truncate(2,S^1),N)))
     assert(Ext^1(OO_X,N~(>= 0)) != prune truncate(0,Ext^1(truncate(1,S^1),N)))
     ///

-- Example 4.2: locally free sheaves and global Ext.
TEST ///
     S = ZZ/32003[u,v,w,x,y,z];
     I = minors(2,genericSymmetricMatrix(S,u,3));
     X = variety I;
     R = ring X;
     Omega = cotangentSheaf X;
     OmegaDual = dual Omega;
     assert(Ext^1(OmegaDual, OO_X^1(>= 0)) == Ext^1(OO_X^1, Omega(>= 0)))
     ///

-- Example 4.3: Serre-Grothendieck duality.
TEST ///
     S = QQ[v,w,x,y,z];
     X = variety ideal(w*x+y*z,w*y+x*z);
     R = ring X;
     omega = OO_X^{-1};
     G = sheaf cokernel genericSymmetricMatrix(R,R_0,2);
     assert(Ext^2(G,omega) == dual HH^0(G))
     assert(Ext^1(G,omega) == dual HH^1(G))
     assert(Ext^0(G,omega) == dual HH^2(G))
     ///

///
R = QQ[vars(0..24)]
f = () -> (alarm 4; try res coker vars R else "ran out of time")
time f()
///

TEST "
fib = memoize( n -> if n <= 1 then 1 else fib(n-1) + fib(n-2) )
assert ( fib 10 == 89 )
"

TEST "
a = 0
f = memoize ( x -> ( a = a + 1; true ))
f 1
f 2
f 3
f 1
f 2
f 3
f 1
f 2
f 3
assert( a == 3 )
"


TEST "
x = set {1,2,3}
y = set {3,4,5}
assert( member(2,x) )
assert( not member(x,x) )
assert( sum y === 12 )
assert( product y === 60 )
assert ( x * y === set {3} )
assert ( x ** y === set {
	  (3, 4), (2, 5), (3, 5), (1, 3), (2, 3), (1, 4), (3, 3), (1, 5), (2, 4)
	  } )
assert ( x - y === set {2, 1} )
assert ( x + y === set {1, 2, 3, 4, 5} )
assert ( toString x === \"set {1, 2, 3}\" )
"

TEST ///
frac(QQ[a,b])
assert ( a == denominator(b/a) )
assert ( b == numerator(b/a) )
assert ( 1 == numerator(b/b) )
///

TEST ///
     assert( net (2/1) === "2" )
     assert( net (1/1) === "1" )
///

TEST "
-- test name
R = ZZ/101[a..e]
f = symmetricPower(2,vars R)
assert( f == value toExternalString f )
assert( f == value toString f )
"

TEST "
R = ZZ/101[a,b,c]
M = cokernel matrix {{a,b^2,c^3}}
N = image M_{0}
assert( M == N )
"

TEST "
assert isDirectSum (QQ^1 ++ QQ^2)
assert isDirectSum (QQ^1 ++ QQ^2)
"

TEST ///
     R = ZZ[x_1..x_12,y]
     f = genericMatrix(R,3,4)
     assert(source (f_{1,2}) == R^{-1,-1})
     assert(target (f_{1,2}) == target f)
     M1 = (target f)/(y * target f)
     M2 = (source f)/(y * source f)
     g = map(target f,M2,f)
     h = map(M1,M2,f)
     k = submatrix(g, {1})
     assert(target k === target g)
     l = submatrix(h, {1})
     assert(target l === target h)
     assert(source l === R^{-1})
     m = submatrix(h, {1,2},{2,3})
     assert(target m === R^2)
     assert(source m === R^{2:-1})
     n = submatrix(h, {1,2}, )
     assert(target n === R^2)
     assert(source n === source h)
///


TEST ///
  -- test of submatrixByDegrees
  R = QQ[a..d]
  I = ideal"a2b-c3,abc-d3,ac2-bd2-cd2,abcd-c4"
  C = res I
  submatrixByDegrees(C.dd_2, (3,3),(6,6))
  submatrixByDegrees(C.dd_2, ({3},{3}),({6},{6}))
  submatrixByDegrees(C.dd_2, ({4},{4}),({},{}))
  submatrixByDegrees(C.dd_2, ({3},{3}),({7},{7}))
  F = source C.dd_2
  -- rawSelectByDegrees(raw F, {-4}, {-3})
  -- rawSelectByDegrees(raw F, {}, {8})
///

TEST "
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
A = R/I
jacobian A
singA = minors(codim ideal presentation A, jacobian A)
generators gb singA
"

TEST "
R=ZZ/101[a..d]
f = matrix {{a}}
assert( isHomogeneous f )
g = reshape(R^1, R^{-1}, f)
assert isHomogeneous g
"

TEST "
ZZ[t]
assert (matrix {{t}} ** matrix {{t}} == matrix{{t^2}})
"

TEST "
R = ZZ/101[x,y,z]
I = ideal(x,y)
assert( 1 == dim I )
assert( 2 == codim I )
"

TEST "
     R=ZZ/101[x,y,z]
     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x + z) } === 0 )
     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x - z) } === 1 )
     S = ZZ/103[a..d]
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + b^2 + 3*c^2 + 2*d^2 } === 1 )
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + 5*b^2 + 3*c^2 + 2*d^2 } === 0 )
     "
TEST "
R = ZZ/101[a]
assert isInjective R^2_{0}
assert not isInjective R^2_{0,0}
assert isSurjective R^2_{0,0,1}
assert not isSurjective R^2_{1}
"

TEST ///
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover exteriorPower(2,M) == exteriorPower(2,cover M) ))
///
TEST "
R = ZZ/101[x];
k = coker vars R;
M = R^3 ++ k^5;
assert( fittingIdeal(0,M) == ideal 0_R )
assert( fittingIdeal(1,M) == ideal 0_R )
assert( fittingIdeal(2,M) == ideal 0_R )
assert( fittingIdeal(3,M) == ideal x^5 )
assert( fittingIdeal(4,M) == ideal x^4 )
assert( fittingIdeal(5,M) == ideal x^3 )
assert( fittingIdeal(6,M) == ideal x^2 )
assert( fittingIdeal(7,M) == ideal x )
assert( fittingIdeal(8,M) == ideal 1_R )
assert( fittingIdeal(9,M) == ideal 1_R )
"
TEST ///
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    table(modules, modules, (P,Q) -> assert(cover P ** cover Q == cover (P ** Q)));
///
TEST "
scan(3, n -> scan(-3 .. 3, d -> (
	       h := projectiveHilbertPolynomial(n,d);
	       scan(3, i -> assert( h i === binomial(n+d+i,n) )))))
"
TEST "
r = ZZ/101[a,b]
assert ( 2 * degree (a * b^2) === {6} )
M = cokernel matrix (r,{{1}})
assert ( isFreeModule prune M )
"
TEST "
GF(8,Variable => x)
assert ( det matrix{{x,1},{x^2,x^3}} == x^4 - x^2 )
"
TEST "
R = ZZ/101[a..f]
M = cokernel matrix (R, {{1},{-1}})
N = prune M
p = N.cache.pruningMap
assert( source p == N )
assert( target p == M )
assert( prune kernel p == 0 )
assert( prune cokernel p == 0 )
assert isIsomorphism p
assert isIsomorphism p^-1
assert ( p * p^-1 == id_M )
assert ( p^-1 * p == id_N )
"
TEST ///
S = ZZ/101[a..d]
I = monomialCurveIdeal(S, {1,3,4})
R = S/I
use R
J = module ideal(a,d)
K = module ideal(b^2,c^2)
JK = Hom(J,K)
f = JK_{0}
g = homomorphism f
assert isHomogeneous g
assert ( source g === J )
assert ( target g === K )
f' = homomorphism' g
assert (f-f' == 0)
assert (degrees f' === {{{1}}, {{0}}})
assert (degrees f === {{{1}}, {{1}}})
assert (degree f == {0})
assert (degree f' == {1})
///

     TEST ///
	  R = ZZ[x]
	  m = random(R^2,R^{-1,-2,-3,-4,-5})
	  F = target m
	  f = rank F
	  P = source m
	  p = rank P
	  n = random(R^3,R^{-6,-7,-8,-9})
	  G = target n
	  g = rank G
	  Q = source n
	  q = rank Q
	  h = contract(m,n)
	  assert( source h === dual P ** Q )
	  assert( target h === dual F ** G )
	  scan(f, i -> scan(g, j -> scan(p, k -> scan(q, l -> assert( h_(g*i+j,q*k+l) === contract(m_(i,k),n_(j,l)))))))
	  ///
     TEST ///
	  R = ZZ[x]
	  m = random(R^2,R^{-6,-7,-8,-9})
	  F = target m
	  f = rank F
	  P = source m
	  p = rank P
	  n = random(R^3,R^{-1,-2,-3,-4,-5})
	  G = target n
	  g = rank G
	  Q = source n
	  q = rank Q
	  h = contract'(m,n)
	  assert( source h === P ** dual Q )
	  assert( target h === F ** dual G )
	  scan(f, i -> scan(g, j -> scan(p, k -> scan(q, l -> assert( h_(g*i+j,q*k+l) === contract(n_(j,l),m_(i,k)))))))
	  ///
     TEST ///
	  R = ZZ[x]
	  m = random(R^2,R^{-6,-7,-8,-9})
	  F = target m
	  f = rank F
	  P = source m
	  p = rank P
	  n = random(R^3,R^{-1,-2,-3,-4,-5})
	  G = target n
	  g = rank G
	  Q = source n
	  q = rank Q
	  h = diff'(m,n)
	  assert( source h === P ** dual Q )
	  assert( target h === F ** dual G )
	  scan(f, i -> scan(g, j -> scan(p, k -> scan(q, l -> assert( h_(g*i+j,q*k+l) === diff(n_(j,l),m_(i,k)))))))
	  ///

TEST "
R=ZZ/101[a..f]
assert( degrees( R^{1} ++ R^{2} ) == {{-1}, {-2}} )
assert( degrees (R^{1,2} ** R^{3,5}) == {{-4}, {-6}, {-5}, {-7}} )
assert( numgens R^6 == 6 )
assert( rank R^6 == 6 )
f = vars R
M = cokernel (transpose f * f)
assert ( rank M == 5 )
assert ( rank kernel f == 5 )
assert ( rank cokernel f == 0 )
assert(R^{0,0} == R^2)
assert(R^{0,0} != R^{0,1})
"

TEST "
R = ZZ/103[a..c]
C = resolution cokernel vars R
assert(regularity C === 0)
R = ZZ/101[a .. r]
M = cokernel genericMatrix(R,a,3,6)
time C = resolution M
assert(regularity C === 2)
f = symmetricPower(2,vars R)
assert(f%a + a * (f//a) == f)
"

TEST "
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (3,toList (t_1 .. t_9))			  -- 3 by 3
n = matrix pack (3,toList (u_1 .. u_9))			  -- 3 by 3
j = flatten (m * n - n * m)
k = flatten (m * n - n * m)
G = gb j
jj = generators G
assert( numgens source jj == 26 )
T = (degreesRing S)_0
assert( poincare cokernel j == 1-8*T^2+2*T^3+31*T^4-32*T^5-25*T^6+58*T^7-32*T^8+4*T^9+T^10 )
v = apply(7, i -> numgens source generators gb(k,DegreeLimit => i) )
assert (v  === {0, 0, 8, 20, 25, 26, 26} )
"

TEST "
R = ZZ/101[a..d]
A = image matrix {{a}}
B = image matrix {{b}}
f = inducedMap((A+B)/A, B/intersect(A,B))
assert isIsomorphism f
g = f^-1
assert( f^-1 === g )			  -- check caching of inverses
assert( f*g == 1 )
assert( g*f == 1 )
assert isWellDefined f
assert isWellDefined g
assert not isWellDefined inducedMap(R^1,cokernel matrix {{a}},Verify => false)
"

TEST "
S = ZZ/107[vars ( 0 .. 5 ) ]

g = matrix {{a*b*c - d*e*f, a*d^2 - e^3, a*e^2 - b*c*e}}
k = syz g
assert( numgens source k === 4 )

t = (a + b + c)^4
u = (a + b + c) * b^3
v = a * t + b * u
w = c * t - d * u
x = b * t + f * u

h = matrix {{t,u,v,w,x}}
h1 = mingens image h

so = m -> m_(sortColumns m)

assert ( so h1 == so matrix {{
	       a^4+4*a^3*b+6*a^2*b^2-3*b^4+4*a^3*c+12*a^2*b*c+12*a*b^2*c+6*a^2*c^2
	       +12*a*b*c^2+6*b^2*c^2+4*a*c^3+4*b*c^3+c^4,
	       a*b^3+b^4+b^3*c
	       }} )
"
TEST "
R = ZZ/101[a..d,t]
f = a^2-d^3*b-1
assert(homogenize(f,t) == a^2*t^2 - d^3*b - t^4)
assert(homogenize(f,t,{1,2,3,4,1}) == a^2*t^12-t^14-b*d^3)
assert(homogenize(f,b,{1,1,0,-1,1}) == a^2 - d^3*b^5 - b^2)

m = map(R^{1,-1}, , {{a,b},{c,d-1}})
assert(homogenize(m,t) == map(R^{1,-1}, , {{a*t^2, b*t^2}, {c, d-t}}))
assert(homogenize(m,t,{-1,-1,-1,-1,1}) - map(R^{1,-1}, , {{a*t^2, b*t^3}, {c, d*t-1}}) == 0)

v = m_0
F = module v
assert(homogenize(v,t) == a*t^2 * F_0 + c * F_1)
assert(homogenize(v,t,{-1,-1,-1,-1,1}) == a*t^2 * F_0 + c * F_1)

-- now check to make sure that all is ok over quotient rings
R = ZZ/101[a..d]/(a^2-b^2, a*b)
use R
f = c^2 - 1 + b^2 - b
assert(homogenize(f,a) == c^2)
"

TEST "
R = ZZ/5[t]/(t^2+t+1)
assert (not isPrimitive t)
assert isPrimitive (t-1)
assert (not isPrimitive 0_R)
"

TEST ///
assert ( class (x->x) === FunctionClosure )
assert ( class sin === CompiledFunction )
assert ( class depth === MethodFunction )
///

TEST ///
    R := ZZ/101[a..f];
    -- plane quintic, genus=6
    I1 := monomialCurveIdeal(R,{3,5});
    assert(I1 == image matrix{{b^5-a^2*c^3}});

    -- one singular point, g=2
    I2 := monomialCurveIdeal(R,{3,4,5});
    assert(I2 == image matrix {{c^2-b*d, b^2*c-a*d^2, b^3-a*c*d}});

    -- two singular points, g=7
    I3 := monomialCurveIdeal(R,{6,7,8,9,11});
    assert(I3 == image matrix {{
               d*e-b*f, e^2-c*f, c*d-b*e, d^2-c*e,
               c^2-b*d, b*c*e-a*f^2, b^2*d-a*e*f, b^2*c-a*d*f, b^3-a*c*f}});

    -- smooth rational quartic in P^3
    I4 := monomialCurveIdeal(R,{1,3,4});
    assert(I4 == image matrix {{b*c-a*d, c^3-b*d^2, a*c^2-b^2*d, b^3-a^2*c}});
///

-- used to be in coefficients-doc.m2
TEST ///
     R = QQ[a,b,c,d,e,f][x,y];
     F = a*x^2+b*x*y+c*y^2
     (M,C) = coefficients F
     assert(
	  last coefficients(F, Monomials=>M)
	  == C)
     coefficients(F, Variables=>{x,y,R_2})
///

TEST ///
     R = QQ[a,b,c,d,e,f][x,y];
     F = a*x^2+b*x*y+c*y^2
     G = d*x^2+e*x*y+f*y^2
     FG = matrix"F;G"
     mons1 = monomials FG
     (M,C) = coefficients FG -- error at the moment BUG
     mons = matrix"0,0,0,x2,xy,y2;x2,xy,y2,0,0,0"
     assert(M == mons)
     assert(
	  last coefficients(FG, Monomials=>mons1)
	  == C)
     coefficients(F, Variables=>{x,y,R_2})
///

TEST ///
  -- Using this for basis(d, f), where f is a map of modules
  R = ZZ/101[a..d]
  M = matrix"a,b;c,d"
  -- let's compute basis(1,M):
  G = source M
  F = target M
  monsF = basis(1,F)
  monsG = basis(1,G)
  last coefficients(M * monsG, Monomials=>monsF) -- this is it!

  -- Another example
  R = ZZ/101[a..e]
  I = ideal"ab,bc,cd,de,ea"
  S = reesAlgebra I
  T = ambient S
  L = ideal S
  describe S
  C = res L
  f = C.dd_2
  f1 = basis(1,target f)
  f2 = basis(1,source f)
  last coefficients(f * f2, Monomials=>f1)
  f * f2
  f
  f2

  f = C.dd_1
  f1 = basis(2,target f)
  f2 = basis(2,source f)
  last coefficients(f * f2, Monomials=>f1)
  f * f2
///
