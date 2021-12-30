-- these tests were in random places in Macaulay2Doc
-- TODO: move them somewhere appropriate
--
     k = ZZ/101
     R = k[a,b,c,d]/(a^4+b^4+c^4+d^4)
     X = Proj R
     result = table(3,3,(p,q) -> timing ((p,q) => rank HH^q(cotangentSheaf(p,X))))
     assert( {{1, 0, 1}, {0, 20, 0}, {1, 0, 1}} === applyTable(result,last@@last) )
     print new MatrixExpression from result


-- Example 4.1: the bounds can be sharp.
--
     S = QQ[w,x,y,z];
     X = Proj S;
     I = monomialCurveIdeal(S,{1,3,4})
     N = S^1/I;
     assert(Ext^1(OO_X,N~(>= 0)) == prune truncate(0,Ext^1(truncate(2,S^1),N)))
     assert(Ext^1(OO_X,N~(>= 0)) != prune truncate(0,Ext^1(truncate(1,S^1),N)))


-- Example 4.2: locally free sheaves and global Ext.
--
     S = ZZ/32003[u,v,w,x,y,z];
     I = minors(2,genericSymmetricMatrix(S,u,3));
     X = variety I;
     R = ring X;
     Omega = cotangentSheaf X;
     OmegaDual = dual Omega;
     assert(Ext^1(OmegaDual, OO_X^1(>= 0)) == Ext^1(OO_X^1, Omega(>= 0)))


-- Example 4.3: Serre-Grothendieck duality.
--
     S = QQ[v,w,x,y,z];
     X = variety ideal(w*x+y*z,w*y+x*z);
     R = ring X;
     omega = OO_X^{-1};
     G = sheaf cokernel genericSymmetricMatrix(R,R_0,2);
     assert(Ext^2(G,omega) == dual HH^0(G))
     assert(Ext^1(G,omega) == dual HH^1(G))
     assert(Ext^0(G,omega) == dual HH^2(G))

--
fib = memoize( n -> if n <= 1 then 1 else fib(n-1) + fib(n-2) )
assert ( fib 10 == 89 )


--
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



--
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
assert ( toString x === "set {1, 2, 3}" )


--
clearAll
frac(QQ[a,b])
assert ( a == denominator(b/a) )
assert ( b == numerator(b/a) )
assert ( 1 == numerator(b/b) )


--
     assert( net (2/1) === "2" )
     assert( net (1/1) === "1" )


--
-- test name
R = ZZ/101[a..e]
f = symmetricPower(2,vars R)
assert( f == value toExternalString f )
assert( f == value toString f )


--
R = ZZ/101[a,b,c]
M = cokernel matrix {{a,b^2,c^3}}
N = image M_{0}
assert( M == N )


--
assert isDirectSum (QQ^1 ++ QQ^2)
assert isDirectSum (QQ^1 ++ QQ^2)


--
clearAll
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



--
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


--
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
A = R/I
jacobian A
singA = minors(codim ideal presentation A, jacobian A)
generators gb singA


--
R=ZZ/101[a..d]
f = matrix {{a}}
assert( isHomogeneous f )
g = reshape(R^1, R^{-1}, f)
assert isHomogeneous g


--
ZZ[t]
assert (matrix {{t}} ** matrix {{t}} == matrix{{t^2}})


--
R = ZZ/101[x,y,z]
I = ideal(x,y)
assert( 1 == dim I )
assert( 2 == codim I )


--
     R=ZZ/101[x,y,z]
     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x + z) } === 0 )
     assert( dim singularLocus ideal {y^2*z - x*(x - z)*(x - z) } === 1 )
     S = ZZ/103[a..d]
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + b^2 + 3*c^2 + 2*d^2 } === 1 )
     assert( dim singularLocus ideal { a^2 + b^2 + c^2 + d^2, a^2 + 5*b^2 + 3*c^2 + 2*d^2 } === 0 )

--
R = ZZ/101[a]
assert isInjective R^2_{0}
assert not isInjective R^2_{0,0}
assert isSurjective R^2_{0,0,1}
assert not isSurjective R^2_{1}


--
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover exteriorPower(2,M) == exteriorPower(2,cover M) ))

--
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

--
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    table(modules, modules, (P,Q) -> assert(cover P ** cover Q == cover (P ** Q)));

--
scan(3, n -> scan(-3 .. 3, d -> (
	       h := projectiveHilbertPolynomial(n,d);
	       scan(3, i -> assert( h i === binomial(n+d+i,n) )))))

--
r = ZZ/101[a,b]
assert ( 2 * degree (a * b^2) === {6} )
M = cokernel matrix (r,{{1}})
assert ( isFreeModule prune M )

--
GF(8,Variable => x)
assert ( det matrix{{x,1},{x^2,x^3}} == x^4 - x^2 )

--
clearAll
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

--
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


     --
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

     --
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

     --
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


--
clearAll
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


--
R = ZZ/103[a..c]
C = resolution cokernel vars R
assert(regularity C === 0)
R = ZZ/101[a .. r]
M = cokernel genericMatrix(R,a,3,6)
time C = resolution M
assert(regularity C === 2)
f = symmetricPower(2,vars R)
assert(f%a + a * (f//a) == f)


--
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


--
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


--
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

--
clearAll
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


--
R = ZZ/5[t]/(t^2+t+1)
assert (not isPrimitive t)
assert isPrimitive (t-1)
assert (not isPrimitive 0_R)


--
assert ( class (x->x) === FunctionClosure )
assert ( class sin === CompiledFunction )
assert ( class depth === MethodFunction )


--
clearAll
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


-- used to be in coefficients-doc.m2
--
     R = QQ[a,b,c,d,e,f][x,y];
     F = a*x^2+b*x*y+c*y^2
     (M,C) = coefficients F
     assert(
	  last coefficients(F, Monomials=>M)
	  == C)
     coefficients(F, Variables=>{x,y,R_2})


--
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


--
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



--
     R = QQ[x,y,z]
     C = res coker vars R
     D = C ++ C
     E = ker D_[0]
     E = coker D_[0]
     E = image D_[0]
     E = coimage D_[0]


--
R = ZZ/101[x,y]
M = cokernel matrix {{x^2+y^4, y^2 + x*y^3 + 11, 1 + x*y^2}}
C = res M
assert (HH_-1 C == 0)
assert (HH_0 C == M)
assert (HH_1 C == 0)
assert (HH_2 C == 0)
assert (HH_3 C == 0)
assert (HH_4 C == 0)

--
     assert( (     M = ZZ^2 ++ ZZ^3) === ZZ^5 );
     assert( (     formation M) === new FunctionApplication from {directSum,(ZZ^2,ZZ^3)} );
     assert( (     M = directSum(ZZ^2, ZZ^3, ZZ^4)) === ZZ^9 );
     assert( (     formation M) === new FunctionApplication from {directSum,(ZZ^2,ZZ^3,ZZ^4)} );
     assert( (     M = ZZ^2 ** ZZ^3) === ZZ^6 );
     assert( (     formation M) === new FunctionApplication from {tensor,(ZZ^2,ZZ^3)} );


--
stream = (action,state) -> () -> stream(action, action state)
fib = stream( (i,j) -> (j,i+j), (0,1))
scan(1 .. 22, i -> fib = fib())


--
assert( partitions 4 === {{4},{3,1},{2,2},{2,1,1},{1,1,1,1}} / (x -> new Partition from x) )
assert( partitions(5,3) === {{3,2},{3,1,1},{2,2,1},{2,1,1,1},{1,1,1,1,1}} / (x -> new Partition from x) )


--
     R=ZZ/101[a..d]
     C=resolution cokernel vars R
     D = C ++ C[1] ++ C[2]
     betti D
     assert( degree HH_1 D === 0 )
     assert( degree HH_0 D === 1 )
     assert( degree HH_-1 D === 1 )
     assert( degree HH_-2 D === 1 )


--
assert( toString tally {1,1,1,2,1,3,2} === "new Tally from {1 => 4, 2 => 2, 3 => 1}" )
assert( tally {1,1,1,2,1,3,2} === new Tally from {(1,4),(2,2),(3,1)} )


--
R = ZZ/101[a..c]
I = image vars R
J = image symmetricPower (2,vars R)
g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))
E = cone g


--
R=ZZ[a]
assert( toString a === "a" )
assert( toString a^2 === "a^2" )


--
    -- test of lift/promote of an ideal
    A = ZZ/101[a..d]
    A = QQ[a..d]
    A = GF(5,2)[a..d]
    B = A/(a^2-d^2)
    use A
    I = ideal(a,b)
    assert(ring I === A)
    I1 = I*B
    I2 = lift(I1,A)
    assert(trim I2 == ideal(a,b,d^2))
    use B
    C = B/(b^3-c^3)
    I3 = I2*C
    I3a = I*C
    assert(I3 == I3a)
    I4 = lift(I3,B)
    I5 = trim lift(I3,A)
    use A
    assert(I5 == ideal(a,b,c^3,d^2))


--
     z = 2 - 3*ii
     w = 4 + 5*ii
     x = 2 + ii - ii

     eps = 10.^-10
     small = (x) -> abs x < eps
     near = (w,z) -> small realPart(w-z) and small imaginaryPart(w-z)

     assert( z*w == 23  - 2*ii )
     assert( near(z/w , -7/41 + -22/41 * ii ) )
     assert( 1 + z == 3 - 3*ii )
     assert( 2*w == 8 + 10*ii )
     assert( z + w == 6 + 2*ii )
     assert( toString w == "4+5*ii" )
     assert( conjugate z == 2 + 3*ii )
     assert( x == 2 )
     assert( x == 2. )
     assert( x == 2/1 )
     assert( net ( 2 - 3 * ii ) === "2-3*ii" )



--
assert (
     (0,0)..(2,3) ==
     ((0,0),(0,1),(0,2),(0,3),(1,0),(1,1),(1,2),(1,3),(2,0),(2,1),(2,2),(2,3))
     )


--
R=ZZ/101[a,b]
f=matrix(R,{{1,a},{0,1}})
g=matrix(R,{{1,0},{b,1}})
h=f*g*f*g
assert( h^3 * h^-1 == h^2 * h^0 )
assert( h * h^-1 == 1 )


--
R=ZZ/101[a,b]
f = matrix {{a}}
assert( source f != target f)
assert( target f == target f^2 )
assert( source f == source f^2 )
assert( target f == target f^0 )
assert( source f != source f^0 )


--
R = ZZ/101[a..d]
F = R^3
H = subquotient(F_{1,2}, F_{2})
f = map(H,cover H,id_(cover H))
assert( cokernel f == 0 )
assert( kernel f == image R^2_{1} )
assert( isWellDefined f )
assert( isSurjective f )
assert( not isInjective f )



--
clearAll
R = ZZ/101[x,y,z]
assert isHomogeneous map(R^2,2,(i,j)->R_j)
assert isHomogeneous map(R^2,5,{{x,y,z,x^2,y^2},{x,0,z,z^2,0}})


--
R = ZZ/101[a..d]
f=1+a+b+c+d
assert(size f == 5)
S = R/a
assert(size promote(f,S) == 4)


--
clearAll
R = ZZ/101
exteriorPower(3,R^5)
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
M = Ext^2(coker generators I, R)
prune exteriorPower(3,M)
exteriorPower(0,R^3)
exteriorPower(0,M)
prune exteriorPower(1,M)
exteriorPower(2,M)
exteriorPower(-1,M)
exteriorPower(-2,M)

M = subquotient(matrix{{a,b,c}}, matrix{{a^2,b^2,c^2,d^2}})
N = subquotient(matrix{{a^2,b^2,c^2}}, matrix{{a^3,b^3,c^3,d^3}})
m = map(N,M,matrix(R,{{1,0,0},{0,1,0},{0,0,1}}))
source m
target m
trim ker m
M1 = coker presentation M
N1 = coker presentation N
m1 = map(N1,M1,matrix m)
M2 = trim exteriorPower(2,M)
N2 = trim exteriorPower(2,N)


--
R = ZZ/101[a .. i]
m = genericMatrix(R,a,3,3)
assert( exteriorPower(1,m) == m )
assert( minors(1,m) == image vars R )
assert( exteriorPower(2,m*m) == exteriorPower(2,m)*exteriorPower(2,m) )
assert(
     exteriorPower(2,m)
     ==
     matrix {
	  {-b*d+a*e, -b*g+a*h, -e*g+d*h},
	  {-c*d+a*f, -c*g+a*i, -f*g+d*i},
	  {-c*e+b*f, -c*h+b*i, -f*h+e*i}} )
assert( exteriorPower(3,m) == matrix {{-c*e*g+b*f*g+c*d*h-a*f*h-b*d*i+a*e*i}} )


--
k = ZZ/101
f = random(k^3,k^9)
R = k[a,b,c]
g = random(R^4,R^{-2,-2})


--
clearAll
R = ZZ/101[a..f]
assert( dim image matrix {{a,b}} == 6 )
assert( dim coker matrix {{a,b}} == 4 )
assert( dim coker matrix {{a-1,b-c^2}} == 4 )
assert( dim ideal (a,b) == 4 )
assert( codim ideal (a,b) == 2 )
assert( dim R == 6 )
assert( dim (R/a) == 5 )


--
	R = ZZ/101[a..d]
	f = matrix{{a,b},{c,d}}
	g = matrix(R,{{1},{0}})
	M = subquotient(g,f)
	assert( numgens source basis(3,M) == 16 )


--
L = ZZ/5[t]
M = L/(t^2+t+1)
G = GF(M,Variable => v,PrimitiveElement => t-1)
assert( lift(v,M) + 1 == lift (v+1,M) )
assert( lift(v^6,M) == (lift(v,M))^6 )
assert( lift(v^7,M) == (lift(v,M))^7 )


--
R=ZZ/101[a,b,c]
f = map(R,R,matrix(ZZ/101,{{1,2,3},{4,5,6},{7,8,9}}))
assert( f(a) == a + 4*b + 7*c )
assert( kernel f == ideal ( a-2*b+c ) )


--
clearAll
f = map(frac (QQ[r,s,t]), QQ[x,y,z], {(r-s)/t,(s-t)/r,(t-r)/s})
assert( kernel( f, SubringLimit => 1 ) == ideal(x*y*z+x+y+z) )


--
S = ZZ/101[x,y]
R = ZZ/101[t,u]
f = map(S,R,{x,0})
assert( kernel f == ideal u )


--
S = ZZ/101[x,y]
R = ZZ/101[t,u]
f = map(S,R,{x,1})
assert( kernel f == ideal (u-1) )


--
clearAll
R = ZZ/101[a..f]
m = matrix {{a*b*c*d, b^2*c*d, b*c^2*d, b*c*d^2, b*c*d*e,
	     c*d*e*f, a*d*e*f, a*b*e*f, a*b*c*f, b*c*d*f}}
f = map(R,ZZ/101[x_0..x_9],m)
J = kernel f


--
S = ZZ/101[a..j]
m = matrix {{d*g*i-a*g*j, c*h^2-e*h*i, a*b^2*g-a*b*d*h, b*d*f-d*e*j}}
E = Ext^3(cokernel m, S)
annihilator E


--
    R = ZZ/101[s,t]
    J = image matrix {{s^4, s^3*t, s*t^3, t^4}}
    S = symmetricAlgebra J  -- MES: make an assertion here...


--
R = ZZ/101[a,b,c,d]
f = matrix {{c^3-b*d^2, b*c-a*d, b^3-a^2*c, a*c^2-b^2*d}}
M = cokernel f
assert( codim M === 2 )
assert( dim M === 2 )
E = Ext^2(M, R^1)
T = (degreesRing R)_0
p = poincare E
assert ( p == 3*T^(-3)-5*T^(-2)+1*T^(-1)+1 )
assert( dim E === 2 )
assert( dim Ext^1(M,R^1) === -1 )
-- assert ( poincare prune Ext^2(M,M) == (4T^-3 + 2T^-2 - 5T^-1 + 3) (1 - T)^2 )

F = Ext^3(M, R^1)
assert( dim F === 0 )
assert( degree F === 1 )

assert( Ext^4(M,R^1) == 0 )

k = cokernel vars R
N = cokernel matrix {{1_R}}
assert( dim Ext^2(N,k) === -1 )

g = vars R
P = (image g) / (image matrix {{a^2, b^2, c^2, d^2}})

assert( degree Hom(P,k) === 4 )
assert( degree P === 15 )
assert( dim P === 0 )
assert( pdim P === 4 )

assert( degree Ext^4(P,P) === 15 )

image g / image(g**g**g)


--
clearAll
eg1 = () -> (
  R = ZZ/101[a..d];
  m = matrix {{a*d - b*c, a^2*c - b^3, c^3 - b*d^2, a*c^2 - b^2*d}};
  C = resolution cokernel m;
  E2 = Ext^2(cokernel m, R)
  )
eg1()

eg2 = () -> (
  -- gbTrace = 3;
  R = ZZ/101[a..f];
  m = matrix {{a*b*c - d*e*f, a*b*d - c*e*f, a*e*f - b*c*d}};
  C = resolution cokernel m;
  -- topComponents m
  )
eg2()

eg3 = () -> (
  -- test newCoordinateSystem
  R = ZZ/101[a..f];
  m = matrix {{a-b+c, a-d-f}};
  newCoordinateSystem(R, m))
--eg3()


--
     Q = ZZ/101[x,y]
     I = ideal(x^3,y^5)
     R = Q/I
     N = cokernel random (R^3, R^{2:-2})
     M = cokernel random (R^3, R^{2:-2})
     E = Ext(N,M)
     scan(4, d -> (
	  bd := basis Ext^d(N,M);
	  assert(
	       tally splice apply(-10..10,i -> rank source basis({-d,i},E) : {i}) ===
	       tally apply(rank source bd, i -> degree bd_i))))


--
-- copyright 1995 Michael E. Stillman
-- several tests of tensor products and Tor
-- many of these examples were created by David Eisenbud

-- Test 1.  Checking that Tor_i(M,k) and Tor_i(k,M) both give
-- the graded betti numbers of M.

R = ZZ/101[a..d]
k = cokernel vars R
M = cokernel matrix {{a*d - b*c, a^2*c - b^3, c^3 - b*d^2, a*c^2 - b^2*d}}

T0 = Tor_0(M,k)
S0 = Tor_0(k,M)
T1 = Tor_1(M,k)
S1 = Tor_1(k,M)
T2 = Tor_2(M,k)
S2 = Tor_2(k,M)
T3 = Tor_3(M,k)
S3 = Tor_3(k,M)
T4 = Tor_4(M,k)
S4 = Tor_4(k,M)

T = (degreesRing R)_0

assert(poincare T0 ==             (1-T)^4)
assert(poincare T1 == (T^2+3*T^3)*(1-T)^4)
assert(poincare T2 == 4*T^4*      (1-T)^4)
assert(poincare T3 ==  T^5 *      (1-T)^4)
assert(poincare T4 ==  0)

assert(poincare T0 == poincare S0)
assert(poincare T1 == poincare S1)
assert(poincare T2 == poincare S2)
assert(poincare T3 == poincare S3)
assert(poincare T4 == poincare S4)

-- notice that degree Tor_i(M,k) gives the i th betti number of M,
-- as does Tor_i(k,M), and the graded betti numbers can be seen by using
-- 'see target prune Tor_i(k,M)'
-- or by using, for example

hf = poincare T2;
if hf != 0 then while substitute(hf,{T=>1}) == 0 do hf = hf // (1-T);
hf

-- Test 2.  Intersection multiplicity according to Serre
-- The intersection of two planes in 4-space meeting another such.
-- The multiplicity should be 4.  Serre's technique says that this
-- number should be the alternating sum of the 'degree Tor_i(R/I,R/J)':

R = ZZ/101[a..d]
I = generators intersect(image matrix {{a,b}}, image matrix {{c,d}});
J = generators intersect(image matrix {{a-c-b, b-d}}, image matrix {{a-d, b-c}});

U0 = Tor_0(cokernel I, cokernel J);
U1 = Tor_1(cokernel I, cokernel J);
U2 = Tor_2(cokernel I, cokernel J);
U3 = Tor_3(cokernel I, cokernel J);
U4 = Tor_4(cokernel I, cokernel J)

U0 = prune U0
assert( numgens target presentation U0 == 1 )
assert( numgens source presentation U0 == 8 )

U1 = prune U1
assert( numgens target presentation U1 == 4 )
assert( numgens source presentation U1 == 16 )

U2 = prune U2
assert( numgens target presentation U2 == 1 )
assert( numgens source presentation U2 == 4 )

U3 = prune U3
assert( numgens target presentation U3 == 0 )
assert( numgens source presentation U3 == 0 )

U4 = prune U4
assert( numgens target presentation U4 == 0 )
assert( numgens source presentation U4 == 0 )

assert( degree U0 == 7 )
assert( degree U1 == 4 )
assert( degree U2 == 1 )
assert( degree U3 == 0 )
assert( degree U4 == 0 )


--
R=ZZ/101[x]
assert(monomialIdeal vars R != 0)
assert(monomialIdeal map(R^1,R^1,0) == 0)


--
R = ZZ/101[a .. d,Degrees=>{1,2,3,5}]
f = vars R
C = resolution cokernel f
assert(regularity C === 7)
M = kernel f
assert( numgens source M.generators === 6 )
assert( kernel presentation kernel f === kernel presentation kernel f )

g = map(cokernel f, target f, id_(target f))
N = kernel g
assert( numgens source N.generators === 4 )
assert( kernel g == image f )
W = kernel f ++ cokernel f
P = poincare W
assert( P == poincare kernel f + poincare cokernel f )
assert( P == poincare prune W )


--
     -- here we test the commutativity of the pentagon of associativities!
     C = QQ^1[0] ++ QQ^1[-1]
     assert(
	  (tensorAssociativity(C,C,C) ** C) * tensorAssociativity(C,C**C,C) * (C ** tensorAssociativity(C,C,C))
	  ==
	  tensorAssociativity(C**C,C,C) * tensorAssociativity(C,C,C**C)
	  )


--
R = QQ[a..d, MonomialOrder => GRevLex]
a*c + b^2 + a*c^3
R = QQ[a..d, MonomialOrder => {GRevLex=>2, GRevLex=>2}]
a*c + b^2 + a*c^3
R = QQ[a..d, MonomialOrder => {2,2}]
a*c + b^2 + a*c^3
R = QQ[a..d, MonomialOrder => RevLex => 4, Global => false]
1 + a*c + b^2 + a*c^3
a+a^2



--
str = "HTTP/1.1 200 OK\r
Date: Thu, 23 Jun 2016 13:10:59 GMT\r
Server: Apache/2.2\r
Vary: Accept-Encoding\r
Connection: close\r
Transfer-Encoding: chunked\r
Content-Type: text/html; charset=UTF-8\r
\r
2b\r
<head><title>SEARCH RESULTS</title></head>
\r
b\r
<body><pre>\r
17\r
<b>Search command:</b>
\r
1e\r
class.x -di x -He EH10:MVNFL5
\r
10\r

<b>Result:</b>
\r
436\r
4 9  M:22 9 N:14 8 H:10,18 [-16]
   1   0   1   0   2   0  -2  -2  -2
   0   1   0   0  -1   1   1  -1   1
   0   0   2   0   1   1  -3  -1  -4
   0   0   0   1   1   1  -1  -1  -2
4 10  M:23 10 N:15 10 H:10,18 [-16]
    1    0    0    0   -1    1   -2    2    0   -1
    0    1    0    0    1   -1    2   -1   -2    0
    0    0    1    0   -1    1   -1    0    2   -2
    0    0    0    1    1   -1    0   -2   -1    2
4 9  M:24 9 N:14 8 H:10,20 [-20]
   1   0   1   0   1  -1  -2   1  -2
   0   1   0   0   0   2  -2  -1   2
   0   0   2   0  -1  -1   0  -2  -2
   0   0   0   1  -1  -1   1  -1  -1
4 11  M:25 11 N:15 10 H:10,20 [-20]
   1   0   0   0   2  -2   0   2  -2  -2   2
   0   1   0   0  -1   1   1  -1   0   1  -2
   0   0   1   0  -1   1  -1   0   2   0  -2
   0   0   0   1  -1   1   1  -2   1   0  -1
4 10  M:25 10 N:15 10 H:10,20 [-20]
    1    0    0    0   -1    0   -1   -1    2    1
    0    1    0    0    0    0    2    0   -1   -2
    0    0    1    0    0   -2    2    2   -2   -2
    0    0    0    1    0   -1    0    2    0   -2
Exceeded limit of 5
\r
e\r
</pre></body>
\r
0\r
\r
"
(head,body) = splitWWW str;

assert(head === "HTTP/1.1 200 OK\r
Date: Thu, 23 Jun 2016 13:10:59 GMT\r
Server: Apache/2.2\r
Vary: Accept-Encoding\r
Connection: close\r
Transfer-Encoding: chunked\r
Content-Type: text/html; charset=UTF-8")

assert(body === "<head><title>SEARCH RESULTS</title></head>
<body><pre><b>Search command:</b>
class.x -di x -He EH10:MVNFL5

<b>Result:</b>
4 9  M:22 9 N:14 8 H:10,18 [-16]
   1   0   1   0   2   0  -2  -2  -2
   0   1   0   0  -1   1   1  -1   1
   0   0   2   0   1   1  -3  -1  -4
   0   0   0   1   1   1  -1  -1  -2
4 10  M:23 10 N:15 10 H:10,18 [-16]
    1    0    0    0   -1    1   -2    2    0   -1
    0    1    0    0    1   -1    2   -1   -2    0
    0    0    1    0   -1    1   -1    0    2   -2
    0    0    0    1    1   -1    0   -2   -1    2
4 9  M:24 9 N:14 8 H:10,20 [-20]
   1   0   1   0   1  -1  -2   1  -2
   0   1   0   0   0   2  -2  -1   2
   0   0   2   0  -1  -1   0  -2  -2
   0   0   0   1  -1  -1   1  -1  -1
4 11  M:25 11 N:15 10 H:10,20 [-20]
   1   0   0   0   2  -2   0   2  -2  -2   2
   0   1   0   0  -1   1   1  -1   0   1  -2
   0   0   1   0  -1   1  -1   0   2   0  -2
   0   0   0   1  -1   1   1  -2   1   0  -1
4 10  M:25 10 N:15 10 H:10,20 [-20]
    1    0    0    0   -1    0   -1   -1    2    1
    0    1    0    0    0    0    2    0   -1   -2
    0    0    1    0    0   -2    2    2   -2   -2
    0    0    0    1    0   -1    0    2    0   -2
Exceeded limit of 5
</pre></body>
")


--
A = ZZ[a..d]
B = A[r,s,t]
C = B[x,y,z]



--
     assert( accumulate(toList,a,{b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c},d,toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )
     assert( accumulate(toList,{a,b,c,d}) == {{a, b}, {{a, b}, c}, {{{a, b}, c}, d}} )
     assert( accumulate({a,b,c,d},toList) == {{a, {b, {c, d}}}, {b, {c, d}}, {c, d}} )


--
  R = ZZ/101[a..d]
  F = a + d^2 + a*b*c
  assert(F == antipode F)
  assert((1_R) == antipode(1_R))
  assert((0_R) == antipode(0_R))
  assert((R_0) == antipode(R_0))


--
clearAll
  R = ZZ/101[a..f, SkewCommutative=>true]
  F = a + b*d + a*b*c + b*c*d*e + a*b*c*e*f + a*b*c*d*e*f
  assert(a - b*d - a*b*c + b*c*d*e + a*b*c*e*f - a*b*c*d*e*f == antipode F)
  assert((1_R) == antipode(1_R))
  assert((0_R) == antipode(0_R))
  assert((R_0) == antipode(R_0))

  kk = coefficientRing R
  assert(1_kk == antipode (1_kk))
  assert(17_kk == antipode (17_kk))

  M = matrix{{a*b-1, a*c-d, e-a*b*c}}
  N = syz M
  assert(M * N == 0)
  assert((transpose N) * (transpose M) == 0) -- fails without antipode!


--
  R = ZZ/101[a..f, SkewCommutative=>{0,1,3,5}]
  F = a - b*d - a*b*c - b*c*d*e - a*b*c*e*f + a*b*c*d*e*f
  antipode F

  M = matrix{{a*b-1, a*c-d, e-a*b*c}}
  N = syz M
  assert(M * N == 0)
  assert((transpose N) * (transpose M) == 0) -- fails without antipode!

  -- Just taking transpose gives wrong values!
  Mt = matrix transpose entries M
  Nt = matrix transpose entries N
  assert(Nt*Mt != 0)


--
R = RR_100[x,y]
F = x+.01*y^2
G = clean(.001,F^3) - (.03p200*x^2*y^2+x^3)
assert(clean(.001,G) == 0)
leadCoefficient F
norm F
norm(infinity,F)
size F

printingPrecision = 6
M = random(RR_200^10,RR_200^10)
.5 * clean(.5,M)
clean(.995,M)
norm M

R = CC[x]
M = random(R^10,R^10)
norm M
apply(flatten entries M, leadCoefficient)

-- from git issue #56
A = mutableMatrix({{1_RR}}, Dense=>true)
clean(0.1,A) -- works fine
A = mutableMatrix({{1_RR}}, Dense=>false)
assert try (clean(0.1,A);false) else true  -- not yet implemented.



--
needsPackage "SimplicialComplexes"
R = QQ[a..d]
D = simplicialComplex {a*b*c,a*b*d,a*c*d,b*c*d}
C = chainComplex D
assert ( rank HH_2 C == 1 )


--
    R = QQ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover cokernel M_{1} ==  cover M ) )


--
  R = ZZ[x]
  M = image map(R^2,,{{2},{0}})
  f = coverMap M
  assert isSurjective f
  assert ( cokernel f == 0 )
  -- now check it over ZZ, too!
  M = image matrix {{2},{0}}
  f = coverMap M
  assert isSurjective f
  assert ( cokernel f == 0 )


--
R = QQ[a..d]
f = a^3*b+c^4+d^2-d
assert((0,4) == weightRange({1,1,0,0},f))
S = R[x,y]
f = a*x+b*y
assert((1,2) == weightRange({1,2,0,0,0,0},f))
assert((1,2) == weightRange({1,2},f))
assert((1,2) == weightRange({1,2,0,0,0,0,231,12312,132,3212,2,123,12123,23},f))
(34489274,534535353) == weightRange({34489274,534535353},f)
weightRange({0,0,3,7,1},f)


--
  R = ZZ[x,y,z]
  f = y^4*(3*z^3-z^2-1) - y^3*z^7 + y + z^12
  assert(topCoefficients f == (y^4, 3*z^3-z^2-1))
  assert(topCoefficients matrix{{f}} == (matrix{{y^4}}, matrix{{3*z^3-z^2-1}}))
  assert(topCoefficients matrix{{f, x*y-1}} == (matrix{{y^4, x}}, matrix{{3*z^3-z^2-1, y}}))
  assert(topCoefficients matrix{{x*y^4}, {x*z}} == (matrix{{x}}, matrix{{y^4},{z}}))
  assert(topCoefficients 3_R == (1_R, 3_R))
  assert(topCoefficients 0_R == (1_R, 0_R))
  assert(topCoefficients matrix{{1_R}} == (matrix{{1_R}}, matrix{{1_R}}))
  assert(topCoefficients(x*y-1) == (x,y))
  assert(topCoefficients(x+3*x^2+5*x^3+7*x^4+19) == (x^4, 7_R))


--
  A = ZZ[a,b]
  B = A[c,d,e]
  f = a*c^2-b*c
  assert(topCoefficients f == (c^2, a))
  g = sub(a^2*b+a*b, B)
  assert(topCoefficients g == (a^2, b)) -- this is perhaps not completely expected behavior


--
  A = ZZ/32003[a,b,c,d]/(a^2-b-1)
  f = a*(b+c+d^2) - 3
  topCoefficients f == (0,0) -- no, it should really treat it as a polynomial...



-- --Errors in the above code
kk = ZZ/101
A = kk[a,b]
B = kk[c,d,e]
-- this test doesn't test much.  Why?
describe (C = tensor(A,B,MonomialOrder=>Eliminate numgens A))
describe (C = tensor(A,B,MonomialOrder=>GRevLex))
describe tensor(A,B,Degrees=>{5:1}) -- BUG
describe tensor(A,B,WeylAlgebra=>{a=>c}) -- ignores it
describe tensor(A,B,DegreeRank=>3) -- weird behavior BUG
describe(C = tensor(A,B,Inverses=>true,MonomialOrder=>RevLex)) -- allowed, but not appropriate here

describe tensor(A,B,Weights=>{1,2,3,4,5}) -- ignored?
describe tensor(A,B,Global=>false) -- ??
describe(C = tensor(A,B,SkewCommutative=>true)) -- ignored


--
     assert( subsets(4,2) === {{0,1},{0,2},{1,2},{0,3},{1,3},{2,3}} )
     assert( subsets({a,b,c,d},2) === {{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}} )
     assert(
      set subsets(set {a,b,c,d},2) ===
      set apply({{a,b},{a,c},{b,c},{a,d},{b,d},{c,d}},set) )


--
R = ZZ[a..d]
f = (a+b+c)^3
assert(someTerms(f,0,1) == leadTerm f)
assert(someTerms(f,-1,1) == c^3)
assert(someTerms(f,-2,2) == 3*b*c^2 + c^3)


--
R = QQ[a..d]
S = R/(a^2-b^2)
T = S[x,y,z]
promote(1/2,S)
1/2 * 1_S
I = ideal(a^3,c^3)
-- (I_0) ** T -- doesn't make sense [dan]
(gens I) ** T


R = QQ[a..d]
f = a^2
S = R/(a^2-b-1)
F = map(S,R)
F (2/3)
G = map(R,S)
G (a^2)
lift(a^2,R)
promote(2/3,S)
promote(f,S)

A = QQ[a,b,c]
B = ZZ
F = map(A,ZZ)
F 3

-- should we get this to work? (MES, 8/23/06):
          kk = ZZ/32003;
	  substitute(matrix{{12/235}},kk)
	  promote(12/235,kk)
	  12_kk/235_kk
	  lift(oo,QQ)




--
    R = ZZ[x,y,z]
    modules = {
	 image matrix {{x^2,x,y}},
	 coker matrix {{x^2,y^2,0},{0,y,z}},
	 R^{-1,-2,-3},
	 image matrix {{x,y}} ++ coker matrix {{y,z}}
	 }
    scan(modules, M -> assert( cover M == target presentation M ) )


--
     assert( 3 === position({a,b,c,d,e,f},i->i===d ) )



--
    R = QQ[x,y,z];
    I = monomialIdeal(x^2,y^3,x*y^2*z,y*z^4);
    J = polarize(I);
    assert(betti res I==betti res J)


--
    R = QQ[x,y,z];
    I = monomialIdeal(x^2*y^2,y^2*z^2,x*y*z^4);
    J = polarize(I, VariableBaseName => "whyNotAWord");
    assert(betti res I==betti res J)


--
clearAll
R=ZZ/101[a..f]
m=genericSkewMatrix(R,a,4)
assert( pfaffians(-2,m) == ideal(0_R) )
assert( pfaffians(0,m) == ideal(1_R) )
assert( pfaffians(1,m) == ideal(0_R) )
assert( pfaffians(2,m) == ideal(a,b,c,d,e,f) )
assert( pfaffians(3,m) == ideal(0_R) )
assert( pfaffians(4,m) == ideal(c*d-b*e+a*f) )


--
numgens ZZ
numgens GF(9)
A = ZZ[a,b,c]
numgens A
B = A/a
numgens B
C = A[x,y]
numgens C  -- 2
use A
D = C/(x-y^2, y-b*c)
numgens D
--status: somebody should fix this! [dan]
minPres D -- error, perhaps it wants to use R.FlatMonoid instead of R.monoid here???
K = frac A
numgens K


-- tests for nextprime
--
  assert( nextPrime(-10) == 2)
  assert( nextPrime 100 == 101)
  assert( nextPrime 1000 == 1009)


--
     setRandomSeed("getPrimeOfUnity")
     (p,r)=getPrimeWithRootOfUnity(2,3,Range=>(10^3,10^4))
     assert( (p,r)==(3511,-1)) -- works if the random number generator is not unchanged
     (p,r)=getPrimeWithRootOfUnity(15,20)
     assert((p,r)==(18181,21))
     (p,r)=getPrimeWithRootOfUnity(12,2,Range=>(100,200))
     assert(r^12%p==1 and r^6%p !=1 and r^4%p != 1)


--
clearAll
     setRandomSeed 0
     p=10007,kk=ZZ/p
     R=kk[x_0..x_2]
     I=ideal(random(4,R));
     time randomKRationalPoint(I)
     R=kk[x_0..x_4]
     I=minors(3,random(R^5,R^{3:-1}));
     codim I
     time pt = randomKRationalPoint(I)
     -- The following is the answer given the above random seed.
     ans = ideal(x_3+74*x_4,x_2+2336*x_4,x_1-4536*x_4,x_0-4976*x_4)
     assert(pt == ans)

--
R = ZZ[a..d]
m = matrix{{(a*b-1)*(c*d-d^3)}}
coefficients m
oo_0 * oo_1 == m
monomials(m,Variables=>{a,b})
monomials(m,Variables=>c)
monomials(m,Variables=>1)
monomials(m,Variables=>(a,b))
monomials m

S = R[x,y,z]
m = matrix{{(a+1)*(x+y)^2}}
m_(0,0)
coefficients(m, Variables => {x,y}, Monomials => matrix {{x^2, x*y, y}})
monomials m
coefficients m



--
R = ZZ/101[a..d,MonomialOrder => Position => Up]
f = matrix{{a,b},{c,d}}
h = matrix {{1,0,0},{0,c,d}}
M = subquotient(h,f)
assert( mingens M == matrix (R, {{1},{0}}))


--
R = ZZ/101[a..d,MonomialOrder => Position => Up]
f = matrix{{a,b},{c,d}}
h = matrix {{1,0,0},{0,c,d}}
M = subquotient(h,f)
assert( generators trim M == matrix (R, {{1},{0}}))


--
     assert(min{4,5,6} === 4)
     assert(min(4,5,6) === 4)


--
     assert(max{4,5,6} === 6)
     assert(max(4,5,6) === 6)



--
clearAll
R = ZZ/32003[a..d,x_1..x_4,y_(1,1)..y_(2,2)];
F = time poly"a5+5a4b+10a3b2+10a2b3+5ab4+b5+5a4x[1]+20a3bx[1]
  +30a2b2x[1]+20ab3x[1]+5b4x[1]+10a3x[1]2+30a2bx[1]2
  +30ab2x[1]2+10b3x[1]2+10a2x[1]3+20abx[1]3+10b2x[1]3
  +5ax[1]4+5bx[1]4+x[1]5+5a4y[1,1]+20a3by[1,1]
  +30a2b2y[1,1]+20ab3y[1,1]+5b4y[1,1]+20a3x[1]y[1,1]
  +60a2bx[1]y[1,1]+60ab2x[1]y[1,1]+20b3x[1]y[1,1]
  +30a2x[1]2y[1,1]+60abx[1]2y[1,1]+30b2x[1]2y[1,1]
  +20ax[1]3y[1,1]+20bx[1]3y[1,1]+5x[1]4y[1,1]
  +10a3y[1,1]2+30a2by[1,1]2+30ab2y[1,1]2+10b3y[1,1]2
  +30a2x[1]y[1,1]2+60abx[1]y[1,1]2+30b2x[1]y[1,1]2
  +30ax[1]2y[1,1]2+30bx[1]2y[1,1]2+10x[1]3y[1,1]2
  +10a2y[1,1]3+20aby[1,1]3+10b2y[1,1]3+20ax[1]y[1,1]3
  +20bx[1]y[1,1]3+10x[1]2y[1,1]3+5ay[1,1]4+5by[1,1]4
  +5x[1]y[1,1]4+y[1,1]5+5a4y[2,2]+20a3by[2,2]+30a2b2y[2,2]
  +20ab3y[2,2]+5b4y[2,2]+20a3x[1]y[2,2]+60a2bx[1]y[2,2]
  +60ab2x[1]y[2,2]+20b3x[1]y[2,2]+30a2x[1]2y[2,2]
  +60abx[1]2y[2,2]+30b2x[1]2y[2,2]+20ax[1]3y[2,2]
  +20bx[1]3y[2,2]+5x[1]4y[2,2]+20a3y[1,1]y[2,2]
  +60a2by[1,1]y[2,2]+60ab2y[1,1]y[2,2]+20b3y[1,1]y[2,2]
  +60a2x[1]y[1,1]y[2,2]+120abx[1]y[1,1]y[2,2]
  +60b2x[1]y[1,1]y[2,2]+60ax[1]2y[1,1]y[2,2]
  +60bx[1]2y[1,1]y[2,2]+20x[1]3y[1,1]y[2,2]+30a2y[1,1]2y[2,2]
  +60aby[1,1]2y[2,2]+30b2y[1,1]2y[2,2]+60ax[1]y[1,1]2y[2,2]
  +60bx[1]y[1,1]2y[2,2]+30x[1]2y[1,1]2y[2,2]+20ay[1,1]3y[2,2]
  +20by[1,1]3y[2,2]+20x[1]y[1,1]3y[2,2]+5y[1,1]4y[2,2]
  +10a3y[2,2]2+30a2by[2,2]2+30ab2y[2,2]2+10b3y[2,2]2
  +30a2x[1]y[2,2]2+60abx[1]y[2,2]2+30b2x[1]y[2,2]2
  +30ax[1]2y[2,2]2+30bx[1]2y[2,2]2+10x[1]3y[2,2]2
  +30a2y[1,1]y[2,2]2+60aby[1,1]y[2,2]2+30b2y[1,1]y[2,2]2
  +60ax[1]y[1,1]y[2,2]2+60bx[1]y[1,1]y[2,2]2+30x[1]2y[1,1]y[2,2]2
  +30ay[1,1]2y[2,2]2+30by[1,1]2y[2,2]2+30x[1]y[1,1]2y[2,2]2
  +10y[1,1]3y[2,2]2+10a2y[2,2]3+20aby[2,2]3+10b2y[2,2]3
  +20ax[1]y[2,2]3+20bx[1]y[2,2]3+10x[1]2y[2,2]3+20ay[1,1]y[2,2]3
  +20by[1,1]y[2,2]3+20x[1]y[1,1]y[2,2]3+10y[1,1]2y[2,2]3+5ay[2,2]4
  +5by[2,2]4+5x[1]y[2,2]4+5y[1,1]y[2,2]4+y[2,2]5"
assert( F == (a+b+x_1+y_(1,1)+y_(2,2))^5 )



R = ZZ/32003[a..d,x_1..x_4,y_(1,1)..y_(2,2)];
assert( time (a+3*b+5*d-1)^7 ==
time poly "a7+21a6b+189a5b2+945a4b3+2835a3b4+5103a2b5+5103ab6+2187b7+35a6d+630a5bd+
     4725a4b2d+18900a3b3d+42525a2b4d+51030ab5d+25515b6d+525a5d2+7875a4bd2+
     47250a3b2d2+141750a2b3d2+212625ab4d2+127575b5d2+4375a4d3+52500a3bd3+
     236250a2b2d3+472500ab3d3+354375b4d3+21875a3d4+196875a2bd4+590625ab2d4+
     590625b3d4+65625a2d5+393750abd5+590625b2d5+109375ad6+328125bd6+78125d7-7a6-
     126a5b-945a4b2-3780a3b3-8505a2b4-10206ab5-5103b6-210a5d-3150a4bd-18900a3b2d
     -56700a2b3d-85050ab4d-51030b5d-2625a4d2-31500a3bd2-141750a2b2d2-283500ab3d2
     -212625b4d2-17500a3d3-157500a2bd3-472500ab2d3-472500b3d3-65625a2d4-
     393750abd4-590625b2d4-131250ad5-393750bd5-109375d6+21a5+315a4b+1890a3b2+
     5670a2b3+8505ab4+5103b5+525a4d+6300a3bd+28350a2b2d+56700ab3d+42525b4d+
     5250a3d2+47250a2bd2+141750ab2d2+141750b3d2+26250a2d3+157500abd3+236250b2d3+
     65625ad4+196875bd4+65625d5-35a4-420a3b-1890a2b2-3780ab3-2835b4-700a3d-
     6300a2bd-18900ab2d-18900b3d-5250a2d2-31500abd2-47250b2d2-17500ad3-52500bd3-
     21875d4+35a3+315a2b+945ab2+945b3+525a2d+3150abd+4725b2d+2625ad2+7875bd2+
     4375d3-21a2-126ab-189b2-210ad-630bd-525d2+7a+21b+35d-1" )


--
kk = RR
A = matrix"1,2,3;4,7,8" ** kk
(P,L,U) = LUdecomposition A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0
Q = matrix Q
norm(Q*L*U - A)

kk = RR
A = matrix"1,2;3,4;7,8" ** kk
(P,L,U) = LUdecomposition A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0
Q = matrix Q
norm(Q*L*U - A)

kk = CC
A = matrix"1,2,3;4,7,8" ** kk
(P,L,U) = LUdecomposition A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0+0.0*ii
Q = matrix Q
norm(Q*L*U - A)

kk = CC
A = matrix"1,2;3,4;7,8" ** kk
(P,L,U) = LUdecomposition A
Q = mutableMatrix(kk, #P, #P)
for i from 0 to #P-1 do Q_(P_i,i) = 1.0+0.0*ii
Q = matrix Q
norm(Q*L*U - A)

kk = ZZ/32003
A = mutableMatrix(kk,5,10, Dense=>true)
fillMatrix(A, Density=>.5)
(P,L,U) = LUdecomposition A
Q = id_(kk^#P) _ P
Q * (matrix L) * (matrix U) - matrix A
-- warning: lapack support had many bugs in Macaulay2 versions <= 1.0


--
A = QQ[a..d]
f = (a+1)^2-a^2-2*a
lift(f,ZZ)
lift(lift(f,QQ),ZZ)

lift(0.0 * ii + 3.0, RR)


--
assert( ZZ/2 === ZZ/(4,6) )


--
assert not isPseudoprime(101*1617839547365369353)
assert not isPseudoprime(18158848484363*1617839547365369353)
assert isPseudoprime 1617839547365369353


--
assert (not isPrime 1333333)
assert (not isPrime 3133333)
assert (not isPrime 3313333)
assert ( isPrime 3331333)
assert ( isPrime 3333133)
assert ( isPrime 3333313)
assert ( isPrime 3333331)


--
R=ZZ/2[t]
assert isPrime (t^2+t+1)
assert (not isPrime (t^2+1))


--
isHomogeneous (ZZ/7)
isHomogeneous (ZZ/7[x])
isHomogeneous (ZZ/7[x]/(x^2-1))
isHomogeneous ZZ
A = QQ[a,b,c]
B = A[x,y]
isHomogeneous B
isHomogeneous ideal(a*x+y,y^3-b*x^2*y)


--
R = QQ[w,x,y,z];
(L,p) = irreducibleCharacteristicSeries ideal(x^2-y*w,x^3-z*w^2)
n = apply(L, m -> p m)
assert( n === {matrix {{-x^3+w^2*z, -x^2+w*y}}, matrix {{x, w}}} )



--
clearAll
    R = ZZ/101[x_0 .. x_10]
    scan(11, i -> assert(index x_i == i))
    assert( try (index x_11;false) else true )
    R = ZZ/101[w,z,t,e]
    assert( index w == 0 )
    assert( index z == 1 )
    assert( index t == 2 )
    assert( index e == 3 )


--
S = ZZ/101[x,y,z]
M = cokernel vars S
assert ( 0 == HH_-1 res M )
assert ( M == HH_0 res M )
assert ( 0 == HH_1 res M )
assert ( 0 == HH_2 res M )
assert ( 0 == HH_3 res M )
assert ( 0 == HH_4 res M )


     --
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
	  h = diff(m,n)
	  assert( source h === dual P ** Q )
	  assert( target h === dual F ** G )
	  scan(f, i -> scan(g, j -> scan(p, k -> scan(q, l -> assert( h_(g*i+j,q*k+l) === diff(m_(i,k),n_(j,l)))))))



--
     assert( class examples MutableList === Net )


--
R = ZZ/101[a,b,c]/c^4
assert ( genera R == {3,3} )
assert ( genus R == 3 )
assert ( eulers R == {-2,4} )
assert ( euler R == -2 )
R = ZZ/101[a,b,c]/c^3
assert ( genera R == {1,2} )
assert ( eulers R == {0,3} )


--
M = matrix{{1.0,1.0},{0.0,1.0}}
eigenvalues M
eigenvectors M

M = matrix{{1.0, 2.0}, {2.0, 1.0}}
eigenvectors(M, Hermitian=>true)

M = matrix{{1.0, 2.0}, {5.0, 7.0}}
(eigvals, eigvecs) = eigenvectors M
-- here we use "norm" on vectors!
assert( 1e-10 > norm ( M * eigvecs_0 - eigvals_0 * eigvecs_0 ) )
assert( 1e-10 > norm ( M * eigvecs_1 - eigvals_1 * eigvecs_1 ) )

printingPrecision = 2

m = map(CC^10, CC^10, (i,j) -> i^2 + j^3*ii)
(eigvals, eigvecs) = eigenvectors m
max (abs \ eigvals) / min (abs \ eigvals)
scan(#eigvals, i -> assert( 1e-10 > norm ( m * eigvecs_i - eigvals_i * eigvecs_i )))

-- some ill-conditioned matrices

m = map(CC^10, CC^10, (i,j) -> (i+1)^(j+1))
(eigvals, eigvecs) = eigenvectors m
max (abs \ eigvals) / min (abs \ eigvals)
apply(#eigvals, i -> norm ( m * eigvecs_i - eigvals_i * eigvecs_i ))
scan(#eigvals, i -> assert( 1e-4 > norm ( m * eigvecs_i - eigvals_i * eigvecs_i )))

m = map(RR^10, RR^10, (i,j) -> (i+1)^(j+1))
(eigvals, eigvecs) = eigenvectors m
max (abs \ eigvals) / min (abs \ eigvals)
apply(#eigvals, i -> norm ( m * eigvecs_i - eigvals_i * eigvecs_i ))
scan(#eigvals, i -> assert( 1e-4 > norm ( m * eigvecs_i - eigvals_i * eigvecs_i )))



--
m = map(CC^10, CC^10, (i,j) -> i^2 + j^3*ii)
eigenvalues m
m = map(CC^10, CC^10, (i,j) -> (i+1)^(j+1))
eigenvalues m
m = map(RR^10, RR^10, (i,j) -> (i+1)^(j+1))
eigenvalues m


--
     R=ZZ/101[a..d]
     C=resolution cokernel vars R
     D = C ++ C[1] ++ C[2]
     betti D
     assert( degree HH_1 D === 0 )
     assert( degree HH_0 D === 1 )
     assert( degree HH_-1 D === 1 )
     assert( degree HH_-2 D === 1 )


--
     assert( fold(toList, a, {b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c}, d, toList) === {a, {b, {c, d}}} )
     assert( fold(toList, {a,b,c,d}) === {{{a, b}, c}, d} )
     assert( fold({a,b,c,d}, toList) === {a, {b, {c, d}}} )


--
-- Test of various stopping conditions for GB's
R = ZZ/32003[a..j]
I = ideal random(R^1, R^{-2,-2,-2,-2,-2,-2,-2});
gbTrace=3
--time gens gb I;
I = ideal flatten entries gens I;
G = gb(I, StopBeforeComputation=>true); -- now works
m = gbSnapshot I
assert(m == 0)

I = ideal flatten entries gens I;
mI = mingens I; -- works now
assert(numgens source mI == 7)

I = ideal flatten entries gens I;
mI = trim I; -- It should stop after mingens are known to be computed.
assert(numgens source gens mI == 7)

I = ideal flatten entries gens I;
G = gb(I, DegreeLimit=>3); -- this one works
assert(numgens source gbSnapshot I == 18)
G = gb(I, DegreeLimit=>4); -- this one works
assert(numgens source gbSnapshot I == 32)
G = gb(I, DegreeLimit=>3); -- this one stops right away, as it should
assert(numgens source gbSnapshot I == 32)
G = gb(I, DegreeLimit=>5);
assert(numgens source gbSnapshot I == 46)

I = ideal flatten entries gens I;
G = gb(I, BasisElementLimit=>3); -- does the first 3, as it should
assert(numgens source gbSnapshot I == 3)
G = gb(I, BasisElementLimit=>7); -- does 4 more.
assert(numgens source gbSnapshot I == 7)

I = ideal flatten entries gens I;
G = gb(I, PairLimit=>23); --
assert(numgens source gbSnapshot I == 16) -- ?? is this right??

I = ideal flatten entries gens I;
hf = poincare ideal apply(7, i -> R_i^2)
G = gb(I, Hilbert=>hf); -- this works, it seems
assert(numgens source gens G == 67)

Rlex = ZZ/32003[a..j,MonomialOrder=>Eliminate 1]
IL = substitute(I,Rlex);
G = gb(IL, SubringLimit=>1, Hilbert=>hf, DegreeLimit=>2); -- SubringLimit now seems OK
G = gb(IL, SubringLimit=>1, Hilbert=>hf, DegreeLimit=>4);
assert(numgens source selectInSubring(1,gens G) == 1)



--
-- For more determinant tests, see Macaulay2/test/testdet.m2
R = ZZ/103[a,b,c,d]
h = matrix {{a,b},{c,d}}
assert( det h == a * d - b * c )
assert( minors(1,h) == image matrix {{a,b,c,d}} )
assert( minors(2,h) == image matrix {{a * d - b * c}} )
