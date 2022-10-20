-- Test of exterior power of a matrix, and determinants
-- and associated optional computation arguments

testexterior = (p,m1,m2) -> 
  assert(exteriorPower(p,m1*m2) == exteriorPower(p,m1) * exteriorPower(p,m2))

testsame = (p,m) -> if p >= 0 then
     assert(minors(p,m) == ideal exteriorPower(p,m))

testsame1 = (p,m) -> (
     time answer1 := gens minors(p,m,Strategy=>Bareiss);
     time answer2 := gens minors(p,m,Strategy=>Cofactor);
     assert(answer1 == answer2))

testsame2 = (p,m) -> (
     time answer1 := exteriorPower(p,m,Strategy=>Bareiss);
     time answer2 := exteriorPower(p,m,Strategy=>Cofactor);
     assert(answer1 == answer2))

R = ZZ/101[a..d]
m = matrix{{a,b},{c,d}}
assert(det m == a*d - b*c)
assert(minors(2,m) == ideal(a*d-b*c))
assert(minors(1,m) == ideal(a,b,c,d))
assert(minors(0,m) == ideal(1_R))
assert(minors(-1,m) == 1)
assert(minors(3,m) == 0)

testsame1(1,m)
testsame1(2,m)
testsame1(3,m)

assert(exteriorPower(2,m) == matrix{{det m}})
assert(exteriorPower(1,m) == m)
assert(exteriorPower(0,m) == matrix{{1_R}})

assert(exteriorPower(2,m^3) == (exteriorPower(2,m))^3)
assert(exteriorPower(2,m^6) == (exteriorPower(2,m))^6)

R = ZZ[vars(0..34)]
m1 = genericMatrix(R,a,3,5)
m2 = genericMatrix(R,p,5,4)

testexterior(-1,m1,m2)  
testexterior(0,m1,m2)  
testexterior(1,m1,m2)  
testexterior(2,m1,m2)  
testexterior(3,m1,m2)  
testexterior(4,m1,m2)

testsame(-1,m1)
testsame(0,m1)
testsame(1,m1)
testsame(2,m1)
testsame(3,m1)
testsame(4,m1)

testsame1(-1,m1)
testsame1(0,m1)
testsame1(1,m1)
testsame1(2,m1)
testsame1(3,m1)
testsame1(4,m1)

F = GF(8, Variable => x)
m = matrix{{x,1},{x^2,x^3}}
assert ( det m == x^4 - x^2 )

R = QQ
m = random(QQ^8,QQ^8)
testsame1(8,m)
-- testsame1(8,m) time on my Mac PB G3: 0.58 sec (Bareiss), 148.44 sec (Cofactor).
-- 4/30/2001.

R = ZZ/32003[a..d]
m = random(R^6, R^{6:-1})
testsame1(6,m)

R = ZZ/32003[x,y,z]
m = random(R^12, R^{12:-1}) -- changed 18 to 12, so it would run somewhat faster...
time minors(12,m,Strategy=>Bareiss);

R = ZZ/101[vars(0..14)]

  /// generateAssertions
  M = genericMatrix(R,a,3,5)
  exteriorPower(3,M)
  N = genericMatrix(R,a,2,5)
  P = transpose exteriorPower(2,N)
  transpose P
  ///

assert( (M = genericMatrix(R,a,3,5)) 
     === map(R^{{0}, {0}, {0}}, R^{{-1}, {-1}, {-1}, {-1}, {-1}}, 
	  {{a, d, g, j, m}, {b, e, h, k, n}, {c, f, i, l, o}}) )
assert( exteriorPower(3,M) 
     === map(R^{{0}}, R^{{-3}, {-3}, {-3}, {-3}, {-3}, {-3}, {-3}, {-3}, {-3}, {-3}}, 
	  {{-c*e*g+b*f*g+c*d*h-a*f*h-b*d*i+a*e*i, -c*e*j+b*f*j+c*d*k-a*f*k-b*d*l+a*e*l, -c*h*j+b*i*j+c*g*k-a*i*k-b*g*l+a*h*l, 
		    -f*h*j+e*i*j+f*g*k-d*i*k-e*g*l+d*h*l, -c*e*m+b*f*m+c*d*n-a*f*n-b*d*o+a*e*o, 
		    -c*h*m+b*i*m+c*g*n-a*i*n-b*g*o+a*h*o, -f*h*m+e*i*m+f*g*n-d*i*n-e*g*o+d*h*o, 
		    -c*k*m+b*l*m+c*j*n-a*l*n-b*j*o+a*k*o, -f*k*m+e*l*m+f*j*n-d*l*n-e*j*o+d*k*o, 
		    -i*k*m+h*l*m+i*j*n-g*l*n-h*j*o+g*k*o}}) )
assert( (N = genericMatrix(R,a,2,5)) 
     === map(R^{{0}, {0}}, R^{{-1}, {-1}, {-1}, {-1}, {-1}}, 
	  {{a, c, e, g, i}, {b, d, f, h, j}}) )
assert( (P = transpose exteriorPower(2,N)) 
     === map(R^{{2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}, {2}}, R^{{0}}, 
	  {{-b*c+a*d}, {-b*e+a*f}, {-d*e+c*f}, {-b*g+a*h}, {-d*g+c*h}, {-f*g+e*h}, {-b*i+a*j}, {-d*i+c*j}, {-f*i+e*j}, {-h*i+g*j}}) )
assert( transpose P === map(R^{{0}}, R^{{-2}, {-2}, {-2}, {-2}, {-2}, {-2}, {-2}, {-2}, {-2}, {-2}}, 
	  {{-b*c+a*d, -b*e+a*f, -d*e+c*f, -b*g+a*h, -d*g+c*h, -f*g+e*h, -b*i+a*j, -d*i+c*j, -f*i+e*j, -h*i+g*j}}) )

R = ZZ[vars(0..24)]
m = genericMatrix(R,a,5,5)
minors(1,m)
minors(2,m)
assert(minors(3,m) == ideal exteriorPower(3,m))
assert(minors(4,m) == ideal exteriorPower(4,m))
assert(minors(5,m) == ideal exteriorPower(5,m))
assert(minors(6,m) == ideal exteriorPower(6,m))
assert(gens minors(0,m) == matrix{{1_R}})
assert(numgens minors(-1,m) == 1)

R = ZZ[vars(0..24)]
m = genericMatrix(R,a,3,6)
minors(1,m)
minors(6,m)
minors(0,m)
minors(-1,m)
exteriorPower(-1,m)
exteriorPower(0,m)
time exteriorPower(1,m);
time exteriorPower(2,m);
time exteriorPower(3,m);
time exteriorPower(4,m);
time exteriorPower(5,m);
testsame2(1,m);
testsame2(2,m);
testsame2(3,m);
testsame2(4,m);
testsame2(5,m);

-- The following are examples where Cofactors are better...
m2 = (transpose m) * m
testsame2(1,m2);
testsame2(2,m2);
-- too long for testing: testsame2(3,m2);
-- too long for testing: testsame2(4,m2);
-- testsame2(5,m2);
testsame2(6,m2);

-- What about a non-domain
R = ZZ/101[a..d]/(a^2)
m = matrix{{c,b,a},{a,d,1},{a,b,c}}
answer1 = det(m,Strategy=>Bareiss)
answer2 = det(m,Strategy=>Cofactor)
assert(a * (answer1 - answer2) == 0) -- They should differ by a zero-divisor.

x = symbol x
R = ZZ[x_1 .. x_49]
m = genericMatrix(R,x_1,7,7)
time exteriorPower(3,m);

R = ZZ/101
F = R^4
wedgeProduct(1,2,F)
wedgeProduct(0,0,F)
wedgeProduct(0,1,F)
exteriorPower(0,F)
target exteriorPower(0,id_F)
F == target exteriorPower(1,id_F)
target exteriorPower(2,id_F)

-- testing computation of some minors:
R = ZZ[vars(0..35)]
m = genericMatrix(R,a,6,6)
m3 = minors(3,m,Limit=>10)
m3 = minors(3,m,Limit=>10, First=>{{1,2,3},{1,2,3}})
numgens m3

minors(3,m,First=>{{2,3,4},{0,1,2}},Limit=>10)
time minors(3,m);
testsame1(3,m)
testsame2(3,m)
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test testdet.out"
-- End:
