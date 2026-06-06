R = ZZ[x,y,z]
modules = {
    image matrix {{x^2,x,y}},
    coker matrix {{x^2,y^2,0},{0,y,z}},
    image matrix {{x,y}} ++ coker matrix {{y,z}},
    R^{-1,-2,-3}
    }

scan(modules, M -> assert( cover exteriorPower(2, M) == exteriorPower(2, cover M) ))
scan(modules, M -> assert( cover cokernel M_{1} == cover M ))
scan(modules, M -> assert( cover M == target presentation M ))
table(modules, modules,
    (P, Q) -> assert( cover P ** cover Q == cover(P ** Q) ));

-- see https://github.com/Macaulay2/M2/issues/2550
(t, M) = toSequence timing symmetricPower(2, R^(splice{3:2, 16:1, 3:0}))
assert(flatten degrees M == splice{3:-4, 16:-3, 3:-2, 2:-4, 16:-3, 3:-2, -4..-3,
	15:-3, 19:-2, 3:-1, 15:-2, 3:-1, 14:-2, 3:-1, 13:-2, 3:-1, 12:-2, 3:-1,
	11:-2, 3:-1, 10:-2, 3:-1, 9:-2, 3:-1, 8:-2, 3:-1, 7:-2, 3:-1, 6:-2, 3:-1,
	5:-2, 3:-1, 4:-2, 3:-1, 3:-2, 3:-1, 2:-2, 3:-1, -2..-1, 2:-1, 6:0})
assert(t < 1)

--
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
assert( exteriorPower(2,m) == matrix {
	{-b*d+a*e, -b*g+a*h, -e*g+d*h},
	{-c*d+a*f, -c*g+a*i, -f*g+d*i},
	{-c*e+b*f, -c*h+b*i, -f*h+e*i}} )
assert( exteriorPower(3,m) == matrix {{-c*e*g+b*f*g+c*d*h-a*f*h-b*d*i+a*e*i}} )

--
R = ZZ/101[a..d]
I = monomialCurveIdeal(R,{1,3,4})
A = R/I
jacobian A
singA = minors(codim ideal presentation A, jacobian A)
generators gb singA

-- fitting ideal
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
