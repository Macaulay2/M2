-- used to be in 0-core-tests.m2
gbTrace = debugLevel = 1
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (3,toList (t_1 .. t_9))			  -- 3 by 3
n = matrix pack (3,toList (u_1 .. u_9))			  -- 3 by 3

-- Try the following with various resolution algorithms
alg = 0
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1, Strategy => alg)
assert( rank C_2 == 2 )
C = res(M, LengthLimit => 4, DegreeLimit => 2, Strategy => alg)
assert( rank C_2 == 33 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4, Strategy => alg)
assert( rank C_5 == 0 )

alg = 1
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1, Strategy => alg)
assert( rank C_2 == 2 )
C = res(M, LengthLimit => 4, DegreeLimit => 2, Strategy => alg)
assert( rank C_2 == 33 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4, Strategy => alg)
assert( rank C_5 == 0 )

alg = 2
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1+2, Strategy => alg)
assert( rank C_2 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 2+2, Strategy => alg)
assert( rank C_2 == 34 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3+2, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4+2, Strategy => alg)
assert( rank C_5 == 0 )

alg = 3
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 4, DegreeLimit => 1+2, Strategy => alg)
assert( rank C_2 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 2+2, Strategy => alg)
assert( rank C_2 == 34 )
assert( rank C_3 == 32 )
assert( rank C_4 == 3 )
C = res(M, LengthLimit => 4, DegreeLimit => 3+2, Strategy => alg)
assert( rank C_3 == 60 )
assert( rank C_4 == 61 )
C = res(M, LengthLimit => 4, DegreeLimit => 4+2, Strategy => alg)
assert( rank C_5 == 0 )
