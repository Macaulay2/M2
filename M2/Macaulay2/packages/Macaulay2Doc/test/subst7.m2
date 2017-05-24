-- issue 370:
A=matrix({{1,2},{2,1}});
(e,v)=eigenvectors(A);
assert try (substitute(v_1,RR);false) else true -- used to crash (<= 1.9.2)
assert try (substitute(v_{1},RR);false) else true -- used to crash (<= 1.9.2)
assert(ring lift(v_{1},RR_53) === RR_53)

-- issue 473: 
assert try (sub(matrix{{1.0}},QQ);false) else true -- used to crash (<= 1.9.2)


f = map(QQ,RR_53)
assert try (f (matrix{{1.0}}); false) else true  -- used to crash (<= 1.9.2)

assert try (lift(matrix{{1.0}}, QQ); false) else true

C = matrix {{1_CC}}
assert(ring lift(C,RR) === RR_53) -- works
assert try(lift(C,QQ); false) else true 
assert try (sub(C,QQ); false) else true -- used to crash (<= 1.9.2)

kk = ZZ/101
A = matrix{{3_kk}}
assert(3 == sub(A, QQ))
assert(3 == sub(A, ZZ))
assert(3 == sub(A, RR))
assert(3 == sub(A, CC))
phi = map(RR, kk, {})
assert(4 == phi(4_kk))
phi = map(QQ, kk, {})
assert(4 == phi(4_kk))
phi = map(ZZ, kk, {})
assert(4 == phi(4_kk))
assert(3 == lift(A, ZZ))

