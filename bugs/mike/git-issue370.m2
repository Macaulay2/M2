-- issue #370.
A=matrix({{1,2},{2,1}});
(e,v)=eigenvectors(A);
substitute(v_1,RR); -- crashes
lift(v_1,RR_53) -- this should work, I would hope. BUG
lift(v_{1},RR_53) -- this does work
lift(Vector,Ring) := (v,R) -> (lift(matrix v, R))_0

-- issue 473: 
sub(matrix{{1.0}},QQ) -- crash

f = map(QQ,RR_53)
f (matrix{{1.0}}) -- crash

lift(matrix{{1.0}}, QQ)

C = matrix {{1_CC}}
lift(C,RR) -- works
lift(C,QQ) -- error
sub(C,QQ) -- SIGSEGV

kk = ZZ/101
A = matrix{{3_kk}}
sub(A, QQ)
sub(A, ZZ)
sub(A, RR)
sub(A, CC)
phi = map(RR, kk, {})
phi(4_kk)
phi = map(QQ, kk, {})
phi(4_kk)
phi = map(ZZ, kk, {})
phi(4_kk)
lift(A, ZZ)
lift(A, QQ)

end--
