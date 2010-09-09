-- This file involves gb over ZZ and QQ, ZZ[]
-- Currently (8 Sep 2010) I (MES) do not see any bug here anymore

assert(gens gb matrix {{-1}} == matrix"1")
assert(gens gb matrix {{1}} ==  matrix"1")

gens gb random(ZZ^3, ZZ^4)

M = matrix {{-6, -3, 7}, {5, 3, -1}, {-2, -6, 4}}
gens gb M
hermite M
leadTerm gens gb M
N = random(ZZ^5, ZZ^4)
R = ZZ[x]
M1 = substitute(M,R)
gens gb M1
M = matrix(QQ, {{-6, -3, 7}, {5, 3, -1}, {-2, -6, 4}})
gens gb M
leadTerm gens gb M

N1 = substitute(N,R)
gens gb N1
LLL gens gb N -- kill internal debug message
try LLL N1 else true -- check that the ring is correct! This used to be a CRASH
S = QQ[x]
N2 = substitute(N,S)
gens gb N2

S = QQ[x,MonomialOrder=>Position=>Up]
N2 = substitute(N,S)
gens gb N2

gens gb substitute(N,QQ)
gens gb N
-- Kinds of lead terms

debug Core
M = random(ZZ^100, ZZ^70)
M1 = substitute(M,QQ)
time gens gb M1;
Ms = mutableMatrix M
time rawFFLU raw Ms

S = QQ[x,MonomialOrder=>Position=>Up]
M2 = substitute(M,S);
time gens gb M2;

T = ZZ[MonomialOrder=>Position=>Up]
M2 = substitute(M,T);
time gens gb M2;

-- The GB over ZZ code is much better
M = random(ZZ^70, ZZ^40);
--time gens gb M; -- This one is very bad...
T = ZZ[x,MonomialOrder=>Position=>Up]
M1 = substitute(M,T);
gbM1 = time gens gb M1;
gens gb M == substitute(gbM1,ZZ) -- true!

T = ZZ[MonomialOrder=>Position=>Up]
M = random(ZZ^70, ZZ^40);
time gens gb M;
M1 = substitute(M,T);
gbM1 = time gens gb M1;
gens gb M == substitute(gbM1,ZZ) -- true!

T = QQ[MonomialOrder=>Position=>Up]
M = random(QQ^70, QQ^40);
time gens gb M;
M1 = substitute(M,T);
gbM1 = time gens gb M1;
gens gb M == substitute(gbM1,ZZ) -- true!


end

This gb is not sufficiently autoreduced, because the off-diagonal element -5
could be reduced further in size by the 3 on the diagonal.  In fact, to ensure
uniqueness of result, the off-diagonal elements should all be non-negative, too,
as in Hermite normal form, i.e., it should be the same.

    i15 : gens gb matrix {{-6, -3, 7}, {5, 3, -1}, {-2, -6, 4}}

    o15 = | 25 -10 6  |
	  | 0  3   -5 |
	  | 0  0   2  |

		   3        3
    o15 : Matrix ZZ  <--- ZZ

-- Local Variables:
-- M2-send-to-buffer: "*M2*"
-- End:
