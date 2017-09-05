
--###################################
-- Binomial triangularize
--###################################

checkTriangularize "Binomial"

--###################################
-- Triangular sets
--###################################

-- regular chains

R = QQ[x,y,t,s, MonomialOrder=>Lex];
F = {x + y^2 - t, t^2 -s};
T = triaSystem(R,F,{});
assert(isRegularChain T)
assert(isStronglyNormalized T)
f = x*y*t;
assert(resultant(f,T)==y^2*s^2-y^6*s)
assert(codim T == 2)
assert(dim T == 2)

R = QQ[x,y,z, MonomialOrder=>Lex];
F = {x*y - y*z, y^2 - z^2};
T = triaSystem(R,F,{y});
assert(isRegularChain T)
assert(not isStronglyNormalized T)

-- prem

R = QQ[a,b,c,d,e, MonomialOrder=>Lex];
T = triaSystem (R,{a*b-c,b^2-c},{b})
assert(isRegularChain T)
assert(not isStronglyNormalized T)
assert((a-b) % T == 0)

--###################################
-- Others
--###################################

-- minimalObjects

L = 2..50
cmp = (i,j) -> if i%j==0 then 1 else if j%i==0 then -1 else 0;
minl = minimalObjects (L,cmp);
primes = (2,3,5,7,11,13,17,19,23,29,31,37,41,43,47)
assert( select(L,i ->minl#i) == primes )
