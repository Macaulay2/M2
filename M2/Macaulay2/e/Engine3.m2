needs "Engine.m2"

-- Test of monomial orders
-- Creation, and their use.

-- Commutative case
makeRing = (mo) -> (
  K = ZmodP 101;
  M = emonoid(mo, toList(0..5), "a b c d e f");
  R = polyring(K, M, degreeRing 1, {1,1,1,1,1,1});
  a = R_(1_K,{0,1});
  b = R_(1_K,{1,1});
  c = R_(1_K,{2,1});
  d = R_(1_K,{3,1});
  e = R_(1_K,{4,1});
  f = R_(1_K,{5,1});)

mo1 = monomialOrder(RevLex=>6)
mo2 = monomialOrder(Weights=>{1,1,1,0,0,0},RevLex=>6)
mo3 = monomialOrder(RevLex=>2,RevLex=>4)
mo4 = monomialOrder(Lex=>2,RevLex=>{1,2,3,4})
mo5 = monomialOrder(Lex=>{3,5},Weights=>{0,0,-1},RevLex=>{1,2,3,4})

-- Graded reverse lex order
makeRing monomialOrder(RevLex=>6)  
assert(leadTerm(a+b^100) == b^100)
assert(leadTerm(a*c + b^2) == b^2)
assert(leadTerm(a*b-1_R) == a*b)
assert(leadTerm(a^3 - b^3) == a^3)
assert(leadTerm(a^3 - b^4) == -b^4)
assert(leadTerm(a+b+c+d+e+f) == a)

-- Lex order (not graded)
makeRing monomialOrder(Lex=>6)
assert(leadTerm(a+b^100) == a)
assert(leadTerm(a*c + b^2) == a*c)
assert(leadTerm(a*b-1_R) == a*b)
assert(leadTerm(a^3 - b^3) == a^3)
assert(leadTerm(a+b+c+d+e+f) == a)

-- Weighted reverse lex order
assert(try (makeRing monomialOrder(RevLex=>{0,0,0,0,0,0})) else true)
makeRing monomialOrder(RevLex=>{1,1,1,6,6,10})
assert(leadTerm(a+b+c+d+e+f) == f)
assert(leadTerm(a^5 + d) == d)
assert(leadTerm(a^6 + d) == a^6)
assert(leadTerm(c*d-b^2) == c*d)
assert(leadTerm(a*c-b^2) == -b^2)

-- Weight function
makeRing monomialOrder(Weights=>{-1,-1,-1,-1,-1,-1},RevLex=>6)
see R 
c+a^2+b^2
leadTerm(a^2+b^2+c) == c
