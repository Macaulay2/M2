--status: this old test depends on internal things and probably should be deleted


---------------------------------------------------
-- Test of engine ring creation code --------------
---------------------------------------------------
-- Also tests whether these are connected at top level correctly

needs "raw-util.m2"
errorDepth = 0

-- Test of the following routines:
-- Arithmetic for each of these is tested in its own file.
-- Here we make sure that the interface is correct for them.
--   rawZZ
--   rawQQ
--   rawZZp
--   rawGaloisField
--   rawRR
--   rawCC
--   rawPolynomialRing (2 forms)
--   rawSkewPolynomialRing
--   rawWeylAlgebra
--   rawSolvableAlgebra
--   rawFractionRing
--   rawLocalRing
--   rawQuotientRing (2 forms)
--   rawSchurRing
-- and
--   rawIsField
--   rawDeclareField
--   rawGetZeroDivisor
--   rawAmbientRing
--   rawDenominatorRing

assert(raw ZZ === rawZZ())
assert(raw QQ === rawQQ())
assert(raw RR_53 === rawRR(53))
assert(raw CC_53 === rawCC(53))
rawZZp 32003
assert (raw (ZZ/32003) =!= rawZZp 32003)
R = ZZ/5
S = ZZ/5
assert(R === S)

assert(instance(rawGaloisField, Function))
assert(instance(rawSchurRing, Function))
assert(instance(rawQuotientRing, Function))
assert(instance(rawLocalRing, Function))
assert(instance(rawFractionRing, Function))
assert(instance(rawSolvableAlgebra, Function))
assert(instance(rawWeylAlgebra, Function))
assert(instance(rawSkewPolynomialRing, Function))
assert(instance(rawPolynomialRing, Function))

assert(instance(rawIsField, Function))
assert(instance(rawDeclareField, Function))
assert(instance(rawGetNonUnit, Function))
assert(instance(rawAmbientRing, Function))
assert(instance(rawDenominatorRing, Function))


----------------------------------
-- Monomial orders ---------------
----------------------------------
needs "raw-util.m2"
mo = rawMonomialOrdering { Lex => 2, GRevLex => {1,2,3}, Weights => {-1}, Position => Up }
M = rawMonoid(mo, ("a","b","c","d","e"), degring 1, {1,1,1,2,3}, {1})
R = rawPolynomialRing(rawZZp 32003, M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
d = rawRingVar(R,3)
e = rawRingVar(R,4)
--status: no one maintains this old test file, so it's broken, since it tests the raw interface
a*a
a+b
a*b^3
b^(2^31-1)
2^31-1
assert try ((a+b^(2^31-1))^2;false) else true
b1 = b^(2^31-1)
assert try (b1*b;false) else true
print "QUESTION: why is monomial overflow happening 5 times?"
a*b^(2^31-1)
print "ERROR: this power should give an error"
  --assert (try (b^(2^31); false) else true)
  --b^(2^31)

a
a*b
(c^1000*d^2000*e^3000)^3

needs "raw-util.m2"
mo = rawMonomialOrdering {
     Lex => 3,
     GroupLex => 1,
     Weights => {1,1,1,1,1},
     LexTiny => 4,
     Position => Down}
M = rawMonoid(mo, ("a","b","c",
	           "d",
		   "g","h","i","j"),
	      degring 1, {1,1,1,1,1,1,1,1}, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
d = rawRingVar(R,3)

g = rawRingVar(R,4)
h = rawRingVar(R,5)
i = rawRingVar(R,6)
j = rawRingVar(R,7)

p = g*h^3*j^255
print "ERROR: overflow is not being checked in the engine here?"
  --j * p

assert try(a^(-1);false) else true
d^(-1)
F = a * d^-1 + d^-2
F^3


-------------------------------
-- all the monordering types --
-------------------------------
needs "raw-util.m2"
--------------
-- Lex -------
--------------
mo = rawMonomialOrdering {
     Lex => 3 }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
-------------------
-- LexSmall -------
-------------------
mo = rawMonomialOrdering {
     LexSmall => 3 }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
assert(toString(a^32767*b^41*c^513) === "a32767b41c513")
print "ERROR: a^32768 should not be allowed?"
assert try (assert(toString(a^32768*b^41*c^513) === "a32768b41c513"); false) else true
-------------------
-- LexTiny -------
-------------------
mo = rawMonomialOrdering {
     LexTiny => 3 }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^127) === "ab41c127")
assert try (a*b^41*c^128;false) else true

R = ZZ[symbol a .. symbol c, MonomialOrder => {LexTiny=>3}]
print "ERROR: max exponent size should be 127 here"
f = a+b^255
a+b^256
assert try(a*f;false) else true
------------------
-- GRevLex -------
------------------
mo = rawMonomialOrdering {
     GRevLex => {1,1,1} }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
toString(a^2312321*b^32352352*c^56464646) === "a2312321b32352352c56464646"
-----------------------
-- GRevLexSmall -------
-----------------------
mo = rawMonomialOrdering {
     GRevLexSmall => {1,1,1} }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
assert try(a^2312321*b^32352352*c^56464646;false) else true
-----------------------
-- GRevLexTiny --------
-----------------------
mo = rawMonomialOrdering {
     GRevLexTiny => {1,1,1} }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b*c^125) === "abc125")
assert try (a*b*c^126;false) else true
assert try (toString(a^1000*b^41*c^513); false) else true
-------------------
-- GroupLex -------
-------------------
mo = rawMonomialOrdering {
     GroupLex => 3 }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
toString(a^-100000 * b * c^-5) === "a^(-100000)bc^(-5)"
-------------------
-- GroupRevLex -------
-------------------
needs "raw-util.m2"
mo = rawMonomialOrdering {
     GroupRevLex => 3 }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
toString(a^-100000 * b * c^-5) === "a^(-100000)bc^(-5)"
--------------------------
-- GRevLex weights -------
--------------------------
needs "raw-util.m2"
mo = rawMonomialOrdering {
     GRevLex => {3,5,7} }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString(a^3+b^3+c^3) == "c3+b3+a3")
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
toString(a^2312321*b^32352352*c^56464646) === "a2312321b32352352c56464646"
-------------------------------
-- GRevLexSmall weights -------
-------------------------------
needs "raw-util.m2"
mo = rawMonomialOrdering {
     GRevLexSmall => {3,5,7} }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
b+c
assert(toString(a^3+b^3+c^3) == "c3+b3+a3")
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a*b^41*c^513) === "ab41c513")
assert(toString(a^1000*b^41*c^513) === "a1000b41c513")
assert try (a^2312321*b^32352352*c^56464646;false) else true
-------------------------------
-- GRevLexTiny weights --------
-------------------------------
mo = rawMonomialOrdering {
     GRevLexTiny => {3,5,7} }
M = rawMonoid(mo, ("a","b","c"), degring 1, 3:1, {1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a === "a")
assert(toString b === "b")
assert(toString c === "c")
assert(toString(a*b*c) === "abc")
assert(toString(a^3+b^3+c^3) == "c3+b3+a3")
--------------------------
-- All together ----------
--------------------------
needs "raw-util.m2"
mo = rawMonomialOrdering {
     Lex => 3,
     GroupLex => 1,
     Weights => {1,1,1,1,1},
     LexTiny => 4,
     LexSmall => 3,
     Position => Down,
     GRevLex => {1,1},
     GRevLexSmall => {1,1,1},
     GRevLexTiny => {2,3,4}}
M = rawMonoid(mo, ("a","b","c",
	           "d",
		   "g","h","i","j",
		   "k","m","n",
		   "o","p",
		   "q","r","s",
		   "t","u","v"),
	      degring 1, 19:1, {1})
R = rawPolynomialRing(rawZZ(), M)

a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)

d = rawRingVar(R,3)

g = rawRingVar(R,4)
h = rawRingVar(R,5)
i = rawRingVar(R,6)
j = rawRingVar(R,7)

k = rawRingVar(R,8)
m = rawRingVar(R,9)
n = rawRingVar(R,10)

o = rawRingVar(R,11)
p = rawRingVar(R,12)

q = rawRingVar(R,13)
r = rawRingVar(R,14)
s = rawRingVar(R,15)

t = rawRingVar(R,16)
u = rawRingVar(R,17)
v = rawRingVar(R,18)

assert(toString(a*g*m^4) === "agm4")
assert(toString(a*m) === "am")
assert(toString(b*m) === "bm")
assert(toString(a*b^2*c^3*d^4*g^5*h^6*i^7*j^6*k^5*m^4*n^3*o^2*p*r^2*s^3*t^4*u^5*v^6)
     === "ab2c3d4g5h6i7j6k5m4n3o2pr2s3t4u5v6")
c+g^4
t+u
t+u+v

-----------------------------
-- testing non term orders --
-----------------------------
needs "raw-util.m2"
mo = rawMonomialOrdering {
     Weights => {0,0,-1,-1,0,3},
     GRevLex => 3:1,
     Lex => 2,
     RevLex => 1
     }

M = rawMonoid(mo, ("a","b","c","d","e","f"),
	      degring 1, 6:1, {1})
R = rawPolynomialRing(rawZZ(), M)

a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
d = rawRingVar(R,3)
e = rawRingVar(R,4)
f = rawRingVar(R,5)
(1+c)*(1+b)
1+c+d^4
end
-----------------------------
complement(A,P) -- need a GB of P.
affine(A,f) -- creates a ring A[t]/(tf-1)...
product(S1,S2,...,Sr) -- not always a multiplicatively closed set
intersect(S1,S2,...,Sr)

poly(K, M, I, S)
  -- K is either basic or ZZ
  -- M is a monoid
  -- I is an ideal in K[M], equipped with a GB (over a field, if base is not ZZ, or
  --  if the ring contains QQ).  Not quite...
  -- S is a multiplicatively closed set in K[M], s.t.
  -- (K[M]/I)_S is K[M]_S/I
poly(K,M)

fractions(K[M], S)

-- Local Variables:
-- compile-command: "M2 -e errorDepth=0 --stop -e 'load \"raw-ring.m2\"' -e 'exit 0' "
-- End:
