-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

restart
R = ZZ[a..d,MonomialOrder=>Lex]
sorted = sort{a*d, b^100, c^3*d}
assert(sorted === {c^3*d, b^100, a*d}) -- fails
assert(c^3*d <= b^100)
assert(b^100 <= a*d)
assert(c^3*d <= a*d)
sorted2 = rsort{a*d, b^100, c^3*d} -- same as sort??

-- files to watch out for:
ann'-doc.m2 (egrep screwed up)
ann-doc.m2 -- already exists??
contract'-doc.m2
diff'-doc
identity-doc.m2 -- already exists??
file functions/normalPrompts-doc.m2 already exists -- not modifying it
file functions/numeric-doc.m2 already exists -- not modifying it
writing functions/peek'-doc.m2
writing functions/quotient'-doc.m2
writing functions/remainder'-doc.m2

-- design flaw:
-- forceGB shouild be
--    forceGB(f, GroebnerBasis=>g, ChangeMatrix=>c, SyzygyMatrix=>s) ?
-- forceGB(f, ChangeMatrix=>m)
--    where does this go to?  The minimal matrix?

-- 'monomials' applied to a matrix with more than one row is coming out messed up
-- see the test in functions/monomials-doc.m2

-- RR and CC stuff:
-- (1) conjugate should work on a MutableMatrix, or Matrix
--     same with conjugate-transpose?
-- (2) U^-1, for U a matrix over CC, is WRONG
-- (3) printingPrecision should be used for matrices?
--     Actually it seems to be incorrect for real numbers in any case.

-- modules -- 10/1/05 -----------------
R = ZZ[a,b]; 
R^{10000:1,10000:2}  -- displays ALL of the degrees on one line!
M = R^3; I = ideal(x); I*M_0 -- should be a submodule
S = R[x,y]
S*a

Ring * RingElement := (R,f) -> (
     if ring f === R then ideal(f)
     else ideal(promote(f,R)))
Ideal * Vector := (I,v) -> (
     image((gens I) ** new Matrix from v)
     )
Ring * Vector := (R,v) -> (
     if ring v =!= R then error "different rings encountered";
     image new Matrix from v
     )
--Module + Vector := (M,v) -> M + (ring v) * v
--Vector + Module := (v,M) -> (ring v) * v + M
isHomogeneous Vector := (v) -> isHomogeneous new Matrix from v

R = QQ[a,b]
S = R[x,y]
M = S^3
I = ideal(x,y)
I*M + S*M_0

M_0; R*M_0 -- should also be allowed
  -- then submodules are easy to do: I*M + R*M_0 + R*a*M_1
  -- WANT: that if I, M, R are graded, then so is this submodule of M. 
M = kernel vars R ++ cokernel vars R
I * M_0
I*M_0 + M_1 + (x-y)*M_2
isHomogeneous oo
M_1
-- Question: given M_0, how do I do anything with it?

---------------------------------
-- Problems arising 11/17/2005:
---------------------------------
mingens Ideal -- computes the entire GB. -- FIXED
%, // require complete GB's
need groebner (and grobner) routine

-- the following all need some efficiency work...
quotients
saturation
elimination

gb: doesn't allow stopping conditions?
decompose: doesn't quit
aborting: lose all your variables

gens gb: recomputes the min gens...

-- This following 4 lines sent to Dan, 12/8/05
S = QQ[a,b,Degrees=>{{0,-1},{1,1}},Heft=>{2,-1}]
N = matrix{{1,2,3}}
substitute(N,S)

-- 12/21/05
R = ZZ/7[x1,x2,x3]
toExternalString monomialIdeal(x1,x2)

-- 12/22/05
gb of a monomial ideal (over a poly ring or quotient by monomials)
  should not recompute the GB.
MonomialIdeal : RingElement now computes horrendous amount.  It
  should call immediately the monomial ideal code.  If the ring element
  is not a monomial, it should still call the monideal code, using the
  monomial ideal of all terms of a polynomial.
hilbert series of a monomial ideal should be using monideal code.
