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

M_0; S*M_0 -- should also be allowed
  -- then submodules are easy to do: I*M + R*M_0 + R*a*M_1
  -- WANT: that if I, M, R are graded, then so is this submodule of M. 
M = kernel vars S ++ cokernel vars S
I * M_0
I*M_0 + S*M_1 + S*(x-y)*M_0
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
minimalPrimes: doesn't quit
aborting: lose all your variables

gens gb: recomputes the min gens...

-- This following 4 lines sent to Dan, 12/8/05 FIXED
S = QQ[a,b,Degrees=>{{0,-1},{1,1}},Heft=>{2,-1}]
N = matrix{{1,2,3}}
substitute(N,S)

-- Resolutions over such rings FIXED
S = QQ[a,b,Degrees=>{{0,-1},{1,1}},Heft=>{2,-1}]
res ideal vars S

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

-- 1/18/06
R = QQ[a..d]
I = ideal"ab,cd,a2c,abd2"
primaryDecomposition I -- this gives answer as ideals.  That is what it should be.

-- 1/18/06
A = QQ[a]/(a^2+a+1)
isField A
toField A
isField A
B = A[x,y,z]
I = ideal(a*x-2, a^2*y^2-x-1)
gens gb I
(1_A//a) * a -- this works
(a+1)/a -- recursion limit of 300 exceeded.  Should / over a ring be same as // ?
        -- perhaps: 'toField' should install a new "/" routine that calls // ?

J = Grass(2,5)
ZZ/101 ** (ring J) -- fails, why?
coefficientRing (ZZ/101) -- because it is somehow using this
ZZ/101 ** (gens J) -- should we allo this sort of thing?
RJ = (ZZ/101) monoid ring J
(gens J) ** RJ -- fails


-- 1/18/06
-- problems to fix
  -- sort
  -- toExternalString and toString
  -- classic input
  -- abort
  -- monideal code (bit vague...)
  -- finite extensions of QQ or ZZ/p as fields
  -- gb stop conditions
  -- in reduction: %, should not recompute GB.
  -- resolutions: seem to recompute resolution 
  --   are stop conditions for resolutions working?
  -- display problems:
  --   wrapping of lines would be better if it cut between polynomials if possible
  --   R^{10000:1} (not wrapped)
  --   monideals (not wrapped)
  --   use 'show()', 'see()', 'put()' ?
  --   polynomial rings also don't wrap
  -- multi-graded things

-- 1/18/06
R = QQ[a..d]
time basis(1000,R);
-- <abort>
-- in debugger (sort of), R not visible.

-- 1/19/06
help -- displays very long lines
listUserSymbols doc: says something about 'loaddata', why?
in info mode: paragraphs should have a blank line?
sourceHomeDirectory --> emacsPath
environment
path in doc is silly
unexport 

-- 1/25/06 Questions and issues for Dan
'local', 'global', 'symbol'.  Is 'global' different from 'symbol'?
readme file for linux
match up the 'getting started' page with the readme's?
classic mode
emacsPath?
For me: remove missing node/extra node doc nodes.
linking to doc pages in another package.

in order to statically link on macaulay.math.cornell.edu (linux), I need to add
  in the -latlas.  Let's put this or something else in more globally...
  -- use --with-lapacklibs=
in capybara: get conflicts with packages on macaulay.math.cornell.edu
same in M2 after doing 'installPackage "Macaulay2"'
where do I upload files to have them on our website?
I would like the Dmodule package in the dist, if possible?  is this easy?

operators: which are active?  precedence?  which can have methods installed?
also: under linux, example files can be built in short time, so make sure that
  we redo them all.

1/26/06
bug:
  R = ZZ[s,t]
  S = ZZ[a..d]
  F = map(R,S,{s^3,s^2*t,s*t^2,t^3})
  kernel F -- not implemented yet

1/19/06
bug, emailed to Dan. FIXED
  6.667 * 10^15
  floor oo
  -- gives 436928511999999934464

toString matrix{{6.667 * 10^15}}

2/5/06
bug, emailed to Dan. (subject: RR bug)

There are 2 bugs here (1 bug, 1 missing feature). 
 (1) Can't input real numbers of form: 1.23e14
 (2) Want an outputPrecision value, or something similar.
 
i1 : 1.234535 * 10^15

o1 = 1.23453e15

o1 : RR

i2 : toString o1

o2 = 1.23453e15

i3 : toExternalString o1

o3 = 1.23453e15

