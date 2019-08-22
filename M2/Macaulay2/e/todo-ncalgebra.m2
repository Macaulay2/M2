todo list being redone with start of semester, 22 Aug 2019 FM+MS
0. rename PolynomialAlgebra.m2 package.  To what?
  (this package needs documentation).
1. get code back working:  rawNCFreeAlgebra has changed arguments.
  Fix the code that calls this.
2. Get other monomial orders working: mainly elimination orders, also different degrees.
3. Make a series of tests for noncomm gc.
4. Clean this file up: get rid of old cruft code.
5. Play with magma, singular, bergman, gap for noncomm performance.
6. Run profiler on our nc gb code.  What do we need to improve?

The following more specific list is still active todo as well:

todo: for "today":
  1. debug NCGB
  1a. put tests, clean up code, code review.
  2. add in computation type, some functions to query
      computation object
      make one
      compute it (soft degree bound)
      get answer from it
      get status
      add interrupts to M2
      add gbTrace usage.
  3. NCAlgebra class (quotient of a free algebra) (DONE)
  4. SuffixTree (this code is functional, but perhaps is buggy, or performance buggy).
  5. Better reduction (heap, poly with pos, using hashtables)
  6. F4 like reduction of overlap pairs.
also:
  change local rings to not use braces for construction.  
-------------------------------

Steps to make NCalgebra arithmetic in the engine

restart
debug Core    
kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"), raw degreesRing 1)
1_R
a = R_0
b = R_1
c = R_2
-a
a*b*a*b*b*a*a*a
a > b
a < b
a >= b
a <= b
a == b -- not sure why this is returning true
a*b*a*b*b*a*a*c > a*b*a*b*b*a*a*b
a*b*b*a*a*c > a*b*a*b*b*a*a*b
a*b*a*b*b*a*a*c > a*b*b*a*a*b
f = a+b+c
-- this thing takes up a lot of memory... 3^12 terms!
time(f*f*f*f*f*f*f*f*f*f*f*f);
time(f3 = f*f*f);
time(f3*f3*f3*f3);
g = a-b-c
f*g-g*f
f*f


restart
needsPackage "NCAlgebra"
R = QQ{a,b,c}
f = a+b+c
elapsedTime time(f^12);
M = ncMatrix {{a,b},{c,a}}
elapsedTime(M^10);

restart
debug Core    
kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"), raw degreesRing 1)
1_R
a = R_0
b = R_1
c = R_2

matrix{{f}}
R^5
a * rawIdentity(R^5, 5)
rawMutableMatrix(R,4,4,true)
elems = toSequence flatten {{a,b,c},{b*a,c*a,a*c}}
M = rawMatrix1(R^2, 3, elems, 0)
N = rawDual M
M*N
oo * oo

elems = toSequence flatten {{a,b},{c,a}}
M = rawMatrix1(R^2, 2, elems, 0)
M*M*M
time (M*M*M*M*M*M*M*M*M*M);

M = rawMutableMatrix(R,4,4,false) -- crashes


R = QQ {a,b,c}
coefficientRing R
f = a*a*a*a*a + a*b*a*b;
terms f
ring f
rawPairs(raw coefficientRing R, raw f)

time g = f^20;
time g = f^10;
time h = g*g;
f + f^2
R_0

restart
needsPackage "NCAlgebra"
R = QQ {a,b,c}
f = a*a*a*a*a + a*b*a*b;
time g = f^20;

restart
needsPackage "PolynomialAlgebra"
R = QQ {a,b,c}
f = a*a*a*a*a + a*b*a*b;
time g = f^20;

R_0
gens R

kk = QQ
R = rawNCFreeAlgebra(raw kk, ("a","b","c"), raw degreesRing 1)
A = newNCEngineRing R;


