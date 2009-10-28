

R = QQ[x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14,
x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28,
x29, x30];
I = monomialIdeal(
x2*x7*x9*x14*x16*x17*x19*x20*x23*x24*x26*x29*x30,
x1*x6*x7*x8*x11*x12*x13*x14*x16*x21*x24*x25*x26*x27,
x1*x2*x4*x5*x9*x12*x13*x14*x15*x24*x25,
x4*x5*x10*x12*x14*x16*x18*x20*x22*x23*x25*x27,
x1*x2*x3*x4*x8*x12*x14*x17*x21*x22*x25*x27*x28,
x1*x3*x4*x6*x7*x8*x9*x14*x15*x16*x17*x18*x19*x20*x22*x24*x29,
x1*x2*x3*x4*x6*x9*x10*x12*x14*x15*x18*x20*x21*x23*x25*x28*x29,
x1*x3*x4*x6*x7*x8*x10*x11*x13*x14*x19*x23*x24*x25*x27*x29,
x2*x3*x6*x8*x11*x14*x20*x25*x26*x27*x28*x30,
x1*x3*x5*x7*x9*x16*x18*x20*x21*x22*x23*x26*x28*x30,
x1*x2*x3*x8*x10*x11*x13*x14*x16*x17*x18*x19*x20*x21*x26*x27*x30,
x3*x6*x9*x13*x14*x17*x21*x22*x23*x25*x26*x27*x30,
x4*x5*x9*x11*x12*x13*x15*x18*x20*x22*x26*x28*x29*x30,
x1*x2*x3*x5*x7*x8*x9*x10*x14*x19*x24*x28*x29*x30,
x1*x2*x4*x10*x11*x13*x14*x15*x16*x19*x24*x25*x26*x27*x28*x29,
x8*x9*x10*x11*x12*x14*x15*x16*x18*x19*x20*x21*x22*x24*x26*x29*x30,
x1*x2*x4*x5*x7*x8*x9*x10*x13*x15*x17*x18*x19*x22*x23*x25*x29*x30,
x1*x2*x3*x4*x5*x8*x10*x11*x12*x14*x15*x16*x17*x18*x21*x23*x25*x29,
x1*x3*x6*x8*x12*x14*x15*x17*x21*x25*x26*x27*x29*x30,
x1*x3*x4*x18*x19*x20*x24*x25*x26*x28,
x3*x5*x7*x9*x10*x11*x12*x13*x16*x21*x22*x24*x26*x27*x28*x29,
x1*x2*x3*x5*x6*x7*x8*x9*x12*x13*x14*x16*x19*x21*x22*x24*x27*x29*x30,
x1*x4*x5*x8*x9*x12*x13*x20*x21*x24*x26*x28*x30,
x3*x6*x8*x9*x10*x11*x12*x14*x15*x16*x17*x19*x21*x22*x23*x24*x25*x29,
x1*x2*x4*x9*x12*x13*x14*x19*x20*x25*x26*x27*x29*x30,
x2*x3*x5*x6*x7*x11*x15*x16*x17*x21*x22*x23*x25*x27*x29*x30,
x2*x3*x4*x7*x9*x11*x15*x16*x17*x22*x23*x27*x29,
x1*x2*x3*x8*x10*x11*x12*x15*x17*x22*x23*x24*x28*x29*x30,
x3*x4*x5*x7*x9*x13*x14*x15*x16*x18*x23*x27*x29*x30,
x1*x2*x4*x5*x8*x9*x10*x11*x12*x15*x17*x22*x25*x27*x29*x30,
x1*x2*x3*x4*x8*x10*x11*x12*x13*x14*x16*x19*x20*x21*x22*x23*x24*x28*x29,
x3*x5*x11*x12*x14*x16*x17*x19*x20*x23*x25*x26*x28*x29*x30,
x4*x6*x7*x8*x13*x15*x17*x18*x19*x21*x22*x23*x26*x27*x28,
x1*x2*x3*x4*x5*x6*x8*x11*x14*x15*x17*x18*x19*x20*x21*x22*x23*x24*x25*x27*x28,
x6*x8*x10*x13*x15*x19*x23*x24*x27*x28,
x1*x5*x6*x7*x8*x9*x10*x11*x12*x13*x14*x16*x17*x22*x25*x26*x27*x28*x30,
x1*x2*x3*x5*x12*x13*x14*x17*x18*x23*x26,
x4*x11*x12*x17*x19*x24*x28,
x2*x3*x5*x9*x12*x14*x17*x20*x21*x25*x26,
x1*x4*x5*x7*x11*x12*x13*x16*x19*x20*x22*x24*x25*x26*x27*x30
);

time #independentSets I
time #independentSets I
time #independentSets I
time #independentSets I
time #independentSets I
time #independentSets I
time #independentSets I
time #independentSets I

-- On Cygwin, one of these gives a spurious answer.

o48 = 3
i49 : time #independentSets I
   -- used 0.016 seconds
o49 = 3
i50 : time #independentSets I
   -- used 0.015 seconds
o50 = 3
i51 : time #independentSets I
   -- used 8.173 seconds
o51 = 49822
i52 : time #independentSets I
   -- used 0. seconds
o52 = 3
i53 : time #independentSets I
   -- used 0.015 seconds
o53 = 3
i54 : time #independentSets I
   -- used 0.016 seconds
o54 = 3

-- June 22, 2009
Here is a fix for the bug reported above. The fix is to make the
method AssociatedPrimes::codimension set
minprime_limit to -1 as the first thing it does.

The issue is in assprime.cpp, where the fields minprime_limit and
n_minprimes of
AssociatedPrimes are not initialized in the constructors of
AssociatedPrimes. They thus have whatever value happened to be at that
position in memory when the AssociatedPrimes object got constructed.

In order to compute the maximal independent sets, AssociatedPrimes has
to know what the maximal size is, which is the number of variables
minus the codimension. The codimension code runs through the
associated primes in order to find the codimension. However, that code
is a subrutine that does more than just codimension, and in particular
the value of minprime_limit, if non-zero, tells it to stop computation
after having found that number of associated primes (see line 131 of
assprime.cpp). This should never happen when computing the
codimension, and the code is written to try to avoid this happening,
but if minprime_limit and n_minprime (that are random) happen to get
values such that minprime_limit is positive and n_minprime is >=
minprime_limit, then the code stops before being done with computing
the codimension.

Also, setting Limit=0 in independentSets in the M2 language
will compute all independent sets, but from the documentation it
should compute zero independent sets.

Other than that, from looking at the code in assprime.* and
monideal-minprimes.*, I get the impression that someone decided to
remove the class AssociatedPrimes in favor of MinimalPrimes, but got
distracted by something else before finishing with this. Much of the
code is copied. E.g. the bug that is fixed above is already fixed in
MinimalPrimes, which also has a codimension method and uses nearly
identical code to compute it. The codim method in the M2 language on a
monomial ideal uses the corrected MinimalPrimes version of the
codimension code, which is why codim(I) doesn't exhibit this bug.


-- June 29, 2009
Macaulay 2, version 1.2
with packages: Elimination, IntegralClosure, LLLBases,
PrimaryDecomposition,
              ReesAlgebra, SchurRings, TangentCone

i1 : R=QQ[a]

o1 = R

o1 : PolynomialRing

i2 : I=monomialIdeal(0_R)

o2 = 0

o2 : MonomialIdeal of R

i3 : irreducibleDecomposition I
stdio:3:1:(1):[0]: error: expected a polynomial ring without quotient
elements
