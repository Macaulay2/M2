Qsqrt2 = QQ[sqrt2]/(sqrt2^2-2)
Rx=Qsqrt2[x]
Rxy = Rx[y]/(y^2 - poly "x4+4x3+2x2+1")
(x-sqrt2/2)
(x-sqrt2/2) + (y-(1/2+sqrt2))
oo * ((x-sqrt2/2) + (-y-(1/2+sqrt2)))
end

From: Brent Baccala <cosine@freesoft.org>
Date: May 8, 2008 4:46:47 AM EDT
To: Macaulay2@math.uiuc.edu
Subject: trying to use macaulay2 for integration

Hi -

I used Macaulay 2 this last week to put together a lecture on modern
integration theory (the algebraic curve case), and, I have to admit,
it wasn't a pleasant experience, but I guess you need to hear about
the problems as well as the successes.

Here are the highlights:

Macaulay 2, version 1.1
with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone

i1 : Qsqrt2 = QQ[sqrt2]/(sqrt2^2-2)

o1 = Qsqrt2

o1 : QuotientRing

i2 : Rx=Qsqrt2[x]

o2 = Rx

o2 : PolynomialRing

i3 : Rxy = Rx[y]/(y^2 - poly "x4+4x3+2x2+1")

o3 = Rxy

o3 : QuotientRing

i4 : (x-sqrt2/2)

        1
o4 = x - -*sqrt2
        2

o4 : Rx

i5 : (x-sqrt2/2) + (y-(1/2+sqrt2))

            3         1
o5 = y + x - -*sqrt2 - -
            2         2

o5 : Rxy

i6 : o5 * ((x-sqrt2/2) + (-y-(1/2+sqrt2)))

       4     3    2                     9      2   3         3
o6 = - x  - 4x  - x  + (- 3sqrt2 - 1)x + -*sqrt2  + -*sqrt2 - -
                                        4          2         4

o6 : Rxy


This is disappointing, to say the least.  I constantly found myself
having to clear these sqrt2^2 expressions manually using something
like toString(o8), then cut and paste to put it back into the program.

Here's another session that illustrates several problems:


Macaulay 2, version 1.1
with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone

i1 : Rx = QQ[x]

o1 = Rx

o1 : PolynomialRing

i2 : root = poly "x4+4x3+2x2+1"

     4     3     2
o2 = x  + 4x  + 2x  + 1

o2 : Rx

i3 : num=poly "2x6+4x5+7x4-3x3-x2-8x-8"

      6     5     4     3    2
o3 = 2x  + 4x  + 7x  - 3x  - x  - 8x - 8

o3 : Rx

i4 : V=poly "2x2-1"

      2
o4 = 2x  - 1

o4 : Rx

i5 : Fx = frac Rx

o5 = Fx

o5 : FractionField

i6 : Fxy = Fx[y]/(y^2-root)

o6 = Fxy

o6 : QuotientRing

i7 : ytic=diff(x,root)//2/root*y
stdio:7:6:(1):[0]: expected a polynomial ring

i8 : use Rx

o8 = Rx

o8 : PolynomialRing

i9 : ytic=diff(x,root)//2/root*y

        3     2
      2x  + 6x  + 2x
o9 = ------------------*y
     4     3     2
    x  + 4x  + 2x  + 1

o9 : Fxy

i10 : root*(V*ytic - diff(x,V)*y)

          4     3     2
o10 = (- 4x  - 6x  - 6x  - 6x)y

o10 : Fxy

i11 : num/o10

         6     5     4     3    2
     - 2x  - 4x  - 7x  + 3x  + x  + 8x + 8
     -------------------------------------
                4     3     2
              4x  + 6x  + 6x  + 6x
o11 = -------------------------------------
                       y

o11 : frac(Fxy)

i12 : num/o10*y

         6      5      4      3     2
     - 8x  - 16x  - 28x  + 12x  + 4x  + 32x + 32
o12 = -------------------------------------------
                  4      3      2
               16x  + 24x  + 24x  + 24x

o12 : frac(Fxy)

i13 : sub(o10, y => -y)

        4     3     2
o13 = (4x  + 6x  + 6x  + 6x)y

o13 : Fxy

i14 : sub(o11, y => -y)
stdio:14:1:(1):[0]: expected y to be a generator of frac((Fx [y])/(y^2-x^4-4*x^3-2*x^2-1))

i15 : quit


i7 is really annoying; I guess if I don't "use Rx" then the "x" in
"diff(x,root)" is taken to be in Fx or Fxy.  But the real problem is
o12 - notice that everything is a multiple of 4, but it fails to clear
that.  And what's the big difference between i13 and i14?  I can't
tell you how many times I wanted to compute a field norm like that
only to end up staring at "expected y to be a generator".

Here's a snippet from the middle of a typescript of an longer session
that illustrates how bad some of this got.  I've calculated o207, but
it's unsimplified to an absurd degree, so I toString it, then cut and
paste to get o209:

i207 : o205*o184

             5         4         3         2                2
      (11264x  + 33792x  - 22528x  - 28160x  + (- 11264sqrt2  + 19712)x + 18304
o207 = -------------------------------------------------------------------------


      -------------------------------------------------------------------------
           2                  8         7          6         5                2
      sqrt2  - 14080)y + 5632x  + 45056x  + 118272x  + 95744x  + (- 18304sqrt2
      -------------------------------------------------------------------------
                                                                 2            2
                                                            5632x  - 1408sqrt2
      -------------------------------------------------------------------------
                4                2          3                2          2
       - 14080)x  + (- 73216sqrt2  + 45056)x  + (- 42240sqrt2  + 46464)x  + (56
      -------------------------------------------------------------------------
      -------------------------------------------------------------------------
             2                       4             2
      32sqrt2  - 18304)x + 12672sqrt2  - 22880sqrt2  + 14432
      ------------------------------------------------------

o207 : frac(Rxy)

i208 : toString(o207)

o208 = ((11264*x^5+33792*x^4-22528*x^3-28160*x^2+(-11264*sqrt2^2+19712)*x+18304*
      sqrt2^2-14080)*y+5632*x^8+45056*x^7+118272*x^6+95744*x^5+(-18304*sqrt2^2-
      14080)*x^4+(-73216*sqrt2^2+45056)*x^3+(-42240*sqrt2^2+46464)*x^2+(5632*
      sqrt2^2-18304)*x+12672*sqrt2^4-22880*sqrt2^2+14432)/(5632*x^2-1408*sqrt2^
      2)

i209 : ((11264*x^5+33792*x^4-22528*x^3-28160*x^2+(-11264*sqrt2^2+19712)*x+18304*        sqrt2^2-14080)*y+5632*x^8+45056*x^7+118272*x^6+95744*x^5+(-18304*sqrt2^2-        14080)*x^4+(-73216*sqrt2^2+45056)*x^3+(-42240*sqrt2^2+46464)*x^2+(5632*
             sqrt2^2-18304)*x+12672*sqrt2^4-22880*sqrt2^2+14432)/(5632*x^2-1408 *sqrt2^       2)

          6       5       4       3      2                     7       6
      (16x  + 128x  + 408x  + 544x  - 20x  - 208x - 174)y + 64x  + 488x  + 1152
o209 = -------------------------------------------------------------------------
                                                    16y + 32x + 8
      -------------------------------------------------------------------------
       5       4       2
      x  + 812x  - 274x  - 184x - 183
      -------------------------------

o209 : frac(Rxy)


I'm not trying to bitch and gripe.  If it's perceived that way at
first, then please note that I didn't just drop you that entire
typescript; I tried to clean things up into representative
small sessions to illustrate the problems I had.

You've suggested to me before the idea of a Macaulay 2 library to do
integration.  I definitely was thinking as I worked on this that I
could use some library functions to do things like resultants, field
norms, and elementary row reduction to upper triangular form, none of
which (I think) is available standard in Macaulay 2, and I'd be
willing to work on and contribute some of that.

But first, it's got to simplify sqrt2^2!

Thank you.

					-bwb

					Brent Baccala
					cosine@freesoft.org
