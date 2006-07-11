I gave this bug to Mike, as 1-res-change-basis.  Let's wait and see what happens.

-----------------------------------------------------------------------------

Date: Fri, 7 Jul 2006 11:24:02 -0500 (CDT)
From: Dan Grayson <dan@math.uiuc.edu>
To: mike@math.cornell.edu
CC: dan@math.uiuc.edu
Subject: res image gens gb
Reply-to: dan@math.uiuc.edu


Is there a reason that the routine for sorting the columns of the matrices in a
resolution is not the same as the routine for sorting the columns of the
matrices in a gb?

    i23 : res image gens gb k'

	   6      4
    o23 = R  <-- R  <-- 0

	  0      1      2

    o23 : ChainComplex

    i24 : degrees source gens gb k'

    o24 = {{5}, {0}, {0}, {1}, {4}, {4}}

    o24 : List

    i25 : degrees o23_0

    o25 = {{0}, {0}, {1}, {4}, {4}, {5}}

    o25 : List

(Maybe you'll tell me it puts a Schreyer order on even the first free module.)

In any case, I think what this means is that when we give the user a resolution
C of a module M, we are failing to give the user the map M <--- C_0 that comes
with the resolution!  Is that change of basis matrix inside the resolution
object?  I should put it somewhere the user can get it.

-----------------------------------------------------------------------------

Here is a simpler example:

    i1 : R = QQ[x,y]

    o1 = R

    o1 : PolynomialRing

    i11 : g = matrix {{x^3,x^2},{y^3,y^2}}

    o11 = | x3 x2 |
	  | y3 y2 |

		  2       2
    o11 : Matrix R  <--- R

    i12 : C = res image g

	   2
    o12 = R  <-- 0

	  0      1

    o12 : ChainComplex

    i13 : degrees C_0

    o13 = {{2}, {3}}

    o13 : List

    i14 : degrees source g

    o14 = {{3}, {2}}

    o14 : List

-----------------------------------------------------------------------------

Here is an example:

    i32 : M = coker matrix {{1_R},{0}}

    o32 = cokernel | 1 |
		   | 0 |

				 2
    o32 : R-module, quotient of R

    i33 : HH_0 res M

    o33 = cokernel | 0 |
		   | 1 |

				 2
    o33 : R-module, quotient of R

    i34 : describe R

    o34 = QQ [x]

