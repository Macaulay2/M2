Date: Fri, 7 Jul 2006 11:48:52 -0500 (CDT)
From: Dan Grayson <dan@math.uiuc.edu>
To: mike@math.cornell.edu
CC: dan@math.uiuc.edu
Subject: ZZ[x,y]
Reply-to: dan@math.uiuc.edu


An idle question for an idle moment:

Given a ZZ[x,y]-module, how do we compute the primes of ZZ over which it is not
flat?

Here is an example of one that is not flat over 2.

    i1 : R = ZZ[x,y]

    o1 = R

    o1 : PolynomialRing

    i2 : f = random(R^2,R^{4:-2})

    o2 = | -9x2-9xy    4x2-10xy-4y2 9x2-5xy-6y2 -7x2+4xy-3y2 |
	 | 7x2+8xy-9y2 6x2-2y2      4x2-4xy+3y2 -9x2-4xy+7y2 |

		 2       4
    o2 : Matrix R  <--- R

    i3 : gbTrace = 3

    o3 = 3

    i4 : C = res coker f

	  2      4      4      2
    o4 = R  <-- R  <-- R  <-- R  <-- 0

	 0      1      2      3      4

    o4 : ChainComplex

    i16 : S = ZZ/2[t,u]

    o16 = S

    o16 : PolynomialRing

    i17 : f = map(S,R,vars S)

    o17 = map(S,R,{t, u})

    o17 : RingMap S <--- R

    i19 : prune HH f C

    o19 = 0 : cokernel | 0     u2    t2 |
		       | tu+u2 t2+u2 0  |

	  1 : cokernel {2} | tu+u2 t2+u2 |

	  2 : 0

	  3 : 0

	  4 : 0

    o19 : GradedModule

