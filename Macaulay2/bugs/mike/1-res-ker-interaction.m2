-- Date: Thu, 17 May 2007 15:40:47 -0500
-- From: "Daniel R. Grayson" <dan@math.uiuc.edu>
-- To: mike@math.cornell.edu
-- CC: dan@math.uiuc.edu, snapp@math.uiuc.edu
-- Subject: bug in res
-- Reply-to: dan@math.uiuc.edu
-- 
-- 
-- Bart reported the (serious) bug that appears in the last line below.  I can't
-- figure it out.
-- 
    Macaulay 2, version 0.9.96
    with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone

    i1 : A = ZZ/101[x]/x^2

    o1 = A

    o1 : QuotientRing

    i2 : F = res(coker vars A, LengthLimit => 5 )

	  1      1      1      1      1      1
    o2 = A  <-- A  <-- A  <-- A  <-- A  <-- A

	 0      1      2      3      4      5

    o2 : ChainComplex

    i3 : peek F.dd_3

    o3 = Matrix{cache => CacheTable{}}
		RawMatrix => x 

		ring => A
			   1
		source => A
			   1
		target => A

    i4 : syz gb (matrix entries F.dd_3, Syzygies => true)

    o4 = {1} | x |

		 1       1
    o4 : Matrix A  <--- A

    i5 : syz gb (F.dd_3, Syzygies => true)

    o5 = 0

		 1
    o5 : Matrix A  <--- 0

