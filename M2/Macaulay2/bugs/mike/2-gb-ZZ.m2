This gb is not sufficiently autoreduced, because the off-diagonal element -5
could be reduced further in size by the 3 on the diagonal.  In fact, to ensure
uniqueness of result, the off-diagonal elements should all be non-negative, too,
as in Hermite normal form, i.e., it should be the same.

    i15 : gens gb matrix {{-6, -3, 7}, {5, 3, -1}, {-2, -6, 4}}

    o15 = | 25 -10 6  |
	  | 0  3   -5 |
	  | 0  0   2  |

		   3        3
    o15 : Matrix ZZ  <--- ZZ
