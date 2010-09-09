-- This was fixed sometime earlier (8 Sep 2010 MES)

This result is not in Hermite normal form, because the off-diagonal element
-160 is not between 0 and 150-1.

    i14 : hermite matrix {{-6, -3, 7}, {5, 3, -1}, {-2, -6, 4}}

    o14 = | 0   0    1  |
	  | 0   1    0  |
	  | 150 -160 42 |

		   3        3
    o14 : Matrix ZZ  <--- ZZ
