R = QQ[x]
f = leadTerm matrix (R,{{2/1},{1/2}})
g = leadTerm matrix {{2/1},{1/2}}
assert ( f == g ** R )

end

-- In-Reply-To: <200607071226.k67CQRB01053@u00.math.uiuc.edu>
-- From: Michael Stillman <mike@math.cornell.edu>
-- Subject: Re: mon order on fields
-- Date: Fri, 7 Jul 2006 08:46:24 -0400
-- To: dan@math.uiuc.edu
-- 
-- This must be a bug.  Let me look at it...
-- 
-- On Jul 7, 2006, at 8:26 AM, Dan Grayson wrote:
-- 
-- >
-- > Fields seem to be Position => Up and polynomial rings seem to be  
-- > Position =>
-- > Down.  Is that intentional?  Is it worth correcting that discrepancy?
-- >
-- >     i1 : R = QQ[x]
-- >
-- >     o1 = R
-- >
-- >     o1 : PolynomialRing
-- >
-- >     i2 : leadTerm matrix (R,{{2/1},{1/2}})
-- >
-- >     o2 = | 2 |
-- > 	 | 0 |
-- >
-- > 		 2       1
-- >     o2 : Matrix R  <--- R
-- >
-- >
-- >     i41 : leadTerm matrix {{2/1},{1/2}}
-- >
-- >     o41 = | 0   |
-- > 	  | 1/2 |
-- >
-- > 		   2        1
-- >     o41 : Matrix QQ  <--- QQ
-- 
