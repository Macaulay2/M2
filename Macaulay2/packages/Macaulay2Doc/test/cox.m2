-- Date: Wed, 27 Aug 2003 14:14:09 -0400
-- From: David Cox <dac@cs.amherst.edu>
-- To: dan@math.uiuc.edu
-- CC: David Cox <dac@cs.amherst.edu>
-- Subject: Re: monomial orders for Macaulay 2
-- In-Reply-To: <200308271730.h7RHUpl07280@u00.math.uiuc.edu>
-- Content-Type: text/plain; charset=ISO-8859-1; format=flowed
-- Content-Transfer-Encoding: 7bit
-- 
-- Dear Dan: Up and Down are great.  The example we do using Macaulay 2 is 
-- the following very simple computation:
--  
R = QQ[a..d, MonomialOrder => {Position => Down}]

M = matrix{{a^2+b^2, a^3 - 2*b*c*d, a - b},{c^2 - d^2, b^3 + a*c*d, c+d}}

gens gb M
-- 
-- The answer appears as equation (2.10) on page 205 of Using Algebraic 
-- Geometry.  We also do this example in Singular (and, in the 2nd edition, 
-- also in Maple).  In all three computations, we use TOP downward grevlex. 
--   It would be wonderful if you could check this in Macaulay 2.  
--     Many thanks, David
-- 

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test cox.out "
-- End:
