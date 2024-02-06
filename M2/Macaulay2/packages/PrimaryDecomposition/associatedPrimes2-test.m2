-- Date: Thu, 27 Feb 2003 15:40:55 -0500 (EST)
-- From: Greg Smith <ggsmith@cpw.math.columbia.edu>
-- To: Mike Stillman <mike@polygon.math.cornell.edu>,
--    Dan Grayson <dan@math.uiuc.edu>
-- cc: jsidman@math.berkeley.edu
-- Subject: Bug report.
-- 
-- 
-- There appears to a bug in the Assassinator code.
-- In particular, "associatedPrimes P" doesn't return all the
-- associated primes in the following example.
-- 
S = QQ[a,b,c,d,e];
I1 = ideal(a,b,c);
I2 = ideal(a,b,d);
I3 = ideal(a,e);
P = I1*I2*I3;
L1 = associatedPrimes P;
L2 = apply(associatedPrimes monomialIdeal P, J -> ideal J);
assert( #L1 == #L2 )
-- 
-- Naturally, I'm happy to observe that the monomial
-- ideal code does return the correct answer.
-- 
-- Cheers,
--   Greg.
-- 
