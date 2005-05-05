-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

(1) add the following

    isIdeal Ideal := x -> true
    isIdeal MonomialIdeal := x -> true 
    
(2) the function isAffineRing isn't eliminating enough rings.  Prehaps

isAffineRing PolynomialRing := R -> (
     not (options R).SkewCommutative and not (options R).Inverses and
     isAffineRing coefficientRing R
     )
      
should be

isAffineRing PolynomialRing := R -> (
     isCommutative R and not (options R).Inverses and
     isAffineRing coefficientRing R)

What about local rings?
