-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

(1) add the following

    isIdeal Ideal := x -> true
    isIdeal MonomialIdeal := x -> true 
    
(2) the function isAffineRing isn't eliminating enough rings.  Prehaps

isAffineRing PolynomialRing := R -> (
     not R.?SkewCommutative and not (options R).Inverses and
     isAffineRing coefficientRing R
     )
      
should be

isAffineRing PolynomialRing := R -> (
     isCommutative R and not (options R).Inverses and
     isAffineRing coefficientRing R)

What about local rings?

(3) Cannot factor over GaloisField's;  output can be very wrong:

     GF(9)[t];
     factor(t^2-1)

(4) Some ideal operators over ZZ fail.  For example

    ideal(12,18) == ideal(6)
    
(5) Should (ideal,Module) use isIdeal?

BUG 1/13/2006
R = QQ[a,b,c,d, Degrees=>{2:{1,0},2:{0,1}}]
I = intersect(ideal(a,b),ideal(c,d))
res I  -- crashses.  Too bad.

-- another bug
R = QQ[a,b,c,d, Degrees=>{2:{1,0},2:{0,1}},Heft=>{1,1}]
I = intersect(ideal(a,b),ideal(c,d))
