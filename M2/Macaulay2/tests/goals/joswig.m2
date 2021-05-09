-- Date: Sat, 2 Sep 2006 18:14:58 +0200 (CEST)
-- Subject: Groebner basis computation
-- From: "Michael Joswig" <joswig@mathematik.tu-darmstadt.de>
-- To: dan@math.uiuc.edu
-- Content-Type: text/plain;charset=iso-8859-1
-- Content-Transfer-Encoding: 8bit
-- Importance: Normal
-- 
-- Dear Dan,
-- 
-- As we discussed in the morning, I am interested in eliminating
-- systems (in characteristic zero) like
-- 
--    1 + s^2 x1 x3 + s^8 x2 x3 + s^19 x1 x2 x4
--    x1 + s^8 x1 x2 x3 + s^19 x2 x4
--    x2 + s^10 x3 x4 + s^11 x1 x4
--    x3 + s^4 x1 x2 + s^19 x1 x3 x4 + s^24 x2 x3 x4
--    x4 + s^31 x1 x2 x3 x4
-- 
-- for the elimination order x4 > x3 > x2 > x1 > s.  In this particular
-- (test) case the discriminant polynomial in s is of degree 444 and has 2
-- real roots.
-- 
-- Systems of this type (we can produce more, if you like this one)
-- occur in http://front.math.ucdavis.edu/math.CO/0508180.  This paper
-- is to appear in Adv. Math.
-- 
-- With kind regards,
-- Michael
-- 
-- 
-- 
-- 
-- 
-- --
-- Prof. Dr. Michael Joswig <joswig@mathematik.tu-darmstadt.de>
-- TU Darmstadt, Fachbereich Mathematik, AG7
-- 64289 Darmstadt, Germany
-- 

R = QQ[x4,x3,x2,x1,s, MonomialOrder => Eliminate 4]
I = ideal(
    1 + s^2 * x1 * x3 + s^8 * x2 * x3 + s^19 * x1 * x2 * x4,
    x1 + s^8 * x1 * x2 * x3 + s^19 * x2 * x4,
    x2 + s^10 * x3 * x4 + s^11 * x1 * x4,
    x3 + s^4 * x1 * x2 + s^19 * x1 * x3 * x4 + s^24 * x2 * x3 * x4,
    x4 + s^31 * x1 * x2 * x3 * x4
    )
gbTrace = 3
time g = gb I;

