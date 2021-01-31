--From: "Paul S. Aspinwall" <psa@cgtp.duke.edu>
--Date: Tue, 13 Mar 2007 15:36:26 -0400
--To: "Daniel R. Grayson" <dan@math.uiuc.edu>
--Subject: Degree of transpose
--
--Dear Dan,
--
--I'm pretty sure I've found a math error in "transpose" (and "dual").
--
--If I take the transpose of a map f:A->B, then I should negate the
--degrees of A and B, but *not* the degrees of f. Right now, Macaulay2
--negates the degrees of a map under transpose (as well as the degrees
--of the target and source).
--
--The following code exhibits the "bug":

R = QQ[x]
m = map(R^{0},R^{0},matrix{{x}},Degree=>1)
assert isHomogeneous m
mt = transpose m
assert isHomogeneous mt
degree mt === degree m
