-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"
Grassmannian with rings doesn't work.
Specifically:
R = QQ[a..f];
Grassmannian(1,3,R);
returns 
stdio:9:5:(2): no method found for applying coefficients to:
argument 1 :  (0, 1) (of class Sequence)
argument 2 :  0 (of class Matrix)
Grassmannian(1,3) doesn't work either.