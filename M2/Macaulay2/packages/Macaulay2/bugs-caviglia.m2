-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"
-----------------------------------------------------------------------------------
i1 : R=ZZ/101[a..c];

i2 : I=intersect(ideal(a,b),ideal(b,c),ideal(a,c),ideal(a^2,b^2,c^2))

               2   2      2     2   2    2
o2 = ideal (b*c , b c, a*c , a*b , a c, a b)

o2 : Ideal of R

i3 : topComponents I

o3 = ideal (b*c, a*c, a*b)

o3 : Ideal of R

i4 : topComponents (R^1/I)

o4 = cokernel | 1 |  --<--------PROBLEM with -topComponents- applyed to a module. 

                            1
o4 : R-module, quotient of R
------------------------------------------------------------------