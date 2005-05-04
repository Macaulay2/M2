-- Place bugs that you find in here, and
-- then check in the file, using (outside of M2):
--   cvs ci -m "another bug"

There is something funny with hilbertSeries of projectiveHilbertPolynomial. 
See its documentation for a comment on what I was trying to do.

There is also something wrong with reduceHilbert in this case:

i52 : P = (projectiveHilbertPolynomial 0) +  6 * (projectiveHilbertPolynomial 1) + 12 * (projectiveHilbertPolynomial 2) + 8 * (projectiveHilbertPolynomial 3) 

o52 = P  + 6*P  + 12*P  + 8*P
       0      1       2      3

o52 : ProjectiveHilbertPolynomial

i53 : hilbertSeries P

                   2    3
      27 - 27T + 9T  - T
o53 = -------------------
                   4
            (1 - T)

o53 : Divide

i54 : reduceHilbert oo
stdio:63:1:(1): key not found in hash table

i55 : reduceHilbert o53
stdio:64:1:(1): key not found in hash table
