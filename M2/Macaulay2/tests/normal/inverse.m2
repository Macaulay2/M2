
-- Also in the included file is some strange behavior of
-- units in a ring I was doing some matrix computations in.
-- In this case, R = ZZ/101[x,y,z,w]/ideal{x*y,z*w}, and
-- inputting (2_R)^(-1) gives a result of 2, even though
-- its inverse in R is -50. Performing lift(1_R/2_R, R)
-- does indeed give -50, and so I did this temporarily,
-- but I thought you may want to know about this problem too.
-- 
-- Thanks,
-- 
-- Frank Moore
-- UNL Math Dept

R = ZZ/101[x,y,z,w]/ideal{x*y,z*w}
assert ( 2 * 2_R^-1 == 1 )

k = ZZ/101
assert(2_k^-1 == -50)
R=k[x]
assert(2_R^-1 == -50)
S = ZZ/101[x,y,z,w]/ideal{x*y,z*w}
assert(2_S^-1 == -50)

assert(1//x == 0)
assert try x^-1 else true

F = matrix {{1.,2,3},{4,6,9},{2,4,8}}
F * F^-1 == 1.

--- bug found in inverse, originally noticed in a Schubert2 computation.
--- this is from github issue #2051 (May 2021)
  kk = ZZ/32003
  A = kk[][a,b,c,d,e, MonomialOrder => {3,2}, Degrees => {1,2,3,1,2}, Join => false]
  I1 = ideal(-a-d, -b-a*d-e, -c-b*d-a*e, -c*d - b*e, -c*e)
  B = A/I1
  I2 = ideal(2*d^2-5*e)
  C = B/I2
  D = C[f,g,h, MonomialOrder => {2,1}, Degrees => {1,2,1}, Join => false]
  I3 = ideal(-f-h-3*d, -g-f*h+15/2 * e, -g*h-5/2*d*e)
  E = D/I3
  -- these asserts were originally to make sure I got the correct ideals.
  assert isHomogeneous I1
  assert isHomogeneous I2
  assert isHomogeneous I3
  assert isHomogeneous A
  assert isHomogeneous B
  assert isHomogeneous C
  assert isHomogeneous D
  assert isHomogeneous E

  -- The actual test.    
  td = 1 - 1/2*h + 1/12*h^2 + (-1/48*e*h^2 - 1/36*d*e*h)
  assert(td * td^-1 == 1)

  (F, rf) = flattenRing E
  td1 = rf td
  assert(td1^-1 * td1 == 1)
  assert(td1^-1 == rf td^-1)
