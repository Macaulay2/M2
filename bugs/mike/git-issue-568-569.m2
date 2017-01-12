-- git issue #569
    R=QQ{x,y,z}
    i=ideal"xyz+z5,2x2+y3+z7,3z5+y5"
    transpose gens gb i -- this is not minimal GB...  BUG!!

-- git issue #568
R = (ZZ/31991){x,y}
  I = ideal"x2-x3,x4"
  mingens I -- WRONG!!
  assert(numColumns mingens I == 1)
  
-- investigation of #569
f1 = 1/2 * (2*x^2+y^3+z^7) -- ecart 5.
f2 = x*y*z+z^5 -- ecart 2
f3 = y^5+3*z^5 -- ecart 0
f4 = -2*(x*f2 - 1/2*y*z*f1) -- ecart 5
  f4 = f4 + y*z*f1
f5 = 2 * (x^2*f3 - 1/2*y^5*f1)
  f5 - y^5 * f1 - 6*z^5*f1 + y^3*f3 + z^7*f3 == 0
f6 = 1/3 * (x*z*f3 - y^4*f2)

netList {f1,f2,f4,f3,f6,f7}

x*f4-y^3*f2 + 2*z^5*f1 - z^7*f2 == 0
f7 = -1/3 * (y*f4-z*f3)
f6-x*f7 + 2/3*y*z^5*f1 -1/3*y*z^7*f2 == 0
netList {f1,f2,f4,f3,f7}

z^7 - z*f7 + 2/3*x*y*f7-4/9*y^2*z^5*f1 + 2/9*z^5*f3
