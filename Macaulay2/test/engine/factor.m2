-- factoring benchmarks

load "raw-util.m2"

-- Example 1

R = polyring(rawZZ(), (symbol x,symbol y,symbol z))
x^2-y*x-13*z^2*x+y^4+z^7+12
f = (x+3*y-14)^15*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
time rawFactor f                                 
          -- answer: ((z7+y4-13xz2+x2-xy+12, x+3y-14), (3, 15))
          -- used 12.32 seconds, improved to 7.78 seconds, should be able to get to 7.01 sec
          -- used 7.114 seconds : Dell Latitude C840, factory/libfac 2.0.5
          -- used 0.114 seconds : Dell Latitude C840, factory/libfac 3.0.0.5 -- improvement factor 62

-- Example 1 reversed

R = polyring(rawZZ(), (symbol z,symbol y,symbol x))
f = (x+3*y-14)^15*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
time rawFactor f
     -- answer: ((3y+x-14, z7+y4-13z2x-yx+x2+12), (15, 3))
     -- used 0.29 seconds, improved to 0.19 seconds
     -- used 0.172 seconds : Dell Latitude C840, factory/libfac 2.0.5
     -- used 0.034 seconds : Dell Latitude C840, factory/libfac 3.0.0.5 -- improvement factor 5

-- Example 2

R = polyring(rawZZ(), 1: symbol x)
f = x^20+13*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
g = x^20+17*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
h = x^20+21*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
F = f*g*h
time rawFactor F
     -- answer: ((1, x20+17x19+7x15+12x12-x10-x8+x4+13x-20, x20+13x19+7x15+12x12-x10-x8+x4+13x-20, x20+21x19+7x15+12x12-x10-x8+x4+13x-20), (1, 1, 1, 1))
     -- used 1.82 seconds, now used 1.45 seconds
     -- used 1.502 seconds : Dell Latitude C840, factory/libfac 2.0.5
     -- used 0.115 seconds : Dell Latitude C840, factory/libfac 3.0.0.5 -- improvement factor 13

-- Local Variables:
-- compile-command: "echo 'input \"factor.m2\"' | M2 -q --stop"
-- End:
