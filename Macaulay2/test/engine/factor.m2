-- factoring benchmarks

load "raw-util.m2"

-- Example 1

R = polyring(rawZZ(), (symbol x,symbol y,symbol z))
x^2-y*x-13*z^2*x+y^4+z^7+12
f = (x+3*y-14)^15*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
time rawFactor f                                 -- used 12.32 seconds, improved to 7.78 seconds, should be able to get to 7.01 sec

-- Example 1 reversed

R = polyring(rawZZ(), (symbol z,symbol y,symbol x))
f = (x+3*y-14)^15*(x^2+y^4+z^7-x*y-13*x*z^2+12)^3;
time rawFactor f                                  -- used 0.29 seconds, improved to 0.19 seconds

-- Example 2

R = polyring(rawZZ(), 1: symbol x)
f = x^20+13*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
g = x^20+17*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
h = x^20+21*x^19+7*x^15+12*x^12-x^10-x^8+x^4+13*x-20
F = f*g*h
time rawFactor F                                  -- used 1.82 seconds, now used 1.45 seconds

-- Local Variables:
-- compile-command: "echo 'input \"factor.m2\"' | M2 -q --stop"
-- End:
