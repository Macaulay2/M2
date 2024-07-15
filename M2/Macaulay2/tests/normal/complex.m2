-- Test of the various cases for sqrt of a complex number
-- (in default precision)

-- test multiplication --
debug Core
testmult = (a,b) -> (
     a1 = rawFromNumber(raw(CC_53),a);
     b1 = rawFromNumber(raw(CC_53),b);
     abs(rawToCC (a1*b1) - a*b)
     )
assert(testmult(1+ii,2+ii) < 1e-15)
assert(testmult(2+ii,1+ii) < 1e-15)
assert(testmult(1+2*ii,1-2*ii) < 1e-15)
assert(testmult(2*ii,1-2*ii) < 1e-15)
assert(testmult(3+2*ii,1-2*ii) < 1e-15)
assert(testmult(20+2*ii,-1-2*ii) < 1e-15)
assert(testmult(-1-2*ii,20+2*ii) < 1e-15)
assert(testmult(0.0+0.0*ii,20+2*ii) < 1e-15)


-- test division --
debug Core
testdiv = (a,b) -> (
     a1 = rawFromNumber(raw(CC_53),a);
     b1 = rawFromNumber(raw(CC_53),b);
     abs(rawToCC (a1//b1) - a/b)
     )
assert(testdiv(1+ii,2+ii) < 1e-15)
assert(testdiv(2+ii,1+ii) < 1e-15)
assert(testdiv(1+2*ii,1-2*ii) < 1e-15)
assert(testdiv(2*ii,1-2*ii) < 1e-15)
assert(testdiv(3+2*ii,1-2*ii) < 1e-15)
assert(testdiv(20+2*ii,-1-2*ii) < 1e-15)
assert(testdiv(-1-2*ii,20+2*ii) < 1e-15)

-- test abs --
a = 1+ii
b = abs(a)
assert(b == sqrt(2))

a = -1+ii
b = abs(a)
assert(b == sqrt(2))

a = 1-ii
b = abs(a)
assert(b == sqrt(2))

a = -1-ii
b = abs(a)
assert(b == sqrt(2))

a = 1+0*ii
b = abs(a)
assert(b == 1)

a = -1+0*ii
b = abs(a)
assert(b == 1)

a = 0.0+ii
b = abs(a)
assert(b == 1)

a = 0.0-ii
b = abs(a)
assert(b == 1)

-- test sqrt --
a = 1+ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -1+ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 1-ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -1-ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 1+5*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -1+5*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 1-5*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -1-5*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 5+ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 5-ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -5+ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -5-ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 5*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -5*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 1.0+0.0*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -1+0.0*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 1.0*ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = 5-ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -5+ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

a = -5-ii
b = sqrt(a)
assert(abs(b^2-a) < 1e-14)
assert(b >= 0)

assert(log1p ii == log(1 + ii))
assert(expm1 ii == exp ii - 1)
