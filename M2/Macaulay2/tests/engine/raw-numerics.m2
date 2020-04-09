-- Test of numeric rings

needs "raw-util.m2"

-- new ZZ from RawRingElement -- rawToInteger
-- new QQ from RawRingElement -- rawToRational
-- new RR from RawRingElement -- rawToRR
-- new CC from RawRingElement -- rawToCC -- WRITE THIS

C = CC_53
a = 1+3*ii
b = rawFromNumber(raw C,a)
c = rawToCC b
assert(a === c)

-- also need: rawImaginaryPart, rawRealPart, rawFromReals(a,b) gives a+b*ii
-- need promote and lift to work between these rings?

R = RR_53
b = new RR from .234
b = new RR from rawFromNumber(raw R, .234)  -- FAILS
new RR from RawRingElement := (R, f) -> rawToRR f
b = new RR from rawFromNumber(raw R, .234)  -- now it works

new QQ from 2/3
print "The following line is WRONG"
rawFromNumber(raw QQ, 2/3) -- incorrect
m = matrix{{2/3}}
m_(0,0)
a = rawMatrixEntry(raw m,0,0)
rawToRational a
oo === 2/3

new RR from 1.234


new RR from RawRingElement := (R, f) -> new R from { symbol RawRingElement => f };

a = new ZZ from 5245242
b = new ZZ from rawFromNumber(raw ZZ, 23213123897897987897987)
raw a -- 
raw b -- OK

b = new ZZ from rawFromNumber(raw ZZ, 2/3) -- gives 0
b = new ZZ from rawFromNumber(raw ZZ, 2/1) -- gives 0
rawLift(raw ZZ,rawFromNumber(raw QQ, 2/1)) -- FAILS

a = new QQ from 2/3
b = new QQ from rawFromNumber(raw QQ, 2/3) -- WRONG
raw a
raw b

a = new RR from 2/3 -- FAILS
b = new RR from rawFromNumber(raw R, 2/3)
b
a = (2/3)_RR
rawToRR rawFromNumber(raw R, .234567)
raw a
raw b

