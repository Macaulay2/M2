-- Test of the routines for mutable matrices

-- Test: 
--   rawMutableIdentity
--   rawMutableMatrix (2 forms)
--   rawNumberOfRows, rawNumberOfColumns
--   rawMatrixEntry (2 forms for mutable matrices)
--   rawMatrixRowSwap, rawMatrixColSwap
--   rawMatrixRowChange, rawMatrixColChange
--   rawMatrixRowScale, rawMatrixColumnScale
needs "raw-util.m2"
R = ZZ[vars(0..11)]
id10 = rawMutableIdentity(raw R, 10, true); net id10
m = genericMatrix(R,a,3,4)
p = rawMutableMatrix(raw m,true)
assert(rawNumberOfColumns p == 4)
assert(rawNumberOfRows p == 3)

assert(rawColumnDotProduct(p, 1, 1) == raw(d^2+e^2+f^2))
assert(rawColumnDotProduct(p, 1,2) == raw(d*g+e*h+f*i))

assert(raw m === rawMatrix p)
rawMatrixRowSwap(p,0,1); net p
assert(rawMatrix p - raw transpose matrix{{b,a,c},{e,d,f},{h,g,i},{k,j,l}} == 0)
rawMatrixColumnSwap(p,1,2); net p
assert(rawMatrix p == raw matrix{{b,h,e,k},{a,g,d,j},{c,i,f,l}})
net p
rawMatrixRowScale(p, raw (5_R), 2, false); net p
rawMatrixColumnScale(p, raw (7_R), 1, false); net p
rawMatrixRowChange(p, 0, raw c, 1, false); net p
rawMatrixColumnChange(p, 1, raw d, 0, false); net p


p = rawMutableMatrix(raw R, 3,4,true)
rawMatrixEntry(p, 1,3, raw(a+b))
net toString p
p1 = rawMutableMatrix (raw m, true)
raw m === rawMatrix p1
p = rawMutableMatrix(raw ((transpose m) * m), true)
net toString p

p = rawMutableMatrix(raw R, 10, 20, true); net p      
scan(50, i -> (rawMatrixEntry(p,random 10, random 20, raw (random 100)_R)))
net p

map(R, rawMatrix p)

rawMatrixEntry(p,0,0)
rawMatrixEntry(p,0,1)

rawMutableIdentity(raw R, 10, true)

--------------------------------------------
-- Test of mutable dense matrices over RR --
--------------------------------------------
needs "raw-util.m2"
p = rawMutableIdentity(raw RR, 10, true)
rawMatrixEntry(p,3,5,rawFromNumber(raw RR, 3.5))
net p

p = rawMutableMatrix(raw RR, 10, 20, true); net p      
scan(50, i -> (rawMatrixEntry(p,random 10, random 20, rawFromNumber(raw RR, random 1.0))))
net p
map(RR,rawMatrix p)

--------------------------------------------
-- Test of mutable sparse matrices ---------
--------------------------------------------
needs "raw-util.m2"
R = ZZ[a..d]
p = rawMutableIdentity(raw R, 10, false)
net p
rawMatrixEntry(p,1,3,raw(a+b))
net p
