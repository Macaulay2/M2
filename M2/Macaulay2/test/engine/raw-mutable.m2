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
m = genericMatrix(R,a,3,4)
p = rawMutableMatrix(raw m,true)
assert(raw m === rawMatrix p)
rawMatrixRowSwap(p,0,1); net p
assert(rawMatrix p - raw transpose matrix{{b,a,c},{e,d,f},{h,g,i},{k,j,l}} == 0)
rawMatrixColumnSwap(p,1,2); net p
assert(rawMatrix p == raw matrix{{b,h,e,k},{a,g,d,j},{c,i,f,l}})
net p
rawMatrixRowScale(p, raw (5_R), 2, false)
net p
rawMatrixColumnScale(p, raw (7_R), 1, false) -- WRONG
net p
rawMatrixRowChange(p, 0, raw c, 1, false)
net p
rawMatrixColumnChange(p, 1, raw d, 0, false)
net p

p = rawMutableMatrix(raw R, 3,4,true)
rawMatrixEntry(p, 1,3, raw(a+b))
net toString p
p1 = rawMutableMatrix (raw m, true)
raw m === rawMatrix p1
p = rawMutableMatrix(raw ((transpose m) * m), true)
net toString p
      
