-- Test of reducing a matrix, or a resolution, via pivots

restart
R = ZZ/32003[a..f]
I = ideal(a*b*c-d*f, a^2*e^2-b*c, a*d*f-b*c*e^2)
C = res I
C.dd
M1 = mutableMatrix C.dd_1
M2 = mutableMatrix C.dd_2
M3 = mutableMatrix C.dd_3

debug Core
rawReduceByPivots raw M3
rawReduceByPivots raw M2
M3
M2
rawReduceByPivots raw M1
M1

-- Prune the resolution:
columnSwap(M3, 0,1)
rowSwap(M3,4,6)
columnSwap(M2,4,6)
assert((matrix M2) * (matrix M3) == 0)
columnAdd(M3, 0, -e^2, 1)
columnAdd(M3, 0, -a, 2)
M2
-- now work on M2 (but make changes to M3 and M1 as needed
columnSwap(M2, 1, 4)
columnSwap(M2, 0, 3)
rowSwap(M3, 1, 4)
rowSwap(M3, 0, 3)

columnAdd(M2, 0, a, 4)
columnAdd(M2, 1, -e^2, 4)
columnAdd(M2, 1, a*e^2, 3)
columnAdd(M2, 2, -b*c, 3)
rowAdd(M3, 4, -a, 0)
rowAdd(M3, 4, e^2, 1)
rowAdd(M3, 3, -a*e^2, 1)
rowAdd(M3, 3, b*c, 2)

P1 = map(R^1,,matrix submatrix(M1,,{0,1,2}))
P2 = map(source P1,,matrix submatrix(M2,{0,1,2},{0,1,2}))
P3 = map(source P2,,matrix submatrix(M3,{0,1,2},{0}))
chainComplex{matrix P1, matrix P2, matrix P3}
