-- From email from Josephine Yu, 10-27-09
M = matrix {{1.}} -- over RR
MM = mutableMatrix M
rowMult(MM,0,3);
M  -- becomes matrix {{1.5}}
MM -- correct
assert(M == matrix {{1.}})
assert(matrix MM == matrix mutableMatrix {{3.}})

-- sparse matrices are OK
M = matrix {{1.}} -- over RR
MM = mutableMatrix(M, Dense=>false)
rowMult(MM,0,3);
M  -- becomes matrix {{1.5}}
MM -- correct
assert(M == matrix {{1.}})
assert(matrix MM == matrix mutableMatrix {{3.}})

-- dense matrices are not
M = matrix {{1.}} -- over RR
MM = mutableMatrix(M, Dense=>true)
rowMult(MM,0,3);
M  -- becomes matrix {{1.5}}
MM -- correct
assert(M == matrix {{1.}})
assert(matrix MM == matrix mutableMatrix {{3.}})


--- Over CC
-- sparse matrices over CC
M = matrix {{1.+ii}} -- over CC
MM = mutableMatrix(M, Dense=>false) 
rowMult(MM,0,3);
M  -- used to become matrix {{1.5}}
MM -- was wrong at one time.
assert(M == matrix {{1.+ii}})
assert(matrix MM == matrix mutableMatrix {{3.+3.*ii}})

-- sparse matrices over CC
M = matrix {{1.+ii}} -- over CC
MM = mutableMatrix(M, Dense=>false) 
rowMult(MM,0,3.+0.0*ii); -- wrong!!
M  -- ok
assert(M == matrix {{1.+ii}})
assert(matrix MM == matrix mutableMatrix {{3.+3.*ii}})



-- dense matrices are not
M = matrix {{1.}} -- over RR
MM = mutableMatrix(M, Dense=>true)
rowMult(MM,0,3);
M  -- becomes matrix {{1.5}}
MM -- correct
assert(M == matrix {{1.}})
assert(matrix MM == matrix mutableMatrix {{3.}})
-------------------------


M = matrix {{1}}  -- over ZZ
MM = mutableMatrix M
rowMult(MM,0,3);
M  -- still {{1}}
MM -- also ok
assert(M == matrix {{1}})
assert(matrix MM == matrix mutableMatrix {{3}})


M = matrix {{1.}}
MM = new MutableMatrix from M
rowMult(MM,0,3);
M  -- still {{1}}
MM
assert(M == matrix {{1.}})
assert(matrix MM == matrix mutableMatrix {{3.}})

R = ZZ/101[x]
M = matrix {{1_R}}
MM = mutableMatrix M
rowMult(MM,0,x);
M  -- still {{1}}
MM -- also ok
assert(M == matrix {{1_R}})
assert(matrix MM == matrix mutableMatrix {{x}})
