permutationMatrix = (p) -> id_(ZZ^#p)^p
M = matrix {{1.0, 3.0, 4.0, 5.0},{2.0, 3.0, 0.0, 1.0}}
M = mutableMatrix(M, Dense=>true)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)

-- over a prime finite field
kk = ZZ/101
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)

-- over a nonprime finite field.  Not available currently!!
{*
kk = GF(8)
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)
*}

-- over QQ.  Not available currently!
kk = QQ
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
assert(0 == permutationMatrix P * matrix L * matrix U - matrix M)
