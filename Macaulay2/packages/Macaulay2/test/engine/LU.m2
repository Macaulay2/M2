------------------------------
-- Test of LU decomposition --
------------------------------
permutationMatrix = (p) -> (
     M := mutableMatrix(ZZ, #p, #p, Dense=>false);
     for i from 0 to #p-1 do
       M_(i,p#i) = 1;
     matrix M)

M = matrix {{1.0, 3.0, 4.0, 5.0},{2.0, 3.0, 0.0, 1.0}}
(P,L,U) = LU mutableMatrix(M, Dense=>true)
permutationMatrix {0,2,1}
assert(0 == (permutationMatrix P) * (matrix L) * (matrix U) - M)


time m = mutableMatrix(random(RR^7, RR^8), Dense=>true)
time (P,L,U) = LU m;

collectGarbage()

assert(0 == (permutationMatrix P) * (matrix L) * (matrix U) - (matrix m) )

time m = mutableMatrix(random(RR^40, RR^100), Dense=>true);
time (P,L,U) = LU m;

collectGarbage()

(permutationMatrix P) * (matrix L) * (matrix U) - (matrix m) 

time m = mutableMatrix(random(RR^600, RR^600), Dense=>true);
time randomMutableMatrix
time (P,L,U) = LU m;

-- Over ZZ/p
K = ZZ/7
M = matrix(K, {{1,2,3,4},{0,3,1,2}})
(P,L,U) = LU mutableMatrix(M,Dense=>true)
assert(0 == (permutationMatrix P) * (matrix L) * (matrix U) - M )
rowSwap(U,0,1)
U
(P,L,U2) = LU mutableMatrix(U,Dense=>true)
assert(0 == (permutationMatrix P) * (matrix L) * (matrix U2) - matrix U )

-- over QQ
R = QQ
m = matrix(R, {{1,2,3,4,5},
	  {2,3,4,5,1},
	  {3,4,5,1,2},
	  {4,5,1,2,3},
	  {5,1,2,3,4}})
m1 = mutableMatrix(m, Dense=>false)
print "LU over QQ not defined yet"
--(P,L,U) = LU m1

R = ZZ/32003
m = matrix(R, {{1,2,3,4,5},
	  {2,3,4,5,1},
	  {3,4,5,1,2},
	  {4,5,1,2,3},
	  {5,1,2,3,4}})
m1 = mutableMatrix(m, Dense=>true)
(P,L,U) = LU m1
assert(0 == (permutationMatrix P) * (matrix L) * (matrix U) - m )

print "rawFFLU seems to be non-functional? next part commented out"
///
debug Core
rawFFLU raw m1
m1

debug Core
R = ZZ
m = matrix(R, {{1,2,3,4,5},
	  {2,3,4,5,1},
	  {3,4,5,1,2},
	  {4,5,1,2,3},
	  {5,1,2,3,4}})
m1 = mutableMatrix(m, Dense=>false)
rawFFLU raw m1
m1
det matrix m1
det m
factor det m

R = ZZ[vars(0..8)]
m = genericMatrix(R,a,3,3)
m1 = mutableMatrix m
rawFFLU raw m1
m1
///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine LU.out"
-- End:

