
debug Core
R = rawARingZZFlint()
a = 1_R
b = 2_R
rawRing a
a+b
a-b
a^3
a = 23746237846237846237846237846237846327846178623418764
a^4
-a
a*b
b*b
b^2
M = rawMutableMatrix(R, 3, 4, true)
rawMutableMatrixFillRandomDensity(M,1.0,0);
M

N = rawMutableMatrix(R, 4, 5, true)
rawMutableMatrixFillRandomDensity(N,1.0,0);
N
rawLinAlgMult(M,N)

E = {{99, 76, 12}, {78, 28, 96}, {43, 7, 45}}
det matrix E
M = rawMutableMatrix(R, 3, 3, true)
for i from 0 to 2 do for j from 0 to 2 do M_(i,j) = (E#i#j)_R
M
rawLinAlgDeterminant M == 97284
rawLinAlgMult(M,M)

M = rawMutableMatrix(R, 1000, 1000, true);
rawMutableMatrixFillRandomDensity(M,1.0,0);
M
time rawLinAlgMult(M,M);
rawLinAlgDeterminant M

fillMatrix M
R101 = rawARingZZpFlint(101)
select(2^36+1 .. 2^36 + 500, isPrime)
P = 68719476767
R = rawARingZZpFlint P
a = 10_R
a^(P-1)
a^(P^2-1)
a^(P^10)

select(2^64-500 .. 2^64-1, isPrime
select(2^24-500 .. 2^24-1, isPrime)
last oo
P = 18446744073709551557
-- P = 18,446,744,073,709,551,557
R = rawARingZZpFlint P
char R
random R
rawRandom R
R = ZZ/32003
random R

debug Core
R = ZZp(P, "Choose"=>"FLINT")
1_R + (-1_R)
M = 342342342342353242
N = 986908456830938608
M_R
N_R
M_R * N_R == (M*N)_R
M*N

M = mutableMatrix(R,4,5)
fillMatrix M
M_(2,3)
entries M
N1 = matrix M
N2 = transpose N1
N1*N2
det oo
det(N2*N1) -- no good yet.

R1 = ZZp(P, "Choose"=>"FFPACK") -- error, as it should be
R2 = ZZp(P, "Choose"=>"ARING") -- error, as it should be
R3 = ZZp(P) -- error, as it should be
R4 = ZZp(P, "Choose"=>"FLINT")
A = random(R4^4, R4^5)
B = random(R4^5, R4^4)
C = A*B
D = B*A
rank D
det oo
B*A
det oo

R1 = ZZp 101
R2 = ZZp 101
assert(R1 === R2)
R3 = ZZp(101, "Choose"=>"ARING")
R4 = ZZp(101, "Choose"=>"ARING")
assert(R1 =!= R3)
assert(R4 === R3)

restart
debug Core
R = ZZp(101, "Choose"=>"FFPACK")
N = 10
M = mutableMatrix(R, N, 2*N)
fillMatrix M;
time rawLinAlgRank raw M
M = mutableMatrix(R, N, N)
fillMatrix M;
time rawLinAlgDeterminant raw M

R = ZZp(101, "Choose"=>"FLINT")
M = mutableMatrix(R, N, 2*N)
fillMatrix M;
time rawLinAlgRank raw M

P = 18446744073709551557
R = ZZp(P, "Choose"=>"FLINT")
M = mutableMatrix(R, N, 2*N)
fillMatrix M;
time rawLinAlgRank raw M

P = 16777213
R = ZZp(P, "Choose"=>"FFPACK")
N = 2000
M = mutableMatrix(R, N, 2*N)
fillMatrix M;
time rawLinAlgRank raw M

R = ZZp(P, "Choose"=>"FLINT")
N = 2000
M = mutableMatrix(R, N, 2*N)
fillMatrix M;
time rawLinAlgRank raw M


M = mutableMatrix(R, 5, 4)
fillMatrix M
N = mutableMatrix(R,4,5)
fillMatrix N
(matrix N) * (matrix M)
mutableMatrix oo
rawLinAlgRank raw oo
rank M
rank matrix M

TEST /// 
 -- test of small matrix multiplication in flint and ffpack
 restart
 debug Core
 R1 = ZZp(101, "Choose"=>"FFPACK")
 R2 = ZZp(101, "Choose"=>"FLINT")
  E = {{1,4},{7,1}}
  E2 = {{3,34},{17,22}}
  M1 = matrix E
  M2 = matrix E2
  (M1*M2) ** ZZ/101
  ansMult = {{71,21}, {38, 58}}
  
  -- First do ffpack
  M1 = mutableMatrix matrix(R1, E)  
  M2 = mutableMatrix matrix(R1, E2)
  assert(ansMult == entries rawLinAlgMult(raw M1,raw M2))

  -- now flint
  M1 = mutableMatrix matrix(R2, E)
  M2 = mutableMatrix matrix(R2, E2)
  assert(ansMult == entries rawLinAlgMult(raw M1,raw M2))
  assert(M1_(0,1) == 4)
///

TEST ///
  -- timing for matrix mult
  restart
  debug Core
  N = 3
  R1 = ZZp(101, "Choose"=>"FFPACK")
  R2 = ZZp(101, "Choose"=>"FLINT")

  M1 = mutableMatrix(R1, N, 3*N)
  M2 = mutableMatrix(R1, 3*N, N)
  fillMatrix M1;
  fillMatrix M2;
  time rawLinAlgMult(raw M1, raw M2);  -- not working yet...
  time rawLinAlgDeterminant raw M1
  
  M1 = mutableMatrix(R2, N, N)
  M2 = mutableMatrix(R2, N, N)
  fillMatrix M1;
  fillMatrix M2;
  time rawLinAlgMult(raw M1, raw M2);  -- not working yet...
  time rawLinAlgDeterminant raw M1  
  
///
TEST ///
-- Test of determinant
 debug Core
 E = {{86, 13, 36, 39, 39, 88, 7, 0, 66, 86}, {23, 10, 77, 15, 25, 33, 30, 29, 45, 13}, {77, 9, 78, 34, 7, 40, 52, 82, 36, 55}, {66, 100, 92, 27, 87, 97, 32, 6, 96, 29}, {81, 79, 21, 50, 56, 80, 28, 94, 93, 60}, {22, 15, 1, 36, 35, 59, 74, 46, 86, 31}, {82, 83, 98, 40, 19, 11, 14, 78, 29, 94}, {28, 16, 43, 44, 90, 33, 71, 34, 62, 27}, {28, 49, 81, 85, 98, 31, 85, 65, 78, 70}, {1, 33, 4, 92, 17, 17, 69, 27, 40, 30}}
 (det matrix E) % 101

  E = {{1,4},{7,1}}
  E2 = {{3,34},{17,22}}
 R = ZZ/101 
 M = matrix(R, E)  
 M2 = matrix(R,E2)
 det M
 M*M2 

 R = ZZp(101, "Choose"=>"FFPACK")
 M = mutableMatrix matrix(R, E)
 M2 = mutableMatrix matrix(R, E2)
 rawLinAlgDeterminant(raw M)
 rawLinAlgMult(raw M, raw M2)

 R = ZZp(101, "Choose"=>"FLINT")
 M = mutableMatrix matrix(R, E)
 M2 = mutableMatrix matrix(R, E2)
 rawLinAlgDeterminant(raw M)
 rawLinAlgMult(raw M, raw M2)

 N = 4000
 N = 1000
N = 2
 debug Core
 R0 = ZZp(101)
 M = mutableMatrix(R0, N, N)
 fillMatrix M;
 Ma = matrix M;
 time det Ma -- not working

 R1 = ZZp(101, "Choose"=>"FFPACK")
 M = mutableMatrix(R1, N, N)
 fillMatrix M;
 time rawLinAlgDeterminant(raw M);
 time rawLinAlgInvert(raw M) ;
 M2 = mutableMatrix(R1, N, N)
 fillMatrix M2;
 rawLinAlgMult(raw M, raw M2)

 R2 = ZZp(101, "Choose"=>"FLINT")
 M = mutableMatrix(R2, N, N)
 fillMatrix M;
 time rawLinAlgDeterminant(raw M);
 time rawLinAlgInvert(raw M);
 

 P = 18446744073709551557
 R3 = ZZp(P, "Choose"=>"FLINT")
 M = mutableMatrix(R3, N, N)
 fillMatrix M;
 time rawLinAlgDeterminant(raw M);
 time rawLinAlgInvert(raw M) ;
 
  
///