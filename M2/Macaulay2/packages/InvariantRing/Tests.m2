-*
   Copyright 2020, Luigi Ferraro, Federico Galetto,
   Francesca Gandini, Hang Huang, Matthew Mastroeni, Xianglong Ni.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

-------------------------------------------
--- FiniteGroupAction TESTS ---------------
-------------------------------------------

-- Test 0
TEST ///
R = QQ[x_1]
G = finiteAction({matrix{{-1}}}, R)
assert(set group G === set {matrix{{1_QQ}}, matrix{{-1_QQ}}})
assert(isAbelian G)
assert(reynoldsOperator(x_1 + x_1^2, G) === x_1^2)
assert(isInvariant(1 + x_1^4 + x_1^6, G))
assert(not isInvariant(1 + x_1^5 + x_1^4, G))
///

-- Test 1
TEST ///
R = QQ[x_1..x_3]
L = apply(2, i -> permutationMatrix(3, [i + 1, i + 2] ) )
S3 = finiteAction(L,R)
assert(#(group S3) === 6)
assert(not isAbelian S3)
assert(reynoldsOperator(x_1 + x_1*x_2*x_3, S3) === (1/3)*(x_1 + x_2 + x_3) + x_1*x_2*x_3)
assert(isInvariant(1 + x_1 + x_2 + x_3, S3))
assert(not isInvariant(1 + x_1, S3))
///


-------------------------------------------
--- DiagonalAction TESTS -------------
-------------------------------------------

-- The first two tests are of trivial actions, and seem to run into problems. The analogous torusAction test seems to be fine though

-- Test 2
TEST ///
R = QQ[x_1]
T = diagonalAction(matrix{{0}}, {3}, R)
invariants0 = set {R_0}
assert(set invariants T === invariants0)
assert(isInvariant(R_0 + R_0^2, T))
///

-- Test 3
TEST ///
R = QQ[x_1]
T = diagonalAction(matrix{{3}}, {1}, R)
invariants0 = set {R_0}
assert(set invariants T === invariants0)
assert(isInvariant(R_0 + R_0^2, T))
///

-- Test 4
TEST ///
R = QQ[x_1]
T = diagonalAction(matrix{{1}}, {2}, R)
invariants0 = set {R_0^2}
assert(set invariants T === invariants0)
assert(isInvariant(R_0^2, T))
///

-- Test 5
TEST ///
R = QQ[x_1..x_3]
T = diagonalAction(matrix{{1,0,1},{0,1,1}}, {3,3}, R)
invariants1 = set {x_3^3, x_2^3, x_1^3, x_1*x_2*x_3^2, x_1^2*x_2^2*x_3}
assert(set invariants T === invariants1)
///

-- Test 6
TEST ///
R = QQ[x_1]
T = diagonalAction(matrix{{0}}, R)
invariants0 = set {x_1}
assert(first weights T === matrix{{0}})
assert(set invariants T === invariants0)
///

-- Test 7
TEST ///
R0 = QQ[x_1..x_2]
T0 = diagonalAction(matrix{{1,1}}, R0)
invariants0 = set {}
assert(first weights T0 === matrix{{1,1}})
assert(set invariants T0 === invariants0)
///

-- Test 8
TEST ///
R1 = QQ[x_1..x_4]
T1 = diagonalAction(matrix {{-3, -1, 1, 2}}, R1)
invariants1 =  set {x_2*x_3, x_2^2*x_4, x_1*x_3*x_4, x_1*x_2*x_4^2, x_1^2*x_4^3, x_1*x_3^3}
assert(first weights T1 === matrix{{-3, -1, 1, 2}})
assert(set invariants T1 === invariants1)
///

-- Test 9
TEST ///
R2 = QQ[x_1..x_4]
T2 = diagonalAction(matrix{{0,1,-1,1},{1,0,-1,-1}}, R2)
invariants2 = set {x_1*x_2*x_3,x_1^2*x_3*x_4}
assert(set invariants T2 === invariants2)
///
     
     
-------------------------------------------
--- LinearlyReductiveAction TESTS ---------
-------------------------------------------

-- Test 10
TEST ///
A = QQ[z]
I = ideal(z^2 - 1)
M = matrix{{(z+1)/2, (1-z)/2},{(1-z)/2, (1+z)/2}}
R = QQ[x_1,x_2]
V = linearlyReductiveAction(I,M,R)
assert(hilbertIdeal V == ideal(x_1 + x_2, x_2^2))
///

-- Test 11
TEST ///
R = QQ[a,b,c,d]
idealSL2 = ideal(a*d - b*c - 1)
SL2std = matrix{{a,b},{c,d}}
R1 = QQ[x_1..x_2]
V1 = linearlyReductiveAction(idealSL2,SL2std,R1)
assert(hilbertIdeal V1 == 0)
assert(set invariants V1 === set {})
SL2Sym2 = matrix{{a^2, a*b, b^2}, {2*a*c, b*c + a*d, 2*b*d}, {c^2, c*d, d^2}}
R2 = QQ[x_1..x_3]
V2 = linearlyReductiveAction(idealSL2,SL2Sym2,R2)
assert(set invariants V2 === set {x_2^2-x_1*x_3})
///

-- Test 12
-- This tests invariants for an action on a quotient ring
TEST ///
S = QQ[z]
I = ideal(z^2 - 1)
M = matrix{{(z+1)/2, (1-z)/2},{(1-z)/2, (z+1)/2}}
Q = QQ[x,y] / ideal(x*y)
L = linearlyReductiveAction(I, M, Q)
assert(invariants L === {x+y})
assert(hilbertIdeal L === ideal(x+y))
assert(invariants(L,2) === {x^2+y^2})
assert(isInvariant(x^3+y^3,L))
///

-------------------------------------------
--- equivariantHilbertSeries TESTS --------
-------------------------------------------

-- Test 13
-- mixed torus + abelian action
TEST ///
R = QQ[x_1..x_3]
W1 = matrix{{1,0,-1},{0,1,-1}}
W2 = matrix{{0,1,1},{1,0,1}}
d = {3,3}
D = diagonalAction(W1,W2,d,R)
degRing = degreesRing D
e = equivariantHilbertSeries D
assert(value denominator e === 
    1+(-z_0*z_3-z_1*z_2-z_0^(-1)*z_1^(-1)*z_2*z_3)*T+(z_0*z_1*z_
      2*z_3+z_1^(-1)*z_2*z_3^2+z_0^(-1)*z_2^2*z_3)*T^2-z_2^2*z_3^2
      *T^3)
assert(equivariantHilbertSeries(D,Order=>6) ===
    1+(z_0*z_3+z_1*z_2+z_0^(-1)*z_1^(-1)*z_2*z_3)*T+(z_0^2*z_3^2
      +z_0*z_1*z_2*z_3+z_1^2*z_2^2+z_1^(-1)*z_2*z_3^2+z_0^(-1)*z_2
      ^2*z_3+z_0^(-2)*z_1^(-2)*z_2^2*z_3^2)*T^2+(z_0^3+z_0^2*z_1*z
      _2*z_3^2+z_0*z_1^2*z_2^2*z_3+z_0*z_1^(-1)*z_2+z_1^3+z_2^2*z_
      3^2+z_0^(-1)*z_1*z_3+z_0^(-1)*z_1^(-2)*z_2^2+z_0^(-2)*z_1^(-
      1)*z_3^2+z_0^(-3)*z_1^(-3))*T^3+(z_0^4*z_3+z_0^3*z_1*z_2+z_0
      ^2*z_1^2*z_2^2*z_3^2+z_0^2*z_1^(-1)*z_2*z_3+z_0*z_1^3*z_3+z_
      0*z_2^2+z_1^4*z_2+z_1*z_3^2+z_1^(-2)*z_2^2*z_3+z_0^(-1)*z_1^
      2*z_2*z_3+z_0^(-1)*z_1^(-1)+z_0^(-2)*z_2*z_3^2+z_0^(-2)*z_1
      ^(-3)*z_3+z_0^(-3)*z_1^(-2)*z_2+z_0^(-4)*z_1^(-4)*z_2*z_3)*T
      ^4+(z_0^5*z_3^2+z_0^4*z_1*z_2*z_3+z_0^3*z_1^2*z_2^2+z_0^3*z_
      1^(-1)*z_2*z_3^2+z_0^2*z_1^3*z_3^2+z_0^2*z_2^2*z_3+z_0*z_1^4
      *z_2*z_3+z_0*z_1+z_0*z_1^(-2)*z_2^2*z_3^2+z_1^5*z_2^2+z_1^2*
      z_2*z_3^2+z_1^(-1)*z_3+z_0^(-1)*z_1^3*z_2^2*z_3+z_0^(-1)*z_2
      +z_0^(-1)*z_1^(-3)*z_3^2+z_0^(-2)*z_1*z_2^2*z_3^2+z_0^(-2)*z
      _1^(-2)*z_2*z_3+z_0^(-3)*z_1^(-1)*z_2^2+z_0^(-3)*z_1^(-4)*z_
      2*z_3^2+z_0^(-4)*z_1^(-3)*z_2^2*z_3+z_0^(-5)*z_1^(-5)*z_2^2*
      z_3^2)*T^5)
///

-- Test 14
-- torus action
TEST ///
R = QQ[x_1..x_4]
W = matrix{{0,1,-1,1},{1,0,-1,-1}}
D = diagonalAction(W, R)
degRing = degreesRing D
e = equivariantHilbertSeries D
assert(value denominator e ===
    1+(-z_0-z_0*z_1^(-1)-z_1-z_0^(-1)*z_1^(-1))*T+(z_0^2*z_1^(-1
      )+z_0*z_1+z_0+z_1^(-1)+z_1^(-2)+z_0^(-1))*T^2+(-z_0^2-z_0*z_
      1^(-2)-1-z_1^(-1))*T^3+z_0*z_1^(-1)*T^4)
assert(equivariantHilbertSeries(D,Order=>6) ===
    1+(z_0+z_0*z_1^(-1)+z_1+z_0^(-1)*z_1^(-1))*T+(z_0^2+z_0^2*z_
      1^(-1)+z_0^2*z_1^(-2)+z_0*z_1+z_0+z_1^2+z_1^(-1)+z_1^(-2)+z_
      0^(-1)+z_0^(-2)*z_1^(-2))*T^2+(z_0^3+z_0^3*z_1^(-1)+z_0^3*z_
      1^(-2)+z_0^3*z_1^(-3)+z_0^2*z_1+z_0^2+z_0^2*z_1^(-1)+z_0*z_1
      ^2+z_0*z_1+z_0*z_1^(-1)+z_0*z_1^(-2)+z_0*z_1^(-3)+z_1^3+1+z_
      1^(-1)+z_0^(-1)*z_1+z_0^(-1)*z_1^(-2)+z_0^(-1)*z_1^(-3)+z_0
      ^(-2)*z_1^(-1)+z_0^(-3)*z_1^(-3))*T^3+(z_0^4+z_0^4*z_1^(-1)+
      z_0^4*z_1^(-2)+z_0^4*z_1^(-3)+z_0^4*z_1^(-4)+z_0^3*z_1+z_0^3
      +z_0^3*z_1^(-1)+z_0^3*z_1^(-2)+z_0^2*z_1^2+z_0^2*z_1+z_0^2+z
      _0^2*z_1^(-1)+z_0^2*z_1^(-2)+z_0^2*z_1^(-3)+z_0^2*z_1^(-4)+z
      _0*z_1^3+z_0*z_1^2+z_0+z_0*z_1^(-1)+z_0*z_1^(-2)+z_1^4+z_1+1
      +z_1^(-2)+z_1^(-3)+z_1^(-4)+z_0^(-1)*z_1^2+z_0^(-1)*z_1^(-1
      )+z_0^(-1)*z_1^(-2)+z_0^(-2)+z_0^(-2)*z_1^(-3)+z_0^(-2)*z_1
      ^(-4)+z_0^(-3)*z_1^(-2)+z_0^(-4)*z_1^(-4))*T^4+(z_0^5+z_0^5*
      z_1^(-1)+z_0^5*z_1^(-2)+z_0^5*z_1^(-3)+z_0^5*z_1^(-4)+z_0^5*
      z_1^(-5)+z_0^4*z_1+z_0^4+z_0^4*z_1^(-1)+z_0^4*z_1^(-2)+z_0^4
      *z_1^(-3)+z_0^3*z_1^2+z_0^3*z_1+z_0^3+2*z_0^3*z_1^(-1)+z_0^3
      *z_1^(-2)+z_0^3*z_1^(-3)+z_0^3*z_1^(-4)+z_0^3*z_1^(-5)+z_0^2
      *z_1^3+z_0^2*z_1^2+z_0^2*z_1+z_0^2+z_0^2*z_1^(-1)+z_0^2*z_1
      ^(-2)+z_0^2*z_1^(-3)+z_0*z_1^4+z_0*z_1^3+z_0*z_1+z_0+z_0*z_1
      ^(-1)+z_0*z_1^(-2)+z_0*z_1^(-3)+z_0*z_1^(-4)+z_0*z_1^(-5)+z_
      1^5+z_1^2+z_1+z_1^(-1)+z_1^(-2)+z_1^(-3)+z_0^(-1)*z_1^3+z_0
      ^(-1)+z_0^(-1)*z_1^(-1)+z_0^(-1)*z_1^(-3)+z_0^(-1)*z_1^(-4)+
      z_0^(-1)*z_1^(-5)+z_0^(-2)*z_1+z_0^(-2)*z_1^(-2)+z_0^(-2)*z_
      1^(-3)+z_0^(-3)*z_1^(-1)+z_0^(-3)*z_1^(-4)+z_0^(-3)*z_1^(-5
      )+z_0^(-4)*z_1^(-3)+z_0^(-5)*z_1^(-5))*T^5)
///

-- Test 15
-- abelian action
TEST ///
R = QQ[x_1..x_3]
d = {3,3}
W = matrix{{1,0,1},{0,1,1}}
D = diagonalAction(W, d, R)
degRing = degreesRing D
e = equivariantHilbertSeries D
assert(value denominator e ===
    1+(-z_0*z_1-z_0-z_1)*T+(z_0^2*z_1+z_0*z_1^2+z_0*z_1)*T^2-z_0
      ^2*z_1^2*T^3)
assert(equivariantHilbertSeries(D,Order=>6) ===
    1+(z_0*z_1+z_0+z_1)*T+(z_0^2*z_1^2+z_0^2*z_1+z_0^2+z_0*z_1^2
      +z_0*z_1+z_1^2)*T^2+(z_0^2*z_1^2+z_0^2*z_1+z_0^2+z_0*z_1^2+z
      _0+z_1^2+z_1+3)*T^3+(z_0^2*z_1^2+z_0^2*z_1+z_0^2+z_0*z_1^2+3
      *z_0*z_1+3*z_0+z_1^2+3*z_1+1)*T^4+(3*z_0^2*z_1^2+3*z_0^2*z_1
      +3*z_0^2+3*z_0*z_1^2+3*z_0*z_1+z_0+3*z_1^2+z_1+1)*T^5)
///


------------------------------------------------------------------------
--- Tests imported from InvariantRing 1.0 with the new syntax ---------
------------------------------------------------------------------------

-- Test 16
-- Test for reynoldsOperator
TEST ///
R=QQ[x_0..x_2]
A=matrix{{0,1,0},{-1,0,0},{0,0,-1}}
C4=finiteAction(A,R)
assert(reynoldsOperator(x_0^2+x_1+x_2,C4)===(1/2)*(x_0^2+x_1^2))
///

-- Test 17 
-- FiniteGroupAction Test for S5 and A4
TEST ///
A=transpose matrix{{0,1,0,0},{0,0,1,0},{0,0,0,1},{-1,-1,-1,-1}}
B=matrix{{-1,1,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}
R=QQ[x_1..x_4]
S5=finiteAction({A,B},R)
assert(#(group S5) === 120)
assert(not isAbelian S5)
C=permutationMatrix toString 3124
D=permutationMatrix toString 2143
A4=finiteAction({C,D},R)
assert(#(group A4) === 12)
assert(not isAbelian A4)
///



-- Test 18
-- Checks primaryInvariants and secondaryInvariant for correct
-- output for the case where ground field is a number field.
TEST ///
K=toField(QQ[i]/(i^2+1))
R=K[x,y]
D4=finiteAction({matrix{{i,0},{0,-i}},matrix{{0,1},{1,0}}},R)
assert(#(group D4) === 8)
assert(not isAbelian D4)
P=primaryInvariants D4
assert(P === {x*y,(1/2)*x^4+(1/2)*y^4})
assert(secondaryInvariants(P,D4) === {sub(1,R)})
assert(hironakaDecomposition D4 === ({x*y,(1/2)*x^4+(1/2)*y^4},{sub(1,R)}))
///

-- D4 is the dihedral group of order 8
-- Note i is the square root of -1
-- If A=matrix{{i,0},{0,-i}}, B=matrix{{0,1},{1,0}}, one can check that 
-- A^4=B^2=(A*B)^2=1, which are the defining relations for the dihedral group of
-- order 8.

-- Test 19 
-- Checks invariantRing for correct output for the case where ground field is a 
-- number field.
TEST ///
K=toField(QQ[i]/(i^2+1))
R=K[x,y]
D4=finiteAction({matrix{{i,0},{0,-i}},matrix{{0,1},{1,0}}},R)
assert(invariants D4 === {x*y,x^4+y^4})
///

-- Test 20
-- Checks primaryInvariants for the symmetric group S3 - these should be
-- the elementary symmetric polynomials in 3 variables

TEST ///
R = QQ[x,y,z]
r=permutationMatrix toString 312
s=permutationMatrix toString 213
S3 = finiteAction({r,s},R)
assert(isInvariant(x*y*z,S3))
assert(isInvariant(x+y+z,S3))
assert(isInvariant(x*y+x*z+y*z,S3))
P=primaryInvariants(S3)
assert(dim(R/ideal(P))==0)
assert(P==apply(P,f->reynoldsOperator(f,S3)))
H=hilbertSeries(R/ideal(P))
use ring value numerator H
assert(value numerator H === 1-T-T^2+T^4+T^5-T^6)
assert(value denominator H === sub((1-T)^3, ring value denominator H))
///


-- Test 21
-- Checks that the Dade algorithm works for large enough finite fields 
-- *NB it is possible that primaryInvariants(S3,Dade=>true) can run correctly 
-- and output an invariant polynomial of degree strictly less than the 
-- cardinality of the group. If a check on the package invariant ring reports 
-- failure of the folloing test, then one should see if the test is passed upon 
-- a second attempt. Only if the test fails a second time is it worth inspecting 
-- the code for errors.  

TEST ///
K=GF(101)
R=K[x,y,z]
r=permutationMatrix toString 312
s=permutationMatrix toString 213
S3 = finiteAction({r,s},R)
setRandomSeed 0
P=primaryInvariants(S3, Dade=>true)
P/degree			 -- under Ubuntu 32, this gives {{3}, {6}, {6}}
setRandomSeed 0
P=primaryInvariants(S3,Dade=>true);
P/degree			 -- under Ubuntu 32, this gives {{6}, {6}, {6}}
assert(
     P==apply(P,f->reynoldsOperator(f,S3))
     )
assert(
     dim(R/ideal(P))==0
     )
assert(
     apply(P,degree)==toList(#P:{#(group S3)})
     )
///




-- Test 22
-- Checks molienSeries and secondaryInvariants on a known example where the
-- ground field is a number field 
TEST ///
K=toField(QQ[a]/(a^2+a+1))
A=matrix{{a,0},{0,a^2}}
B=sub(matrix{{0,1},{1,0}},K)
R=K[x,y]
D6=finiteAction({A,B},R)
mol=molienSeries D6
use ring value denominator mol;
assert(
     (value denominator mol)==(T^5-T^3-T^2+1)
     )
assert(
     (value numerator mol)==1
     )
assert(
     secondaryInvariants({x^3+y^3,-(x^3-y^3)^2},D6)=={1,x*y,x^2*y^2}
     )	
 ///
 

 
 -- Test 23
 -- Checks the dadeHSOP routine by checking that the list of polynomials output
-- has the expected output. Namely:
-- they are invariant polynimials,
-- they form a homogeneous system of parameters for the polynomial ring
-- they have degrees equal to the cardinality of the group (which should occur
-- with probability 1)*
-- *NB it is possible that dadeHSOP can run correctly and output an invariant
-- polynomial of degree strictly less than the cardinality of the group. If a
-- check on the package invariant ring reports failure of the following test,
-- then one should see if the test is passed upon a second attempt. Only if the
-- test fails a second time is it worth inspecting the code for errors.   	    	 
TEST ///
setRandomSeed 0
S=QQ[x,y]
r=matrix{{0,-1},{1,0}}
s=matrix{{0,1},{1,0}}
D4=finiteAction({r,s},S)
P=primaryInvariants(D4,Dade=>true)
assert(
     P==apply(P,f->reynoldsOperator(f,D4))
     )
assert(
     dim(S/ideal(P))==0
     )
assert(
     apply(P,degree)==toList(#P:{#(group D4)})
     )
///
