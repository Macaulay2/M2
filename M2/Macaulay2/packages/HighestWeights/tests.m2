--------------------------------------------------------------------------------
-- Copyright 2014  Federico Galetto
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.
--
-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.
--------------------------------------------------------------------------------

--tests decomposition of characters
TEST ///
    D=dynkinType{{"B",3}}
    W={{-2,0,2},{-2,1,0},{-2,2,-2},{-1,-1,2},{-1,0,0},{-1,0,0},{-1,0,0},{-1,0,0},{-1,0,0},{-1,0,1},{-1,0,1},{-1,0,2},{-1,1,-2},{-1,1,-1},{-1,1,-1},{-1,1,0},{-1,1,0},{-1,1,0},{-1,1,0},{-1,1,0},{-1,2,-2},{0,-2,2},{0,-1,0},{0,-1,1},{0,-1,1},{0,-1,2},{0,-1,2},{0,-1,2},{0,-1,2},{0,-1,2},{0,0,-2},{0,0,-1},{0,0,-1},{0,0,0},{0,0,0},{0,0,0},{0,0,0},{0,0,0},{0,0,0},{0,0,1},{0,0,1},{0,0,2},{0,1,-2},{0,1,-2},{0,1,-2},{0,1,-2},{0,1,-2},{0,1,-1},{0,1,-1},{0,1,0},{0,2,-2},{1,-2,2},{1,-1,0},{1,-1,0},{1,-1,0},{1,-1,0},{1,-1,0},{1,-1,1},{1,-1,1},{1,-1,2},{1,0,-2},{1,0,-1},{1,0,-1},{1,0,0},{1,0,0},{1,0,0},{1,0,0},{1,0,0},{1,1,-2},{2,-2,2},{2,-1,0},{2,0,-2}}
    T=new Tally from {{0,0,1}=>2,{0,0,2}=>1,{1,0,0}=>3}
    assert(T === decomposeWeightsList(D,W))
///

--tests weight propagation
TEST ///
    R=QQ[x_(1,1)..x_(3,2)]
    G=genericMatrix(R,2,3)
    W={{1,1,0},{-1,1,0},{1,-1,1},{-1,-1,1},{1,0,-1},{-1,0,-1}}
    D=dynkinType{{"A",1},{"A",2}}
    setWeights(R,D,W)
    C=promote(random(QQ^3,QQ^3,MaximalRank=>true),R)
    M=G*C
    (U,B)=propagateWeights(M,{{1,0,0},{-1,0,0}})
    assert (sort U === {{2,-1,1},{2,0,-1},{2,1,0}})
///

--tests highest weight decomposition of a resolution and polynomial ring
TEST ///
    R=QQ[x_1..x_8]
    W={{-1, 0, 0, 0},{-1, 1, 0, 0},{0,-1, 1, 1},{0, 0,-1, 1},{0, 0, 1,-1},{0, 1,-1,-1},{1,-1, 0, 0},{1, 0, 0, 0}}
    D=dynkinType{{"D",4}}
    setWeights(R,D,W)
    K=koszulComplex vars R
    H0=highestWeightsDecomposition(K)
    T0=new HashTable from {0 => new HashTable from {{0} => new Tally from {{0, 0, 0, 0} => 1}}, 1 => new HashTable from {{1} => new Tally from {{1, 0, 0, 0} => 1}}, 2 => new HashTable from {{2} => new Tally from {{0, 1, 0, 0} => 1}}, 3 => new HashTable from {{3} => new Tally from {{0, 0, 1, 1} => 1}}, 4 => new HashTable from {{4} => new Tally from {{0, 0, 0, 2} => 1, {0, 0, 2, 0} => 1}}, 5 => new HashTable from {{5} => new Tally from {{0, 0, 1, 1} => 1}}, 6 => new HashTable from {{6} => new Tally from {{0, 1, 0, 0} => 1}}, 7 => new HashTable from {{7} => new Tally from {{1, 0, 0, 0} => 1}}, 8 => new HashTable from {{8} => new Tally from {{0, 0, 0, 0} => 1}}}
    assert(H0 === T0)
    H1=highestWeightsDecomposition(R,3,4)
    T1=new HashTable from {4=>new Tally from {{0, 0, 0, 0} => 1, {2, 0, 0, 0} => 1, {4, 0, 0, 0} => 1}, 3 => new Tally from {{1, 0, 0, 0} => 1, {3, 0, 0, 0} => 1}}
    assert(H1 === T1)
///

--tests decomposition of graded components
TEST ///
    I=Grassmannian(1,4,CoefficientRing=>QQ)
    R=ring I
    Q=R/I
    L={{1,0,0,0},{-1,1,0,0},{0,-1,1,0},{0,0,-1,1},{0,0,0,-1}}
    W=apply(subsets({0,1,2,3,4},2),s->L_(s_0)+L_(s_1))
    D=dynkinType{{"A",4}}
    setWeights(R,D,W)
    H=highestWeightsDecomposition(Q,1,3)
    T=new HashTable from {1 => new Tally from {{0, 1, 0, 0} => 1}, 2 => new Tally from {{0, 2, 0, 0} => 1}, 3 => new Tally from {{0, 3, 0, 0} => 1}}
    assert(H === T)
///
