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

--tests highestWeightsDecomposition for quotient rings
TEST ///
    I = Grassmannian(1,4,CoefficientRing=>QQ)
    R = ring I
    L = {{1,0,0,0},{-1,1,0,0},{0,-1,1,0},{0,0,-1,1},{0,0,0,-1}}
    W = apply(subsets({0,1,2,3,4},2), s -> L_(s_0)+L_(s_1))
    setWeights(R, dynkinType{{"A",4}}, W)
    Q = R/I
    --degree d of the Gr(2,5) coordinate ring is the irreducible of h.w. {0,d,0,0}
    dList = highestWeightsDecomposition(Q,{2})
    dZZ = highestWeightsDecomposition(Q,2)
    assert(dList === new Tally from {{0,2,0,0} => 1})
    --the ZZ dispatch equals the List dispatch with a singleton list
    assert(dZZ === dList)
    --the range dispatch agrees with the single-degree dispatch at each degree
    H = highestWeightsDecomposition(Q,1,3)
    assert all(toList(1..3), d -> H#d === highestWeightsDecomposition(Q,d))
    assert instance(dZZ, Tally)
    assert instance(H, HashTable)
///

--tests highestWeightsDecomposition for graded modules
TEST ///
    I = Grassmannian(1,4,CoefficientRing=>QQ)
    R = ring I
    L = {{1,0,0,0},{-1,1,0,0},{0,-1,1,0},{0,0,-1,1},{0,0,0,-1}}
    W = apply(subsets({0,1,2,3,4},2), s -> L_(s_0)+L_(s_1))
    setWeights(R, dynkinType{{"A",4}}, W)
    M = R^1/I
    w = {{0,0,0,0}}
    --M = R^1/I is the Gr(2,5) coordinate ring as an R-module: degree d is
    --the irreducible of highest weight {0,d,0,0}
    dList = highestWeightsDecomposition(M,{2},w)
    dZZ = highestWeightsDecomposition(M,2,w)
    assert(dList === new Tally from {{0,2,0,0} => 1})
    --the (Module,ZZ,List) dispatch equals (Module,List,List) with a singleton
    assert(dZZ === dList)
    --the range dispatch agrees with the single-degree dispatch at each degree
    H = highestWeightsDecomposition(M,1,3,w)
    assert all(toList(1..3), d -> H#d === highestWeightsDecomposition(M,{d},w))
    assert instance(dZZ, Tally)
    assert instance(H, HashTable)
    --error tests: a wrong number of weights, and an inverted degree range
    assert(try (highestWeightsDecomposition(M,{2},{}); false) else true)
    assert(try (highestWeightsDecomposition(M,3,2,w); false) else true)
///

--tests highestWeightsDecomposition for equivariant ideals
TEST ///
    I = Grassmannian(1,4,CoefficientRing=>QQ)
    R = ring I
    L = {{1,0,0,0},{-1,1,0,0},{0,-1,1,0},{0,0,-1,1},{0,0,0,-1}}
    W = apply(subsets({0,1,2,3,4},2), s -> L_(s_0)+L_(s_1))
    setWeights(R, dynkinType{{"A",4}}, W)
    --the Gr(2,5) ideal is generated by quadrics forming the irreducible
    --representation of highest weight {0,0,0,1}
    dList = highestWeightsDecomposition(I,{2})
    dZZ = highestWeightsDecomposition(I,2)
    assert(dList === new Tally from {{0,0,0,1} => 1})
    --the ZZ dispatch equals the List dispatch with a singleton list
    assert(dZZ === dList)
    --the range dispatch agrees with the single-degree dispatch at each degree
    H = highestWeightsDecomposition(I,1,3)
    assert all(toList(1..3), d -> H#d === highestWeightsDecomposition(I,d))
    --the ideal has no linear forms, so degree 1 is the empty decomposition
    assert(H#1 === new Tally from {})
    assert instance(dZZ, Tally)
    assert instance(H, HashTable)
    --error test: an inverted degree range
    assert(try (highestWeightsDecomposition(I,3,2); false) else true)
///

--tests the Forward option of propagateWeights
TEST ///
    T = QQ[z_1..z_3]
    setWeights(T, dynkinType{{"A",2}}, {{1,0},{-1,1},{0,-1}})
    phi = vars T
    --backward propagation recovers the weights assigned to the variables
    back = propagateWeights(phi, {{0,0}})
    assert instance(back, Sequence)
    srcW = first back
    assert(srcW === {{1,0},{-1,1},{0,-1}})
    --Forward => true propagates the opposite way and inverts the backward result
    fwd = propagateWeights(phi, srcW, Forward => true)
    assert(first fwd === {{0,0}})
///
